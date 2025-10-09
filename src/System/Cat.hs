{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module System.Cat where

import Command
import Command.Aokana
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.System
import Data.Coerce
import Data.Default
import Data.Maybe
import Data.PersistModel
import External.ChatAPI
import MeowBot
import MeowBot.CommandRule
import MeowBot.Data.Book
import Module.AsyncInstance
import Module.CommandInstance
import Module.ConnectionManager
import Module.CronTabTickInstance
import Module.Database
import Module.LogDatabase
import Module.Logging
import Module.MeowConnection
import Module.MeowTypes
import Module.Prometheus
import Module.ProxyWS
import Module.RS
import Module.RecvSentCQ
import Module.StatusMonitor
import Network.WebSockets
import System.Directory
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (system)
import System.WatchDog
import Text.Read (readMaybe)
import Utils.Logging
import Utils.Persist

botLoop :: Meow never_returns
botLoop = do
  $logDebug "Before system actions"
  beforeSystem

  $logDebug "Listening to events"
  listenToEvents >>= liftIO . atomically >>= handleEvents

  $logDebug "Performing actions"
  performMeowActions

  $logDebug "After system actions"
  afterSystem

  $logDebug "Finished one loop"
  botLoop

performMeowActions :: Meow ()
performMeowActions = do
  actions <- asksModule meowReadsAction
  as <- liftIO . atomically $ readTVar actions <* writeTVar actions []
  mapM_ doBotAction . concat =<< sequence as

-- the process of initialization:
--
-- in the outer layer, init all global states
--
-- in each inner layer where we run each bot, init all local states
--
-- then initialize AllData, and run the botLoop

-- allInitDataG :: AllModuleInitDataG Mods
-- allInitDataG  = CronTabTickInitDataG :** StatusMonitorInitDataG
--               :** AsyncInitDataG :** CommandInitDataG
--               :** LogDatabaseInitDataG "meowbot.db"
--               :** ProxyWSInitDataG :** ConnectionManagerInitDataG :** FNil

-- allInitDataL :: TVar MeowStatus -> [ProxyFlag] -> AllModuleInitDataL Mods
-- allInitDataL tvar pf = CronTabTickInitDataL :** StatusMonitorInitDataL tvar
--                 :** AsyncInitDataL :** CommandInitDataL :** LogDatabaseInitDataL
--                 :** ProxyWSInitDataL [(add, ip) | ProxyFlag add ip <- pf]
--                 :** ConnectionManagerInitDataL :** FNil

pingpongOptions = defaultPingPongOptions
  -- { pingInterval = 30
  -- , pongTimeout  = 30
  -- }

-- runBots :: AllModuleInitDataG Mods -> [BotInstance] -> EffT '[LoggingModule] NoError IO ()
-- runBots initglobs bots = do
--   $(logInfo) "Initializing all global states"
--   global <- initAllModulesG @R initglobs
--   $(logInfo) "Starting bot instances"
--   mapM_ (runBot initglobs global) bots

-- | All these are read-only so no problem to reuse them when restarting the bot
-- runBot :: AllModuleInitDataG Mods -> AllModuleGlobalStates Mods -> BotInstance -> EffT '[LoggingModule] NoError IO ()
-- runBot initglobs glob bot = do
--   case botRunFlag bot of
--     RunClient ip port -> do
--       tvarMeowStat  <- liftIO $ initTVarMeowStatus
--       earlyLocal <- initAllModulesEL @R allInitDataG (allInitDataL tvarMeowStat $ botProxyFlags bot)
--       runBotClient ip port bot initglobs glob earlyLocal
--     RunServer ip port -> do
--       tvarMeowStat  <- liftIO $ initTVarMeowStatus
--       earlyLocal <- initAllModulesEL @R allInitDataG (allInitDataL tvarMeowStat $ botProxyFlags bot)
--       runBotServer ip port bot initglobs glob earlyLocal

runBot :: BotInstance -> Meow a -> EffT '[MeowDatabase, Prometheus, LoggingModule] '[ErrorText "meowdb"] IO ()
runBot bot meow = do
  botm     <- embedEffT $ botInstanceToModule bot
  meowStat <- liftIO $ initTVarMeowStatus
  let botconfig = BotConfig botm (botDebugFlags bot) Nothing
      watchDog  = listToMaybe $ botWatchDogFlags bot

      checkHandle = fmap (== MeowOnline) . readTVarIO
      mWatchDogInit = (\(WatchDogFlag interval action) -> WatchDogInitData meowStat interval checkHandle (void $ system action)) <$> watchDog

  AllData wc bc od <- embedEffT $ initAllData botconfig
  embedNoError
    $ effAddLogCat' (LogCat botm.botId)
    $ runSModule_ od
    $ runSModule_ wc
    $ runSModule_ bc
    $ ( case botRunFlag bot of
          RunClient addr port -> void . withClientConnection addr port
          RunServer addr port ->        withServerConnection addr port
      )
    $ withMeowActionQueue
    $ withRecvSentCQ
    $ withModule CommandModuleInitData
    $ withModule (StatusMonitorModuleInitData meowStat)
    $ maybe id (\init -> withWatchDog init . const) mWatchDogInit
    $ withCronTabTick
    $ withProxyWS (ProxyWSInitData [(add, ip) | ProxyFlag add ip <- bot.botProxyFlags])
    $ withConnectionManager
    $ withAsyncModule
    $ withLogDatabase
    $ meow

runBots :: [BotInstance] -> EffT '[MeowDatabase, Prometheus, LoggingModule] '[] IO ()
runBots bots = mapM_ (\bot -> forkEffT $ runBot bot botLoop) bots >> liftIO (threadDelay maxBound)

withServerConnection
  :: (ConsFDataList FData (MeowConnection : mods), LoggingModule `In` mods, m ~ IO, Show (EList es))
  => String -> Int -> EffT (MeowConnection : mods) es m a -> EffT mods NoError m ()
withServerConnection addr port act = do
  $(logInfo) $ "Running bot server, listening on " <> tshow addr <> ":" <> tshow port
  effTryWith (\(e :: SomeException) -> errorText @"uncaught" (toText e)) (liftWith $ \run -> runServer addr port $ \pending -> do
    conn <- acceptRequest pending

    withPingPong pingpongOptions conn $ \conn -> void $ run $ do
      $logInfo "Connected to client"

      runMeowConnection (MeowConnectionRead conn) (void act) `effCatchAll`
        (\e -> $logError $ "ERROR In connection with client : " <> tshow e)

    void . run $ $logInfo $ "Disconnected from client") `effCatch` \(e :: ErrorText "uncaught") -> do
      $logError $ "Uncaught Error In server: " <> toText e
      $logInfo "Restarting server in 20 seconds"
      liftIO $ threadDelay 20_000_000
      withServerConnection addr port act

withClientConnection
  :: ( ConsFDataList FData (MeowConnection : mods)
     , LoggingModule `In` mods, m ~ IO, Show (EList es)
     )
  => String -> Int -> EffT (MeowConnection : mods) es m a -> EffT mods NoError m ()
withClientConnection addr port act = do
  $(logInfo) $ "Running bot client, connecting to " <> tshow addr <> ":" <> tshow port
  effTryWith (\(e :: SomeException) -> errorText @"uncaught" (toText e)) (liftWith $ \run -> runClient addr port "" $ \conn -> void . run $ do
    $logInfo $ "Connected to server " <> tshow addr <> ":" <> tshow port

    runMeowConnection (MeowConnectionRead conn) (void act) `effCatchAll`
      (\e -> $logError $ "ERROR In connection with server : " <> tshow e)

    ) `effCatch` \(e :: ErrorText "uncaught") -> do
      $logError $ "Uncaught Error In client: " <> toText e
      $logInfo "Restarting client in 20 seconds"
      liftIO $ threadDelay 20_000_000
      withClientConnection addr port act

-- runBotServer ip port bot initglobs glob el = do
--   $(logInfo) $ "Running bot server, listening on " <> tshow ip <> ":" <> tshow port
--   botm     <- botInstanceToModule bot
--   let botconfig = BotConfig botm (botDebugFlags bot) Nothing
--       watchDog = listToMaybe $ botWatchDogFlags bot
--   alldata       <- initAllData botconfig glob
--   connectedTVar <- liftIO $ newTVarIO False
--   let tvarMeowStat = elUsedByWatchDog $ getF @StatusMonitorModule el
--   let checkHandle tvarExtra =
--         liftIO $ atomically $ do
--           meowStat <- readTVar tvarMeowStat
--           extra    <- readTVar tvarExtra
--           return $ foldl' (&&) True [meowStat == MeowOnline, extra]
--   mWatchDogId   <- case watchDog of
--     Just (WatchDogFlag interval action) -> do
--       $(logInfo) $ "Starting watch dog with interval " <> tshow interval
--       wd <- liftIO $ initWatchDog connectedTVar interval checkHandle (void $ system action)
--       liftIO $ Just <$> startWatchDog wd
--     Nothing -> return Nothing
--   void $
--     logThroughCont (runServer ip port) (\pendingconn -> do
--       conn <- lift $ acceptRequest pendingconn
--       liftIO . atomically $ writeTVar connectedTVar True
--       logThroughCont (withPingPong pingpongOptions conn) (\conn -> do
--         $(logInfo) $ "Connected to client"
--         meowData <- liftIO $ initMeowData conn
--         $(logDebug) $ "initMeowData finished"
--         local    <- initAllModulesL meowData initglobs (allInitDataL tvarMeowStat $ botProxyFlags bot) el
--         $(logDebug) $ "initAllModulesL finished, entering bot loop"
--         void (runReaderStateT (runCatT botLoop) (glob, meowData) (local, alldata))) `logCatch`
--           (\e -> do
--             $(logError) $ "ERROR In connection with client : " <> tshow e
--           )
--       $(logInfo) $ "Disconnected from client"
--       liftIO . atomically $ writeTVar connectedTVar False) `logForkFinally`
--         ( \e -> do
--           maybe (pure ()) (liftIO . killThread) mWatchDogId
--           rerunBot initglobs glob el bot e
--         )
-- 
-- runBotClient ip port bot initglobs glob el = do
--   $(logInfo) $ "Running bot client, connecting to " <> tshow ip <> ":" <> tshow port
--   botm     <- botInstanceToModule bot
--   let botconfig = BotConfig botm (botDebugFlags bot) Nothing
--   let tvarMeowStat = elUsedByWatchDog $ getF @StatusMonitorModule el
--   alldata  <- initAllData botconfig glob
--   void $
--     logThroughCont (runClient ip port "") (\conn -> do
--       $(logInfo) $ "Connected to server " <> tshow ip <> ":" <> tshow port
--       meowData <- liftIO $ initMeowData conn
--       local    <- initAllModulesL meowData initglobs (allInitDataL tvarMeowStat $ botProxyFlags bot) el
--       runReaderStateT (runCatT botLoop) (glob, meowData) (local, alldata)
--       ) `logForkFinally` rerunBot initglobs glob el bot

-- rerunBot :: AllModuleInitDataG Mods -> AllModuleGlobalStates Mods -> AllModuleEarlyLocalStates Mods -> BotInstance -> Either SomeException a -> EffT '[LoggingModule] NoError IO ()
-- rerunBot initglobs glob el bot (Left e) = do
--   $(logError) $ "Bot instance failed: " <> tshow bot <> "\nWith Error: " <> tshow e
--   $(logInfo) "Restarting bot instance in 60 seconds"
--   liftIO $ threadDelay 60_000_000
--   case botRunFlag bot of
--     RunClient ip port -> runBotClient ip port bot initglobs glob el
--     RunServer ip port -> runBotServer ip port bot initglobs glob el
-- rerunBot _ _ _ _ (Right _) = do
--   $(logInfo) $ "Bot instance finished successfully."

botInstanceToModule :: BotInstance -> EffT '[LoggingModule] NoError IO BotModules
botInstanceToModule bot@(BotInstance runFlag identityFlags commandFlags mode proxyFlags logFlags watchDogFlags _) = do
    $(logInfo) $ "\n### Starting bot instance: " <> tshow bot
    $(logInfo) $ "Running mode: "   <> tshow mode
    $(logInfo) $ "Running flags: "  <> tshow runFlag
    $(logInfo) $ "Identity flags: " <> tshow identityFlags
    $(logInfo) $ "Command flags: "  <> tshow commandFlags
    $(logInfo) $ "Proxy flags: "    <> tshow proxyFlags
    $(logInfo) $ "Log flags: "      <> tshow logFlags
    $(logInfo) $ "WatchDog flags: " <> tshow watchDogFlags
    -- proxyData <- lift $ sequence $ [ createProxyData addr port | ProxyFlag addr port <- proxyFlags ]
    let mGlobalSysMsg = listToMaybe [ pack sysMsg | UseSysMsg sysMsg <- identityFlags ]
        withDefault def [] = def
        withDefault _ xs = xs
        botModules = BotModules
          { canUseGroupCommands   = withDefault (identifier <$> allGroupCommands)   (coerce commandFlags)
          , canUsePrivateCommands = withDefault (identifier <$> allPrivateCommands) (coerce commandFlags)
          , nameOfBot = BotName $ listToMaybe [ nameBot | UseName nameBot <- identityFlags ]
          , botId = fromMaybe 0 $ listToMaybe [ idBot | UseId idBot <- identityFlags ]
          , globalSysMsg = mGlobalSysMsg
          , proxyTChans = []--proxyData
          , logFile = [ logFile | LogFlag logFile <- logFlags ]
          , botInstance = bot
          }
    return botModules

initAllData :: BotConfig -> EffT '[MeowDatabase, LoggingModule] '[ErrorText "meowdb"] IO AllData
initAllData botconfig = do
  let mods = botModules botconfig
      botid = botId mods
  savedDataDB   <- embedEffT $ errorToEither $ loadSavedDataDB botid
  savedDataFile <- embedEffT $ errorToEither $ baseTransform unsafeInterleaveIO $ loadSavedDataFile (nameOfBot mods)
  case (savedDataDB, savedDataFile) of
    (Right savedDataDB, _) -> do
      $(logInfo) "Loaded saved data from database"
      AllData [] botconfig . OtherData 0 Nothing [] savedDataDB (coerce $ savedAdditional savedDataDB) <$> lift getAllScripts
    (Left errDb, Right savedDataFile) -> do
      $(logError) $ "Failed to load saved data from database: " <> toText errDb
      $(logInfo) "Loaded saved data from file"
      $(logInfo) $ "Trying to migrate data from file to database"
      newSavedDataDB botconfig savedDataFile
      AllData [] botconfig . OtherData 0 Nothing [] savedDataFile (coerce $ savedAdditional savedDataFile) <$> lift getAllScripts
    (Left errDb, Left errFile) -> do
      $(logError) $ "Failed to load saved data from database: " <> toText errDb
      $(logError) $ "Failed to load saved data from file: " <> toText errFile
      $(logInfo) "Starting with empty data, also writing empty data to database"
      newSavedDataDB botconfig (SavedData [] initialUGroups initialGGroups initialRules initialBooks [])
      AllData [] botconfig . OtherData 0 Nothing [] (SavedData [] initialUGroups initialGGroups initialRules initialBooks []) [] <$> lift getAllScripts
  where
    initialUGroups = [(me, Admin)]
    initialGGroups = [(myGroup, AllowedGroup)]
    initialRules =
      [ Allow (UGroup Admin)          (ExceptCommands [Retract])
      , Allow AllUserAndGroups        (CGroup [Cat, Hangman, Help, Md, Random])
      , Allow (GGroup AllowedGroup)   (CGroup [System, Aokana])
      , Allow (UGroup Allowed)        (CGroup [System, Aokana])
      , Allow (SingleGroup myGroup)   (SingleCommand Retract)
      , Deny  (UGroup Denied)         AllCommands
      ]
    initialBooks = []
    me = 754829466 :: UserId
    myGroup = 437447251 :: GroupId

loadSavedDataFile :: BotName -> EffT '[LoggingModule] '[ErrorText "LoadSavedDataFile"] IO SavedData
loadSavedDataFile botName = do
  fileExist <- liftIO $ doesFileExist (savedDataPath botName)
  if fileExist
    then do
      $(logInfo) "Found saved data file, loading data! owo"
      savedData <- liftIO . readFile $ savedDataPath botName
      let maybeSavedData = readMaybe savedData
      case maybeSavedData of
        Just msavedData -> do
          $(logInfo) "Loaded and read saved data file successfully! owo"
          return msavedData
        Nothing -> do
          $(logError) "Failed to load saved data file! o.o Prelude no read!"
          effThrow $ errorText @"LoadSavedDataFile" "Failed to load saved data file! o.o Prelude no read!"
    else do
      $(logInfo) "No saved data file found! o.o"
      effThrow $ errorText @"LoadSavedDataFile" "No saved data file found! o.o"

loadSavedDataDB :: BotId -> EffT '[MeowDatabase, LoggingModule] '[ErrorText "LoadSavedDataDB", ErrorText "meowdb"] IO SavedData
loadSavedDataDB botid = do
  _                    <- fmap  entityVal      . effMaybeInWith (errorText @"LoadSavedDataDB" $ "BotId" <> toText botid <> "Not found")
                            . fmap listToMaybe $ runMeowDB (selectList [BotSettingBotId ==. botid] [LimitTo 1])
  botSettingsPerChat   <- map entityVal <$> runMeowDB (selectList [BotSettingPerChatBotId ==. botid] [])
  inUserGroups         <- map entityVal <$> runMeowDB (selectList [InUserGroupBotId ==. botid] [])
  inGroupGroups        <- map entityVal <$> runMeowDB (selectList [InGroupGroupBotId ==. botid] [])
  commandRulesDB       <- map entityVal <$> runMeowDB (selectList [CommandRuleDBBotId ==. botid] [])
  savedAdditionalDatas <- map entityVal <$> runMeowDB (selectList [SavedAdditionalDataBotId ==. botid] [])
  bookDBs              <- map entityVal <$> runMeowDB (selectList [] [])
  let
      chatIds_chatSettings =
        [( botSettingPerChatChatId c, def
          { systemMessage      = SystemMessage <$> botSettingPerChatSystemMessage c
          , systemTemp         = botSettingPerChatSystemTemp c
          , systemMaxToolDepth = botSettingPerChatSystemMaxToolDepth c
          , systemApiKeys      = botSettingPerChatSystemAPIKey c
              -- case
              -- ( botSettingPerChatSystemAPIKeyOpenAI c
              -- , botSettingPerChatSystemAPIKeyDeepSeek c
              -- , botSettingPerChatSystemAPIKeyOpenRouter c
              -- , botSettingPerChatSystemAPIKeySiliconFlow c
              -- ) of
              --   (Nothing, Nothing, Nothing, Nothing) -> Nothing
              --   (a, b, c, d) -> Just $ APIKey { apiKeyOpenAI = a, apiKeyDeepSeek = b , apiKeyOpenRouter = c, apiKeySiliconFlow = d }
          }) | c <- botSettingsPerChat]
      userIds_userGroups   = [(inUserGroupUserId u, inUserGroupUserGroup u) | u <- inUserGroups]
      groupIds_groupGroups = [(inGroupGroupGroupId g, inGroupGroupGroupGroup g) | g <- inGroupGroups]
      commandRules         = [commandRuleDBCommandRule c | c <- commandRulesDB]
      savedAdditionalData  = map (runPersistUseShow . savedAdditionalDataAdditionalData) savedAdditionalDatas :: [Saved AdditionalData]
      books =
        [ Book
          { book_name    = bookDBBookName b
          , book_pdfPath = bookDBBookPdfPath b
          , book_pages   = bookDBBookPages b
          , book_info    = bookDBBookInfo b
          }
          | b <- bookDBs]
  return $ SavedData
    { chatSettings    = chatIds_chatSettings
    , userGroups      = userIds_userGroups
    , groupGroups     = groupIds_groupGroups
    , commandRules    = commandRules
    , books           = books
    , savedAdditional = savedAdditionalData
    }

newSavedDataDB :: BotConfig -> SavedData -> EffT '[MeowDatabase, LoggingModule] '[ErrorText "meowdb"] IO ()
newSavedDataDB botconfig sd = do
  let botname = nameOfBot (botModules botconfig)
      botid = botId $ botModules botconfig
  runMeowDB (do
    insert_ $ def
      { botSettingBotName                 = maybeBotName botname
      , botSettingBotId                   = botId $ botModules botconfig
      , botSettingDefaultModel            = coerce $ Just (DeepSeek DeepSeekChat)
      , botSettingDefaultModelS           = coerce $ Just (DeepSeek DeepSeekReasoner)
      , botSettingSystemMessage           = globalSysMsg $ botModules botconfig
      , botSettingActiveChat              = Just False
      }
    insertMany_ [ def
      { botSettingPerChatBotName                 = maybeBotName botname
      , botSettingPerChatBotId                   = botId $ botModules botconfig
      , botSettingPerChatChatId                  = chatId
      , botSettingPerChatSystemMessage           = content <$> systemMessage chatSetting
      , botSettingPerChatSystemTemp              = systemTemp chatSetting
      , botSettingPerChatSystemMaxToolDepth      = systemMaxToolDepth chatSetting
      , botSettingPerChatSystemAPIKeyOpenAI      = apiKeyOpenAI      =<< systemApiKeys chatSetting
      , botSettingPerChatSystemAPIKeyDeepSeek    = apiKeyDeepSeek    =<< systemApiKeys chatSetting
      , botSettingPerChatSystemAPIKeyOpenRouter  = apiKeyOpenRouter  =<< systemApiKeys chatSetting
      , botSettingPerChatSystemAPIKeySiliconFlow = apiKeySiliconFlow =<< systemApiKeys chatSetting
      } | (chatId, chatSetting) <- chatSettings sd]
    insertMany_ [ InUserGroup
      { inUserGroupBotName          = maybeBotName botname
      , inUserGroupBotId            = botid
      , inUserGroupUserId           = userId
      , inUserGroupUserGroup        = userGroup
      } | (userId, userGroup) <- userGroups sd]
    insertMany_ [ InGroupGroup
      { inGroupGroupBotName         = maybeBotName botname
      , inGroupGroupBotId           = botid
      , inGroupGroupGroupId         = groupId
      , inGroupGroupGroupGroup      = groupGroup
      } | (groupId, groupGroup) <- groupGroups sd]
    insertMany_ [ CommandRuleDB
      { commandRuleDBBotName        = maybeBotName botname
      , commandRuleDBBotId          = botid
      , commandRuleDBCommandRule    = commandRule
      } | commandRule <- commandRules sd]
    insertMany_ [ SavedAdditionalData
      { savedAdditionalDataBotName  = maybeBotName botname
      , savedAdditionalDataBotId    = botid
      , savedAdditionalDataAdditionalData = PersistUseShow savedAdditional
      } | savedAdditional <- savedAdditional sd]
    insertMany_ [ BookDB
      { bookDBBookName    = book_name book
      , bookDBBookPdfPath = book_pdfPath book
      , bookDBBookPages   = book_pages book
      , bookDBBookInfo    = book_info book
      } | book <- books sd]
    )
