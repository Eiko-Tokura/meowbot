{-# LANGUAGE TemplateHaskell, OverloadedStrings, PartialTypeSignatures #-}
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
import Control.Monad.Trans.Control
import Control.System
import Data.Coerce
import Data.Default
import Data.Maybe
import Data.PersistModel
import External.ChatAPI
import MeowBot
import MeowBot.CronTab.PeriodicCost (initializeCounters)
import MeowBot.CommandRule
import MeowBot.Data.Book
import Module.AsyncInstance
import Module.BotGlobal
import Module.CommandInstance
import Module.ConnectionManager
import Module.CronTabTickInstance
import Module.Database
import Module.LogDatabase
import Module.Logging
import Module.MeowConnection
import Module.MeowTypes
import Module.Prometheus.Manager
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

pingpongOptions = defaultPingPongOptions
  -- { pingInterval = 30
  -- , pongTimeout  = 30
  -- }

runBot :: BotInstance -> Meow a -> EffT '[BotGlobal, ConnectionManagerModule, MeowDatabase, PrometheusMan, LoggingModule] '[ErrorText "meowdb"] IO ()
runBot bot meow = do
  botm     <- embedEffT $ botInstanceToModule bot
  glbMesChan <- asksModule globalMessageChannel
  meowStat <- liftIO $ initTVarMeowStatus
  let botconfig = BotConfig botm (botDebugFlags bot) Nothing
      watchDog  = listToMaybe $ botWatchDogFlags bot

      checkHandle = fmap (== MeowOnline) . readTVarIO
      mWatchDogInit = (\(WatchDogFlag interval action) -> WatchDogInitData meowStat interval checkHandle (case action of SystemCmd cmd -> void $ system cmd; SendToId uid -> atomically $ modifyTVar' glbMesChan $ ((PrivateChat uid, toText botm.nameOfBot <> "似乎睡着了！"):) )) <$> watchDog

  initializeCounters botm.botId

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
    $ withAsyncModule
    $ withLogDatabase
    $ meow

runBots :: [BotInstance] -> EffT '[BotGlobal, ConnectionManagerModule, MeowDatabase, PrometheusMan, LoggingModule] '[] IO ()
runBots bots = mapM_ (\bot -> forkEffT $ runBot bot botLoop) bots >> liftIO (threadDelay maxBound)

withServerConnection
  :: (ConsFDataList FData (MeowConnection : mods), LoggingModule `In` mods, m ~ IO, Show (EList es))
  => String -> Int -> EffT (MeowConnection : mods) es m a -> EffT mods NoError m ()
withServerConnection addr port act = do

  $(logInfo) $ "Running bot server, listening on " <> tshow addr <> ":" <> tshow port
  effTryWith (\(e :: SomeException) -> errorText @"uncaught" (toText e))
    (foreverEffT $ do

      liftWith $ \run -> runServer addr port $ \pending -> do
        conn' <- acceptRequest pending
  
        withPingPong pingpongOptions conn' $ \conn -> void $ do
          run $ do
            $logInfo "Connected to client"
            runMeowConnection (MeowConnectionRead conn) (void act) `effCatchAll`
              (\e -> do
                $logError $ "ERROR In connection with client : " <> tshow e
              )
  
        void . run $ $logInfo $ "Disconnected from client"

      $logInfo "Server quits, restoring state and looping"


    ) `effCatch` \(e :: ErrorText "uncaught") -> do
      $logError $ "Uncaught Error In server: " <> toText e
      $logInfo "Server crashed, restarting server in 20 seconds"

      liftIO $ threadDelay 20_000_000
      withServerConnection addr port act

withClientConnection
  :: ( ConsFDataList FData (MeowConnection : mods)
     , LoggingModule `In` mods, m ~ IO, Show (EList es)
     )
  => String -> Int -> EffT (MeowConnection : mods) es m a -> EffT mods NoError m ()
withClientConnection addr port act = do
  
  $(logInfo) $ "Running bot client, connecting to " <> tshow addr <> ":" <> tshow port
  effTryWith (\(e :: SomeException) -> errorText @"uncaught" (toText e))
    (foreverEffT $ do

      liftWith $ \run -> runClient addr port "" $ \conn -> void . run $ do

        $logInfo $ "Connected to server " <> tshow addr <> ":" <> tshow port

        liftIO $ withPingPong pingpongOptions conn $ \conn -> do
          void . run $ do
            $logInfo "Connected to client"
            runMeowConnection (MeowConnectionRead conn) (void act)
              `effCatchAll` (\(e :: EList es) -> do
                $logError $ "ERROR In connection with client : " <> tshow e
              )

    ) `effCatch` \(e :: ErrorText "uncaught") -> do
        $logError $ "Uncaught Error In client: " <> toText e
        $logInfo "Restarting client in 20 seconds"
        liftIO $ threadDelay 20_000_000

        withClientConnection addr port act

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
      AllData mempty botconfig . OtherData 0 Nothing mempty savedDataDB (coerce $ savedAdditional savedDataDB) <$> lift getAllScripts
    (Left errDb, Right savedDataFile) -> do
      $(logError) $ "Failed to load saved data from database: " <> toText errDb
      $(logInfo) "Loaded saved data from file"
      $(logInfo) $ "Trying to migrate data from file to database"
      newSavedDataDB botconfig savedDataFile
      AllData mempty botconfig . OtherData 0 Nothing mempty savedDataFile (coerce $ savedAdditional savedDataFile) <$> lift getAllScripts
    (Left errDb, Left errFile) -> do
      $(logError) $ "Failed to load saved data from database: " <> toText errDb
      $(logError) $ "Failed to load saved data from file: " <> toText errFile
      $(logInfo) "Starting with empty data, also writing empty data to database"
      newSavedDataDB botconfig (SavedData [] initialUGroups initialGGroups initialRules initialBooks [])
      AllData mempty botconfig . OtherData 0 Nothing mempty (SavedData [] initialUGroups initialGGroups initialRules initialBooks []) [] <$> lift getAllScripts
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
