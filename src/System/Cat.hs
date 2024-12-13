{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module System.Cat where

import System.General
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Control.Applicative
import System
import Data.HList
import MeowBot
import MeowBot.CommandRule
import Network.WebSockets
import Command
import Command.Aokana
import Module.CommandInstance
import Module.AsyncInstance
import Module.LogDatabase

import System.Directory
import Data.Coerce
import Data.Maybe

import External.ProxyWS

botLoop :: Cat never_returns
botLoop = do
  CatT $ handleEvents =<< liftIO . atomically =<< listenToEvents
  performMeowActions
  botLoop

performMeowActions :: Cat ()
performMeowActions = do
  actions <- askSystem
  conn    <- askSystem
  as <- liftIO . atomically $ readTVar actions <* writeTVar actions []
  globalizeMeow $ mapM_ (doBotAction conn) =<< (concat <$> sequence as)

type R = MeowData

-- initializeGlobal :: ModuleInitDataG LogDatabase -> LoggingT IO (AllModuleGlobalStates Mods)
-- initializeGlobal ld = initAllModulesG @R (CommandInitDataG :** AsyncInitDataG :** FNil) --ld :** FNil)

-- the process of initialization:
--
-- in the outer layer, init all global states
--
-- in each inner layer where we run each bot, init all local states
--
-- then initialize AllData, and run the botLoop


allInitDataG :: AllModuleInitDataG Mods
allInitDataG = CommandInitDataG :** AsyncInitDataG :** FNil --LogDatabaseInitDataG "meowbot.db" :** FNil

allInitDataL :: AllModuleInitDataL Mods
allInitDataL = CommandInitDataL :** AsyncInitDataL :** FNil --LogDatabaseInitDataL :** FNil

runBots :: AllModuleInitDataG Mods -> [BotInstance] -> LoggingT IO ()
runBots initglobs bots = do
  $(logInfo) "Initializing all global states"
  global <- initAllModulesG @R initglobs
  $(logInfo) "Starting bot instances"
  mapM_ (runBot initglobs global) bots

-- | All these are read-only so no problem to reuse them when restarting the bot
runBot :: AllModuleInitDataG Mods -> AllModuleGlobalStates Mods -> BotInstance -> LoggingT IO ()
runBot initglobs glob bot = do
  logger    <- askLoggerIO
  case botRunFlag bot of
    RunClient ip port -> do
      $(logInfo) $ "Connecting to " <> tshow ip <> ":" <> tshow port
      liftIO $ runClient ip port "" $ \conn -> flip runLoggingT logger $ do
        meowData <- liftIO $ MeowData conn <$> newTVarIO [] <*> newTVarIO Nothing <*> newTVarIO Nothing
        local    <- initAllModulesL @R meowData initglobs allInitDataL
        botm     <- botInstanceToModule bot
        let botconfig = BotConfig botm (botDebugFlags bot)
        alldata  <- initAllData botconfig
        lift . void $ flip runLoggingT logger ((runReaderStateT (runCatT botLoop) (glob, meowData) (local, alldata)))
          `forkFinally` (flip runLoggingT logger . rerunBot initglobs glob bot)
    RunServer ip port -> do
      $(logInfo) $ "Listening on " <> tshow ip <> ":" <> tshow port
      liftIO $ runServer ip port $ \pendingconn -> flip runLoggingT logger $ do
        conn     <- liftIO $ acceptRequest pendingconn
        $(logInfo) "Accepted connection!"
        meowData <- liftIO $ MeowData conn <$> newTVarIO [] <*> newTVarIO Nothing <*> newTVarIO Nothing
        local    <- initAllModulesL @R meowData initglobs allInitDataL
        botm     <- botInstanceToModule bot
        let botconfig = BotConfig botm (botDebugFlags bot)
        alldata  <- initAllData botconfig
        liftIO $ withPingPong defaultPingPongOptions conn $ \conn ->
          void (flip runLoggingT logger $ runReaderStateT (runCatT botLoop) (glob, meowData { meowConnection = conn }) (local, alldata))


rerunBot :: AllModuleInitDataG Mods -> AllModuleGlobalStates Mods -> BotInstance -> Either SomeException a -> LoggingT IO ()
rerunBot initglobs glob bot (Left e) = do
  $(logError) $ "Bot instance failed: " <> tshow bot <> "\nWith Error: " <> tshow e
  $(logInfo) "Restarting bot instance in 60 seconds"
  liftIO $ threadDelay 60_000_000
  runBot initglobs glob bot
rerunBot _ _ _ (Right _) = do
  $(logInfo) $ "Bot instance finished successfully."

tshow :: Show a => a -> Text
tshow = pack . show

botInstanceToModule :: BotInstance -> LoggingT IO BotModules
botInstanceToModule bot@(BotInstance runFlag identityFlags commandFlags mode proxyFlags logFlags) = do
    $(logInfo) $ "\n### Starting bot instance: " <> tshow bot
    $(logInfo) $ "Running mode: "   <> tshow mode
    $(logInfo) $ "Running flags: "  <> tshow runFlag
    $(logInfo) $ "Identity flags: " <> tshow identityFlags
    $(logInfo) $ "Command flags: "  <> tshow commandFlags
    $(logInfo) $ "Proxy flags: "    <> tshow proxyFlags
    $(logInfo) $ "Log flags: "      <> tshow logFlags
    -- proxyData <- lift $ sequence $ [ createProxyData addr port | ProxyFlag addr port <- proxyFlags ]
    let mGlobalSysMsg = listToMaybe [ sysMsg | UseSysMsg sysMsg <- identityFlags ]
        withDefault def [] = def
        withDefault _ xs = xs
        botModules = BotModules
          { canUseGroupCommands   = withDefault (identifier <$> allGroupCommands)   (coerce commandFlags)
          , canUsePrivateCommands = withDefault (identifier <$> allPrivateCommands) (coerce commandFlags)
          , nameOfBot = BotName $ listToMaybe [ nameBot | UseName nameBot <- identityFlags ]
          , globalSysMsg = mGlobalSysMsg
          , proxyTChans = []--proxyData
          , logFile = [ logFile | LogFlag logFile <- logFlags ]
          , botInstance = bot
          }
    return botModules

initAllData :: BotConfig -> LoggingT IO AllData
initAllData botconfig = do
  let mods = botModules botconfig
  fileExist <- lift $ doesFileExist (savedDataPath $ nameOfBot mods)
  if fileExist
    then do
      $(logInfo) "Found saved data file, loading data! owo"
      savedData <- lift . readFile $ savedDataPath $ nameOfBot mods
      let msavedData = read savedData
      AllData [] botconfig . OtherData 0 Nothing [] msavedData (coerce $ savedAdditional msavedData) <$> lift getAllScripts
    else do
      $(logInfo) "No saved data file found, starting with empty data! owo"
      AllData [] botconfig . OtherData 0 Nothing [] (SavedData [] initialUGroups initialGGroups initialRules initialBooks []) [] <$> lift getAllScripts
  where
    initialUGroups = [(me, Admin)]
    initialGGroups = [(myGroup, AllowedGroup)]
    initialRules =
      [ Allow (UGroup Admin)          (ExceptCommands [Retract])
      , Allow AllUserAndGroups        (CGroup [Cat, Help, Md, Random])
      , Allow (GGroup AllowedGroup)   (CGroup [System, Aokana])
      , Allow (UGroup Allowed)        (CGroup [System, Aokana])
      , Allow (SingleGroup myGroup)   (SingleCommand Retract)
      , Deny  (UGroup Denied)         AllCommands
      ]
    initialBooks = []
    me = 754829466 :: UserId
    myGroup = 437447251 :: GroupId

