module Module.StatusMonitor where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Effect
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Control.System
import Data.Maybe
import MeowBot.API
import MeowBot.BotStructure
import Module.Logging
import Module.MeowConnection
import Module.RecvSentCQ
import Utils.ByteString

data StatusMonitorModule

type Waiter qt = LazyByteString -> Maybe (QueryAPIResponse qt)

data Refresh = Refresh
newtype MonitorStatus = MonitorQuerying (Waiter 'QueryGetStatus)

data MeowStatus
  = MeowOnline
  | MeowOffline
  | MeowNotResponding
  deriving (Show, Eq)

instance Module StatusMonitorModule where
  data ModuleRead  StatusMonitorModule = StatusMonitorModuleRead
  data ModuleState StatusMonitorModule = StatusMonitorModuleState
    { monitorRefresh  :: Async Refresh
    , monitorStatus   :: Maybe MonitorStatus
    , monitorWatchDog :: TVar MeowStatus
    }

instance SystemModule StatusMonitorModule where
  data ModuleEvent StatusMonitorModule = StatusMonitorModuleEvent { monitorTimeTick :: Refresh }
  data ModuleInitData StatusMonitorModule = StatusMonitorModuleInitData { usedByWatchDog :: TVar MeowStatus }

instance Loadable c StatusMonitorModule mods ies where
  withModule (StatusMonitorModuleInitData tvar) act = do
    asyncInit <- liftIO $ async newTick
    runEffTOuter_ StatusMonitorModuleRead (StatusMonitorModuleState asyncInit Nothing tvar) act

instance
  ( Dependency' c StatusMonitorModule '[RecvSentCQ, MeowConnection, LoggingModule] mods
  , InList (ErrorText "send_connection") es
  ) => EventLoop c StatusMonitorModule mods es where
  moduleEvent = do
    s <- getsModule monitorRefresh
    return $ StatusMonitorModuleEvent <$> waitSTM s

  handleEvent _ = do
    $logDebug "Tick, wake up and query Meow status"
    localState <- getModule
    case monitorStatus localState of
      Nothing -> do
        $(logDebug) "Querying Meow status"
        wait <- queryAPI GetStatus
        $(logDebug) "Querying Meow status sent, new tick started"
        continue wait
      Just _ -> do
        $(logInfo) "Last query has no response, querying meow status AGAIN"
        liftIO $ atomically $ writeTVar (monitorWatchDog localState) MeowNotResponding
        wait <- queryAPI GetStatus
        $(logDebug) "Querying Meow status sent, new tick started"
        continue wait
    where continue wait = do
            asyncNewTick <- liftIO $ async newTick
            modifyModule @StatusMonitorModule $ \s -> s
              { monitorRefresh = asyncNewTick
              , monitorStatus  = Just $ MonitorQuerying wait
              }

  afterEvent = do
    localState <- getModule @StatusMonitorModule
    case monitorStatus localState of
      Just (MonitorQuerying waiter) -> fmap (fromMaybe ()) . runMaybeT $ do
        rawBS  <- MaybeT $ asksModule meowRawByteString >>= liftIO . readTVarIO
        result <- MaybeT . pure $ waiter rawBS
        $(logDebug) $ "Received Meow status: " <> toText result
        case result of
          GetStatusResponse True True -> lift $ do
            liftIO $ atomically $ writeTVar (monitorWatchDog localState) MeowOnline
            modifyModule @StatusMonitorModule $ \s -> s { monitorStatus = Nothing }
          GetStatusResponse _ _ -> lift $ do
            liftIO $ atomically $ writeTVar (monitorWatchDog localState) MeowOffline
            modifyModule @StatusMonitorModule $ \s -> s { monitorStatus = Nothing }
            $(logInfo) $ "Received Non-Good Meow status: " <> toText result
      Nothing -> return ()

newTick = do
  threadDelay $ 1000 * 1000 * 60 * 2
  return Refresh

initTVarMeowStatus :: IO (TVar MeowStatus)
initTVarMeowStatus = newTVarIO MeowOnline
