module Module.StatusMonitor where

import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import MeowBot.BotStructure
import Data.Maybe
import Network.WebSockets hiding (Response)
import qualified Data.ByteString.Lazy as BL

import Module
import MeowBot.API

import Utils.ByteString

data StatusMonitorModule

type Waiter qt = LazyByteString -> Maybe (QueryAPIResponse qt)

data Refresh = Refresh
data MonitorStatus
  = MonitorQuerying (Waiter 'QueryGetStatus)

data MeowStatus
  = MeowOnline
  | MeowOffline
  | MeowNotResponding
  deriving (Show, Eq)

instance
  ( HasSystemRead (TVar (Maybe BL.ByteString)) r -- ^ read raw message
  , HasSystemRead Connection r                   -- ^ the connection to the server
  )
  => MeowModule r AllData StatusMonitorModule where

  data ModuleLocalState  StatusMonitorModule = StatusMonitorL
    { monitorRefresh  :: Async Refresh
    , monitorStatus   :: Maybe MonitorStatus
    , monitorWatchDog :: TVar MeowStatus
    }
  data ModuleGlobalState StatusMonitorModule = StatusMonitorG
  data ModuleEvent       StatusMonitorModule = StatusMonitorEvent { monitorTimeTick :: Refresh }

  data ModuleInitDataG StatusMonitorModule = StatusMonitorInitDataG
  data ModuleInitDataL StatusMonitorModule = StatusMonitorInitDataL { usedByWatchDog :: TVar MeowStatus }
  data ModuleEarlyLocalState StatusMonitorModule = StatusMonitorEarlyLocalState { elUsedByWatchDog :: TVar MeowStatus }

  getInitDataG _ = (Just StatusMonitorInitDataG, empty)

  getInitDataL _ = (Nothing, empty)

  initModule _ _ = return StatusMonitorG

  initModuleLocal _ _ _ (StatusMonitorInitDataL tvar) _ = do
    $(logInfo) "Init, new tick started"
    liftIO $ StatusMonitorL
      <$> async newTick
      <*> return Nothing
      <*> return tvar

  initModuleEarlyLocal _ _ (StatusMonitorInitDataL tvar) = return $ StatusMonitorEarlyLocalState tvar

  moduleEvent _ = do
    StatusMonitorL async _status _ <- readModuleStateL (Proxy :: Proxy StatusMonitorModule)
    return $ StatusMonitorEvent <$> waitSTM async

  moduleEventHandler _ _ = do
    $(logDebug) "Tick, wake up and query Meow status"
    conn <- askSystem
    localState <- readModuleStateL (Proxy :: Proxy StatusMonitorModule)
    case monitorStatus localState of
      Nothing -> do
        $(logDebug) "Querying Meow status"
        wait <- lift $ queryAPI conn $ GetStatus
        $(logDebug) "Querying Meow status sent, new tick started"
        continue wait
      Just _ -> do
        $(logInfo) "Last query has no response, querying meow status AGAIN"
        liftIO $ atomically $ writeTVar (monitorWatchDog localState) MeowNotResponding
        wait <- lift $ queryAPI conn $ GetStatus
        $(logDebug) "Querying Meow status sent, new tick started"
        continue wait
    where continue wait = do
            asyncNewTick <- liftIO $ async newTick
            modifyModuleState (Proxy @StatusMonitorModule) $ \s -> s
              { monitorRefresh = asyncNewTick
              , monitorStatus  = Just $ MonitorQuerying wait
              }

  -- | In this function, we need to read the raw message and handle it.
  afterMeow _ = do
    localState <- readModuleStateL (Proxy :: Proxy StatusMonitorModule)
    case monitorStatus localState of
      Just (MonitorQuerying waiter) -> fmap (fromMaybe ()) . runMaybeT $ do
        rawBS <- MaybeT $ askSystem @(TVar (Maybe BL.ByteString)) >>= liftIO . atomically . readTVar
        result <- MaybeT . pure $ waiter rawBS
        $(logDebug) $ "Received Meow status: " <> toText result
        case result of
          GetStatusResponse True True -> lift $ do
            liftIO $ atomically $ writeTVar (monitorWatchDog localState) MeowOnline
            modifyModuleState (Proxy @StatusMonitorModule) $ \s -> s { monitorStatus = Nothing }
          GetStatusResponse _ _ -> lift $ do
            liftIO $ atomically $ writeTVar (monitorWatchDog localState) MeowOffline
            modifyModuleState (Proxy @StatusMonitorModule) $ \s -> s { monitorStatus = Nothing }
            $(logInfo) $ "Received Non-Good Meow status: " <> toText result
      Nothing -> return ()

newTick = do
  threadDelay $ 1000 * 1000 * 60 * 2
  return Refresh

initTVarMeowStatus :: IO (TVar MeowStatus)
initTVarMeowStatus = newTVarIO MeowOnline
