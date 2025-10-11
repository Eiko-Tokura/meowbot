module System.WatchDog where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Effect
import Data.Kind (Type)

-- watch dog has its own thread
-- it reads something (provide a handle to it) periodically to check the status
--
-- the logic is simple:
--
-- * If the status is good, do nothing
--
-- * If the status is bad for 5 continuous checks, do something (use a handle)
--
-- * after doing something, do nothing until the status is good again where the counter is reset

data WatchDogStatus = CountGood !Int | CountBad !Int
  deriving Show

data WatchDog (a :: Type)
  -- { wdExtraState    :: TVar a
  -- , wdCheckInterval :: Int -- in seconds
  -- , wdCheckCount    :: TVar WatchDogStatus
  -- , wdCheckHandle   :: IO Bool
  -- , wdAction        :: IO ()
  -- }

instance Module (WatchDog a) where
  data ModuleRead (WatchDog a) = WatchDogRead
    { wdExtraStateTVar :: TVar a
    , wdCheckCountTVar :: TVar WatchDogStatus
    }
  data ModuleState (WatchDog a) = WatchDogState

instance SystemModule (WatchDog a) where
  data ModuleEvent (WatchDog a)    = WatchDogEvent
  data ModuleInitData (WatchDog a) = WatchDogInitData
    { wdExtraState    :: TVar a
    , wdCheckInterval :: Int -- in seconds
    , wdCheckHandle   :: TVar a -> IO Bool
    , wdAction        :: IO ()
    }

type WatchDogRead a = ModuleRead (WatchDog a)

withWatchDog
  :: ( MonadIO m
     , MonadMask m
     , ConsFDataList FData (WatchDog a : mods)
     )
  => ModuleInitData (WatchDog a)
  -> (WatchDogRead a -> EffT mods es m b)
  -> EffT mods es m b
withWatchDog init act = bracketEffT
  (do
    wd  <- liftIO $ initWatchDog  init
    tid <- liftIO $ startWatchDog init wd
    return (wd, tid)
  )
  (\(_, tid) -> liftIO $ killThread tid)
  (\(wd, _) -> act wd)

-- Function to start the watchdog
startWatchDog :: ModuleInitData (WatchDog a) -> WatchDogRead a -> IO ThreadId
startWatchDog init wd = forkIO $ forever $ do
  threadDelay (wdCheckInterval init * 1_000_000) -- Convert seconds to microseconds
  checkStatus init wd

-- Function to check the status
checkStatus :: ModuleInitData (WatchDog a) -> WatchDogRead a -> IO ()
checkStatus init wd = do
  statusCount <- readTVarIO (wdCheckCountTVar wd)
  currentGood <- wdCheckHandle init init.wdExtraState
  unless currentGood $ do
    putStrLn "WatchDog: Bad Status Detected"
    putStrLn $ "Current Status Count: " ++ show statusCount
  case (statusCount, currentGood) of
    (CountBad n, False) | n == 5  -> do
        atomically $ writeTVar (wdCheckCountTVar wd) (CountBad (n + 1))
        putStrLn $ "WatchDog: Bad Status Detected for " <> show n <> " times, taking action"
        wdAction init

    (CountGood n, True)           -> atomically $ writeTVar (wdCheckCountTVar wd) (CountGood (n + 1))
    (CountBad n, False)           -> atomically $ writeTVar (wdCheckCountTVar wd) (CountBad (n + 1))

    (CountGood _, False)          -> atomically $ writeTVar (wdCheckCountTVar wd) (CountBad 1)
    (CountBad _, True)            -> atomically $ writeTVar (wdCheckCountTVar wd) (CountGood 1)

-- Function to initialize the watchdog
initWatchDog
  :: ModuleInitData (WatchDog a)
  -> IO (WatchDogRead a)
initWatchDog (WatchDogInitData {wdExtraState}) = do
  checkCount   <- newTVarIO (CountGood 0)
  return $ WatchDogRead
            { wdExtraStateTVar = wdExtraState
            , wdCheckCountTVar = checkCount
            }
