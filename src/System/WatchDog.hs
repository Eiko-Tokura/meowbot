module System.WatchDog where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)

-- watch dog has its own thread
-- it reads something (provide a handle to it) periodically to check the status
--
-- the logic is simple:
--
-- * If the status is good, do nothing
--
-- * If the status is bad for 3 continuous checks, do something (use a handle)
--
-- * after doing something, do nothing until the status is good again where the counter is reset

data WatchDogStatus = CountGood Int | CountBad Int

data WatchDog a = WatchDog
  { wdExtraState    :: TVar a
  , wdCheckInterval :: Int -- in seconds
  , wdCheckCount    :: TVar WatchDogStatus
  , wdCheckHandle   :: TVar a -> IO Bool
  , wdAction        :: IO ()
  }

-- Function to start the watchdog
startWatchDog :: WatchDog a -> IO ThreadId
startWatchDog wd = forkIO $ forever $ do
  threadDelay (wdCheckInterval wd * 1_000_000) -- Convert seconds to microseconds
  checkStatus wd

-- Function to check the status
checkStatus :: WatchDog a -> IO ()
checkStatus wd = do
  statusCount <- atomically $ readTVar (wdCheckCount wd)
  currentGood <- wdCheckHandle wd (wdExtraState wd)
  case (statusCount, currentGood) of
    (CountGood n, True)           -> atomically $ writeTVar (wdCheckCount wd) (CountGood (n + 1))
    (CountGood _, False)          -> atomically $ writeTVar (wdCheckCount wd) (CountBad 1)
    (CountBad n, False) | n >= 5  -> do
        atomically $ writeTVar (wdCheckCount wd) (CountBad 0)
        wdAction wd
    (CountBad n, False)           -> atomically $ writeTVar (wdCheckCount wd) (CountBad (n + 1))
    (CountBad _, True)            -> atomically $ writeTVar (wdCheckCount wd) (CountGood 1)

-- Function to initialize the watchdog
initWatchDog :: TVar a -> Int -> (TVar a -> IO Bool) -> IO () -> IO (WatchDog a)
initWatchDog extraState interval checkHandle action = do
  checkCount   <- atomically $ newTVar (CountGood 0)
  return $ WatchDog
            { wdExtraState    = extraState
            , wdCheckInterval = interval
            , wdCheckCount    = checkCount
            , wdCheckHandle   = checkHandle
            , wdAction        = action
            }
