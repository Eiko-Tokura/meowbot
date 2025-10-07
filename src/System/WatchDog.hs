module System.WatchDog where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, unless)
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

data WatchDogStatus = CountGood Int | CountBad Int
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
    { wdExtraState    :: TVar a
    , wdCheckInterval :: Int -- in seconds
    , wdCheckCount    :: TVar WatchDogStatus
    , wdCheckHandle   :: IO Bool
    , wdAction        :: IO ()
    }
  data ModuleState (WatchDog a) = WatchDogState

type WatchDogRead a = ModuleRead (WatchDog a)

-- Function to start the watchdog
startWatchDog :: WatchDogRead a -> IO ThreadId
startWatchDog wd = forkIO $ forever $ do
  threadDelay (wdCheckInterval wd * 1_000_000) -- Convert seconds to microseconds
  checkStatus wd

-- Function to check the status
checkStatus :: WatchDogRead a -> IO ()
checkStatus wd = do
  statusCount <- readTVarIO (wdCheckCount wd)
  currentGood <- wdCheckHandle wd
  unless currentGood $ do
    putStrLn "WatchDog: Bad Status Detected"
    putStrLn $ "Current Status Count: " ++ show statusCount
  case (statusCount, currentGood) of
    (CountBad n, False) | n == 5  -> do
        atomically $ writeTVar (wdCheckCount wd) (CountBad (n + 1))
        putStrLn $ "WatchDog: Bad Status Detected for " <> show n <> " times, taking action"
        wdAction wd

    (CountGood n, True)           -> atomically $ writeTVar (wdCheckCount wd) (CountGood (n + 1))
    (CountBad n, False)           -> atomically $ writeTVar (wdCheckCount wd) (CountBad (n + 1))

    (CountGood _, False)          -> atomically $ writeTVar (wdCheckCount wd) (CountBad 1)
    (CountBad _, True)            -> atomically $ writeTVar (wdCheckCount wd) (CountGood 1)

-- Function to initialize the watchdog
initWatchDog
  :: TVar a
  -> Int
  -> (TVar a -> IO Bool) -- ^ Check handle, using the extra state, and possibly reads other things, determine good or bad
  -> IO ()
  -> IO (WatchDogRead a)
initWatchDog extraState interval checkHandle action = do
  checkCount   <- newTVarIO (CountGood 0)
  return $ WatchDogRead
            { wdExtraState    = extraState
            , wdCheckInterval = interval
            , wdCheckCount    = checkCount
            , wdCheckHandle   = checkHandle extraState
            , wdAction        = action
            }
