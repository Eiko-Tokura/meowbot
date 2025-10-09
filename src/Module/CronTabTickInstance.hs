{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module generates cron tab ticks
-- it is then passed to specific Meow handler to handle the ticks and cron jobs
module Module.CronTabTickInstance where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Logger
import Control.System
import Data.Time
import MeowBot.CronTab
import MeowBot.CronTab.PeriodicCost
import Module.CronTabTick
import Module.Logging
import System.Meow

instance
  ( -- HasSystemRead (TVar [Meow [BotAction]]) r
  )
  => Module CronTabTickModule where
  data ModuleRead CronTabTickModule  = CronTabTickModuleRead
    { recvTick :: TMVar CronTabTick
    }
  data ModuleState CronTabTickModule = CronTabTickModuleState

instance SystemModule CronTabTickModule where
  data ModuleInitData CronTabTickModule = CronTabTickModuleInitData
  data ModuleEvent CronTabTickModule = CronTabTickModuleEvent { cronTabTick :: CronTabTick }

instance Dependency' c CronTabTickModule '[MeowActionQueue] mods => Loadable c CronTabTickModule mods ies where
  withModule _ act = bracketEffT
    (do
      recvTick <- liftIO newEmptyTMVarIO
      thread <- liftIO $ newCronTabTick recvTick
      return (recvTick, thread)
    )
    ( \(_, thread) -> liftIO $ killThread thread
    )
    (\(recvTick, _) -> runEffTOuter_ (CronTabTickModuleRead recvTick) CronTabTickModuleState act
    )

instance Dependency' c CronTabTickModule '[MeowActionQueue, LoggingModule] mods
  => EventLoop c CronTabTickModule mods es where

  moduleEvent = do
    asyncTick <- asksModule recvTick
    return $ CronTabTickModuleEvent <$> readTMVar asyncTick

  handleEvent (CronTabTickModuleEvent tick) = do
    $logDebug "CronTab Event Received"
    meowList <- asksModule meowReadsAction
    liftIO $ atomically $ modifyTVar meowList (<> [meowHandleCronTabTick tick, periodicCostHandleCronTabTick tick])

withCronTabTick :: (MonadMask m, MonadIO m, MeowActionQueue `In` mods, ConsFDataList FData (CronTabTickModule : mods))
  => EffT (CronTabTickModule : mods) es m a
  -> EffT mods es m a
withCronTabTick act = bracketEffT
  (do
    recvTick <- liftIO newEmptyTMVarIO
    thread <- liftIO $ newCronTabTick recvTick
    return (recvTick, thread)
  )
  ( \(_, thread) -> liftIO $ killThread thread
  )
  (\(recvTick, _) -> runEffTOuter_ (CronTabTickModuleRead recvTick) CronTabTickModuleState act
  )

-- | generate a new tick thread
-- it will return if a new minute has reached
newCronTabTick :: TMVar CronTabTick -> IO ThreadId
newCronTabTick putHere = do
  tickTime0 <- getCurrentTime
  forkIO $ forever $ do
    threadDelay 1_000_000 -- 1 second
    now <- getCurrentTime
    let diff = diffUTCTime now (floorToMinute tickTime0)
    when (diff >= 60) $ atomically $ writeTMVar putHere $ CronTabTick now

floorToMinute :: UTCTime -> UTCTime
floorToMinute t = UTCTime (utctDay t) (secondsToDiffTime (floor (utctDayTime t / 60) * 60))
