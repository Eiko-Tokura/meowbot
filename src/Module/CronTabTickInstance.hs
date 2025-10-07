{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module generates cron tab ticks
-- it is then passed to specific Meow handler to handle the ticks and cron jobs
module Module.CronTabTickInstance where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Module.RS
import Control.Monad.Effect
import Data.Time
import MeowBot.BotStructure
import MeowBot.CronTab
import MeowBot.CronTab.PeriodicCost
import Module.CronTabTick
import Module.RS.QQ
import System.Meow
import Control.System

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

instance Dependency' c CronTabTickModule '[MeowActionQueue] mods
  => EventLoop c CronTabTickModule mods es where


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


  -- data ModuleLocalState CronTabTickModule      = CronTabTickModuleL
  --   { cronTickAsync :: Async CronTabTick
  --   }
  -- data ModuleEarlyLocalState CronTabTickModule = CronTabTickEarlyLocalState
  -- data ModuleGlobalState CronTabTickModule     = CronTabTickModuleG
  -- data ModuleEvent CronTabTickModule           = CronTabTickEvent { cronTabTick :: CronTabTick }
  -- data ModuleInitDataG CronTabTickModule       = CronTabTickInitDataG
  -- data ModuleInitDataL CronTabTickModule       = CronTabTickInitDataL

  --getInitDataG _ = (Just CronTabTickInitDataG, empty)

  --getInitDataL _ = (Just CronTabTickInitDataL, empty)

  --initModule _ _ = return CronTabTickModuleG

  --initModuleLocal _ _ _ _ _ = do
  --  newTick <- liftIO $ newCronTabTick Nothing
  --  return $ CronTabTickModuleL newTick

  --initModuleEarlyLocal _ _ _ = return CronTabTickEarlyLocalState

  --quitModule _ = do
  --  CronTabTickModuleL asyncTick <- readModuleStateL (Proxy @CronTabTickModule)
  --  liftIO $ cancel asyncTick

  --moduleEvent _ = do
  --  CronTabTickModuleL asyncTick <- readModuleStateL (Proxy @CronTabTickModule)
  --  return $ CronTabTickEvent <$> waitSTM asyncTick

  --moduleEventHandler p (CronTabTickEvent tick) = do
  --  $(logDebug) "CronTab Event Completed"
  --  newTick <- liftIO $ newCronTabTick (Just tick)
  --  modifyModuleState p $ \_ -> CronTabTickModuleL newTick
  --  meowList <- asks (readSystem . snd)
  --  liftIO $ atomically $ modifyTVar meowList (<> [meowHandleCronTabTick tick, periodicCostHandleCronTabTick tick])

-- | generate a new tick thread
-- it will return if a new minute has reached
newCronTabTick :: TMVar CronTabTick -> IO ThreadId
newCronTabTick putHere = do
  tickTime0 <- getCurrentTime
  forkIO $ forever $ do
    let waitUntilNewMinute = do
          threadDelay 1_000_000 -- 1 second
          now <- getCurrentTime
          let diff = diffUTCTime now (floorToMinute tickTime0)
          if diff >= 60
          then atomically $ writeTMVar putHere $ CronTabTick now
          else waitUntilNewMinute
    waitUntilNewMinute

floorToMinute :: UTCTime -> UTCTime
floorToMinute t = UTCTime (utctDay t) (secondsToDiffTime (floor (utctDayTime t / 60) * 60))
