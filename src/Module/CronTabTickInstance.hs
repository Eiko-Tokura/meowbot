{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module generates cron tab ticks
-- it is then passed to specific Meow handler to handle the ticks and cron jobs
module Module.CronTabTickInstance where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad.Trans.ReaderState
import Data.Time
import MeowBot.BotStructure
import MeowBot.CronTab
import Module
import Module.CronTabTick
import System.Meow

instance
  ( HasSystemRead (TVar [Meow [BotAction]]) r
  )
  => MeowModule r AllData CronTabTickModule where
  data ModuleLocalState CronTabTickModule      = CronTabTickModuleL
    { cronTickAsync :: Async CronTabTick
    }
  data ModuleEarlyLocalState CronTabTickModule = CronTabTickEarlyLocalState
  data ModuleGlobalState CronTabTickModule     = CronTabTickModuleG
  data ModuleEvent CronTabTickModule           = CronTabTickEvent { cronTabTick :: CronTabTick }
  data ModuleInitDataG CronTabTickModule       = CronTabTickInitDataG
  data ModuleInitDataL CronTabTickModule       = CronTabTickInitDataL

  getInitDataG _ = (Just CronTabTickInitDataG, empty)

  getInitDataL _ = (Just CronTabTickInitDataL, empty)

  initModule _ _ = return CronTabTickModuleG

  initModuleLocal _ _ _ _ _ = do
    newTick <- liftIO $ newCronTabTick Nothing
    return $ CronTabTickModuleL newTick

  initModuleEarlyLocal _ _ _ = return CronTabTickEarlyLocalState

  quitModule _ = do
    CronTabTickModuleL asyncTick <- readModuleStateL (Proxy @CronTabTickModule)
    liftIO $ cancel asyncTick

  moduleEvent _ = do
    CronTabTickModuleL asyncTick <- readModuleStateL (Proxy @CronTabTickModule)
    return $ CronTabTickEvent <$> waitSTM asyncTick

  moduleEventHandler p (CronTabTickEvent tick) = do
    $(logDebug) "CronTab Event Completed"
    newTick <- liftIO $ newCronTabTick (Just tick)
    modifyModuleState p $ \_ -> CronTabTickModuleL newTick
    meowList <- asks (readSystem . snd)
    liftIO $ atomically $ modifyTVar meowList (<> [meowHandleCronTabTick tick])

-- | generate a new tick thread
-- it will return if a new minute has reached
newCronTabTick :: Maybe CronTabTick -> IO (Async CronTabTick)
newCronTabTick Nothing = do
  async $ do
    now <- getCurrentTime
    return $ CronTabTick now
newCronTabTick (Just (CronTabTick tickTime0)) = do
  async $ do
    let waitUntilNewMinute = do
          threadDelay 1_000_000 -- 1 second
          now <- getCurrentTime
          let diff = diffUTCTime now (floorToMinute $ tickTime0)
          if diff >= 60
          then return $ CronTabTick now
          else waitUntilNewMinute
    waitUntilNewMinute

floorToMinute :: UTCTime -> UTCTime
floorToMinute t = UTCTime (utctDay t) (secondsToDiffTime (floor (utctDayTime t / 60) * 60))
