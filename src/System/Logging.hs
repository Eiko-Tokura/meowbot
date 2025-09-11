{-# OPTIONS_GHC -Wno-unused-matches #-}{-# LANGUAGE OverloadedStrings #-}
module System.Logging
  ( module Control.Monad.Logger
  , myLogger
  , runLoggingConcurrent
  , runMyLogging
  ) where

import Control.Monad.Logger
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.ByteString as B

import MeowBot.Data
import Data.Time.Clock

-- | Log information, determined by the location, source, level and message.
-- it will both print to the console and write to a file.
myLogger :: Bool -> [DebugFlag] -> [FilePath] -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
myLogger _ [] fps (Loc fn pkg mod locS locE) src LevelDebug msg = return ()
myLogger _ [DebugOther _] fps (Loc fn pkg mod locS locE) src LevelDebug msg = return ()
myLogger time _  fps (Loc fn pkg mod locS locE) src LevelDebug msg = do
  timeStr <- if time then (<> " ") . toLogStr . show <$> getCurrentTime else return ""
  let log = fromLogStr $ "[DEBUG] " <> timeStr <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger time _ fps (Loc fn pkg mod locS locE) src LevelInfo msg = do
  timeStr <- if time then (<> " ") . toLogStr . show <$> getCurrentTime else return ""
  let log = fromLogStr $ "[INFO] " <> timeStr <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger time _ fps (Loc fn pkg mod locS locE) src LevelWarn msg = do
  timeStr <- if time then (<> " ") . toLogStr . show <$> getCurrentTime else return ""
  let log = fromLogStr $ "[WARN] " <> timeStr <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger time _ fps (Loc fn pkg mod locS locE) src LevelError msg = do
  timeStr <- if time then (<> " ") . toLogStr . show <$> getCurrentTime else return ""
  let log = fromLogStr $ "[ERROR] " <> timeStr <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger time [] fps (Loc fn pkg mod locS locE) src (LevelOther lv) msg = return ()
myLogger time _ fps (Loc fn pkg mod locS locE) src (LevelOther lv) msg = do
  timeStr <- if time then (<> " ") . toLogStr . show <$> getCurrentTime else return ""
  let log = fromLogStr $ "[" <> toLogStr lv <> "] " <> timeStr <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
{-# INLINE myLogger #-}

runMyLogging :: Bool -> BotInstance -> LoggingT IO a -> IO a
runMyLogging useTime botin mlog =
  runLoggingConcurrent (myLogger useTime (botDebugFlags botin) [fp | LogFlag fp <- botLogFlags botin]) mlog
{-# INLINE runMyLogging #-}

-- | Run a logging action with a queue for logging.
-- Typically you should only run this function once, for example
--
-- main = runLogging myLogger $ do
--   ...
runLoggingConcurrent :: MonadIO m => (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> LoggingT m a -> m a
runLoggingConcurrent logger logm = do
  queue <- liftIO $ newTBQueueIO 100
  liftIO $ forkIO $ concurrentLogger logger queue
  runLoggingT logm (\loc src level msg -> atomically $ writeTBQueue queue (loc, src, level, msg))

-- | The logger that runs in a separate thread, reading from the queue and calls myLogger.
concurrentLogger :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> TBQueue LogLine -> IO ()
concurrentLogger logger queue = do
  (logLoc, logSource, logLevel, logStr) <- atomically $ readTBQueue queue
  logger logLoc logSource logLevel logStr
  concurrentLogger logger queue
