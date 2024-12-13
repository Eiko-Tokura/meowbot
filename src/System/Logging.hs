{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- | Log information, determined by the location, source, level and message.
-- it will both print to the console and write to a file.
myLogger :: [DebugFlag] -> [FilePath] -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
myLogger [] fps (Loc fn pkg mod locS locE) src LevelDebug msg = return ()
myLogger _  fps (Loc fn pkg mod locS locE) src LevelDebug msg = do
  let log = fromLogStr $ "[DEBUG] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger _ fps (Loc fn pkg mod locS locE) src LevelInfo msg = do
  let log = fromLogStr $ "[INFO] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger _ fps (Loc fn pkg mod locS locE) src LevelWarn msg = do
  let log = fromLogStr $ "[WARN] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger _ fps (Loc fn pkg mod locS locE) src LevelError msg = do
  let log = fromLogStr $ "[ERROR] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log
myLogger [] fps (Loc fn pkg mod locS locE) src (LevelOther lv) msg = return ()
myLogger _ fps (Loc fn pkg mod locS locE) src (LevelOther lv) msg = do
  let log = fromLogStr $ "[OTHER] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  mapM_ (`B.appendFile` log) fps
  B.putStr log

runMyLogging :: BotInstance -> LoggingT IO a -> IO a
runMyLogging botin mlog = 
  runLoggingConcurrent (myLogger (botDebugFlags botin) [fp | LogFlag fp <- botLogFlags botin]) mlog

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

