{-# LANGUAGE OverloadedStrings #-}
module System.Logging
  ( module Control.Monad.Logger
  , myLogger
  , runLoggingConcurrent
  ) where

import Control.Monad.Logger
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.ByteString as B

-- | Log information, determined by the location, source, level and message.
-- it will both print to the console and write to a file.
myLogger :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
myLogger fp (Loc fn pkg mod locS locE) src LevelDebug msg = do
  let log = fromLogStr $ "[DEBUG] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  B.appendFile fp log
  B.putStr log
myLogger fp (Loc fn pkg mod locS locE) src LevelInfo msg = do
  let log = fromLogStr $ "[INFO] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  B.appendFile fp log
  B.putStr log
myLogger fp (Loc fn pkg mod locS locE) src LevelWarn msg = do
  let log = fromLogStr $ "[WARN] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  B.appendFile fp log
  B.putStr log
myLogger fp (Loc fn pkg mod locS locE) src LevelError msg = do
  let log = fromLogStr $ "[ERROR] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  B.appendFile fp log
  B.putStr log
myLogger fp (Loc fn pkg mod locS locE) src (LevelOther lv) msg = do
  let log = fromLogStr $ "[OTHER] " <> toLogStr mod <> " : " <> toLogStr src <> " " <> msg <> "\n"
  B.appendFile fp log
  B.putStr log

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

