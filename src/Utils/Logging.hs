module Utils.Logging
  ( module Control.Monad.Logger
  , logThroughCont
  , logForkFinally
  , logCatch
  , useLoggerInExceptT
  , unsafeInterleaveLoggingTIO
  ) where

import Control.Monad.Logger
import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import System.IO.Unsafe (unsafeInterleaveIO)

logThroughCont :: ((a -> IO b) -> IO c) -> (a -> LoggingT IO b) -> LoggingT IO c
logThroughCont cont logarr = do
  logger <- askLoggerIO
  liftIO $ cont $ \a -> runLoggingT (logarr a) logger
{-# INLINE logThroughCont #-}

logForkFinally :: LoggingT IO a -> (Either SomeException a -> LoggingT IO ()) -> LoggingT IO ThreadId
logForkFinally action and_then = do
  logger <- askLoggerIO
  liftIO $ forkFinally (runLoggingT action logger) (flip runLoggingT logger . and_then)
{-# INLINE logForkFinally #-}

logCatch :: LoggingT IO a -> (SomeException -> LoggingT IO a) -> LoggingT IO a
logCatch action handler = do
  logger <- askLoggerIO
  liftIO $ catch (runLoggingT action logger) (\e -> runLoggingT (handler e) logger)
{-# INLINE logCatch #-}

unsafeInterleaveLoggingTIO :: LoggingT IO a -> LoggingT IO a
unsafeInterleaveLoggingTIO (LoggingT f) = LoggingT $ \r -> unsafeInterleaveIO $ f r
{-# INLINE unsafeInterleaveLoggingTIO #-}

useLoggerInExceptT :: Monad m => (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> ExceptT e (LoggingT m) a -> ExceptT e m a
useLoggerInExceptT logger = ExceptT . flip runLoggingT logger . runExceptT
{-# INLINE useLoggerInExceptT #-}
