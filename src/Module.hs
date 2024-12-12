{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
-- | Author: Eiko chan >w<
-- In this module we define what a meow-module is.
--
-- we want to make things modular, plug-and-play.
module Module where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Data.Proxy

type ModuleT r s l m a = ReaderStateT r (ModuleState l, s) m a

-- | The Module class.
-- For example log messages into database, or adding WS proxy support.
--
-- the default quitModule, beforeMeow, afterMeow does nothing. Override them to provide the functionality.
class MeowModule r s a | a -> r, a -> s where
  {-# MINIMAL initModule #-}

  -- | The state of the module, for example database connection pool.
  type ModuleState a

  -- | The function to initialize the module.
  initModule :: MonadLoggerIO m => Proxy a -> r -> m (ModuleState a)

  -- | Quit the module.
  quitModule :: MonadLoggerIO m => Proxy a -> ModuleT r s a m ()
  quitModule _ = return ()
  {-# INLINE quitModule #-}

  -- | The function to run before the meow bot main logic each time a message is received.
  beforeMeow :: MonadLoggerIO m => Proxy a -> ModuleT r s a m ()
  beforeMeow _ = return ()
  {-# INLINE beforeMeow #-}

  -- | The function to run after the meow bot main logic each time a message is received.
  afterMeow  :: MonadLoggerIO m => Proxy a -> ModuleT r s a m ()
  afterMeow _ = return ()
  {-# INLINE afterMeow #-}

  -- | Get the module state.
  readModuleState :: MonadLoggerIO m => Proxy a -> ModuleT r s a m (ModuleState a)
  readModuleState _ = gets fst
  {-# INLINE readModuleState #-}

  -- | Modify the module state.
  modifyModuleState :: MonadLoggerIO m => Proxy a -> (ModuleState a -> ModuleState a) -> ModuleT r s a m ()
  modifyModuleState _ f = modify $ \(ms, s) -> (f ms, s)
  {-# INLINE modifyModuleState #-}

