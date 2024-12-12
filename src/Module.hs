{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
-- | Author: Eiko chan >w<
-- In this module we define what a meow-module is.
--
-- we want to make things modular, plug-and-play.
module Module 
  ( Proxy(..)
  , MeowModule(..)
  , ModuleT
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Concurrent.STM
import Data.Proxy

-- | The Module type.
--
-- r is the global state of the system, the global state is shared among all modules and is read-only.
--
-- s is the **local state** of the system, the local state is **per-instance** and is modifiable in the monad.
--
-- besides the extra global state r and local state s,
--
-- each module will also have it's own global state and local state.
type ModuleT r s l m a = ReaderStateT (ModuleGlobalState l, r) (ModuleLocalState l, s) (LoggingT m) a

-- | The Module class.
-- For example log messages into database, or adding WS proxy support.
--
-- the default quitModule, beforeMeow, afterMeow does nothing. Override them to provide the functionality.
class MeowModule r s a | a -> s where
  {-# MINIMAL initModule, initModuleLocal #-}

  -- | The state of the module, local states are iniitalized for each instance
  -- for example the Proxy Websocket connection.
  type ModuleLocalState a
  -- | Global state is shared among all bot instances for example database connection pool.
  -- since it has to be shared among threads, it cannot be put into StateT part
  -- therefore it has to be read-only. Use TVar if you want to modify it.
  type ModuleGlobalState a

  -- | The event type of the module, for example the event of a new message received.
  type ModuleEvent a
  type ModuleEvent a = ()

  -- | The function to initialize the module, return the read-only global state.
  initModule :: Proxy a -> r -> LoggingT IO (ModuleGlobalState a)

  -- | The function to initialize the module local state in each instance / thread.
  initModuleLocal :: Proxy a -> r -> LoggingT IO (ModuleLocalState a)

  -- | Quit the module.
  quitModule :: Proxy a -> ModuleT r s a IO ()
  quitModule _ = return ()
  {-# INLINE quitModule #-}

  -- | The function to run before the meow bot main logic each time a message is received.
  beforeMeow :: Proxy a -> ModuleT r s a IO ()
  beforeMeow _ = return ()
  {-# INLINE beforeMeow #-}

  -- | The function to run after the meow bot main logic each time a message is received.
  afterMeow  :: Proxy a -> ModuleT r s a IO ()
  afterMeow _ = return ()
  {-# INLINE afterMeow #-}

  -- | Get the module local state.
  readModuleStateL :: Monad m => Proxy a -> ModuleT r s a m (ModuleLocalState a)
  readModuleStateL _ = gets fst
  {-# INLINE readModuleStateL #-}

  -- | Get the module global state.
  readModuleStateG :: Monad m => Proxy a -> ModuleT r s a m (ModuleGlobalState a)
  readModuleStateG _ = asks fst
  {-# INLINE readModuleStateG #-}

  -- | Modify the module local state.
  modifyModuleState :: Monad m => Proxy a -> (ModuleLocalState a -> ModuleLocalState a) -> ModuleT r s a m ()
  modifyModuleState _ f = modify $ \(ms, s) -> (f ms, s)
  {-# INLINE modifyModuleState #-}

  -- | Listen to the module event. The default implementation is to return an empty STM, always blocking.
  moduleEvent :: Proxy a -> ModuleT r s a IO (STM (ModuleEvent a))
  moduleEvent _ = return $ empty
  {-# INLINE moduleEvent #-}

  -- | Handle the module event. The default implementation is to do nothing.
  moduleEventHandler :: Proxy a -> ModuleEvent a -> ModuleT r s a IO ()
  moduleEventHandler _ _ = return ()
  {-# INLINE moduleEventHandler #-}
