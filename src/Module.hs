{-# LANGUAGE TypeFamilies, FunctionalDependencies, AllowAmbiguousTypes #-}
-- | Author: Eiko chan >w<
-- In this module we define what a meow-module is.
--
-- we want to make things modular, plug-and-play.
module Module 
  ( Proxy(..)
  , MeowModule(..)
  , ModuleT
  , HasSystemRead(..)
  ) where

import Control.Applicative
import Control.Monad.Readable
import Control.Monad.State
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Concurrent.STM
import Data.Proxy
import Data.Kind

import Parser.Run
import Parser.Except

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

instance Monad m => MonadReadable s (ReaderStateT (ModuleGlobalState l, r) (ModuleLocalState l, s) m) where
  query = gets snd
  {-# INLINE query #-}
instance Monad m => MonadModifiable s (ReaderStateT (ModuleGlobalState l, r) (ModuleLocalState l, s) m) where
  change f = modify $ \(ms, s) -> (ms, f s)
  {-# INLINE change #-}

class HasSystemRead r' r where
  readSystem :: r -> r'

  askSystem :: MonadReader r m => m r'
  askSystem = asks readSystem
  {-# INLINE askSystem #-}

instance HasSystemRead r' r => HasSystemRead r' (a, r) where
  readSystem = readSystem . snd
  {-# INLINE readSystem #-}

-- | The Module class.
-- For example log messages into database, or adding WS proxy support.
--
-- the default quitModule, beforeMeow, afterMeow does nothing. Override them to provide the functionality.
class MeowModule r s mod | mod -> s where
  {-# MINIMAL initModule, initModuleLocal, getInitDataG, getInitDataL #-}

  -- | The initial data of the module, given by the user for example coming from the config file or command line arguments.
  data ModuleInitDataG mod :: Type

  -- | The initial data of the module, per instance for example the Proxy Websocket connection.
  data ModuleInitDataL mod :: Type

  -- | The state of the module, local states are iniitalized for each instance
  -- for example the Proxy Websocket connection.
  data ModuleLocalState mod :: Type

  -- | Global state is shared among all bot instances for example database connection pool.
  -- since it has to be shared among threads, it cannot be put into StateT part
  -- therefore it has to be read-only. Use TVar if you want to modify it.
  data ModuleGlobalState mod :: Type

  -- | The event type of the module, for example the event of a new message received.
  data ModuleEvent mod

  -- | How to get the initalization data from the user, the first element is default value (optional), 
  -- the second element is a command line parser.
  getInitDataG :: Proxy mod -> (Maybe (ModuleInitDataG mod), ParserE [String] String String (ModuleInitDataG mod))

  -- | How to get the initalization data from the user, the first element is default value (optional),
  -- the second element is a command line parser.
  getInitDataL :: Proxy mod -> (Maybe (ModuleInitDataL mod), ParserE [String] String String (ModuleInitDataL mod))

  -- | The function to initialize the module, return the read-only global state.
  initModule :: Proxy mod -> ModuleInitDataG mod -> LoggingT IO (ModuleGlobalState mod)

  -- | The function to initialize the module local state in each instance / thread.
  initModuleLocal :: Proxy mod -> r -> ModuleInitDataG mod -> ModuleInitDataL mod -> LoggingT IO (ModuleLocalState mod)

  -- | Quit the module.
  quitModule :: Proxy mod -> ModuleT r s mod IO ()
  quitModule _ = return ()
  {-# INLINE quitModule #-}

  -- | The function to run before the meow bot main logic each time a message is received.
  beforeMeow :: Proxy mod -> ModuleT r s mod IO ()
  beforeMeow _ = return ()
  {-# INLINE beforeMeow #-}

  -- | The function to run after the meow bot main logic each time a message is received.
  afterMeow  :: Proxy mod -> ModuleT r s mod IO ()
  afterMeow _ = return ()
  {-# INLINE afterMeow #-}

  -- | Get the module local state.
  readModuleStateL :: Monad m => Proxy mod -> ModuleT r s mod m (ModuleLocalState mod)
  readModuleStateL _ = gets fst
  {-# INLINE readModuleStateL #-}

  -- | Get the module global state.
  readModuleStateG :: Monad m => Proxy mod -> ModuleT r s mod m (ModuleGlobalState mod)
  readModuleStateG _ = asks fst
  {-# INLINE readModuleStateG #-}

  -- | Modify the module local state.
  modifyModuleState :: Monad m => Proxy mod -> (ModuleLocalState mod -> ModuleLocalState mod) -> ModuleT r s mod m ()
  modifyModuleState _ f = modify $ \(ms, s) -> (f ms, s)
  {-# INLINE modifyModuleState #-}

  -- | Listen to the module event. The default implementation is to return an empty STM, always blocking.
  moduleEvent :: Proxy mod -> ModuleT r s mod IO (STM (ModuleEvent mod))
  moduleEvent _ = return $ empty
  {-# INLINE moduleEvent #-}

  -- | Handle the module event. The default implementation is to do nothing.
  moduleEventHandler :: Proxy mod -> ModuleEvent mod -> ModuleT r s mod IO ()
  moduleEventHandler _ _ = return ()
  {-# INLINE moduleEventHandler #-}
