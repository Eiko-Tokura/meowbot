{-# LANGUAGE DerivingVia, UndecidableInstances, LinearTypes #-}
module Control.Monad.Effect where

import Control.Monad.Trans.ReaderState
import Data.HList
import Data.Bifunctor
import Data.Kind
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Logger
import Control.Monad.Error.Class
import Database.Persist.Sqlite
import Data.Pool
import Data.Text (Text)

-- | A monad transformer managing read, state, and error effects (error without losing state)
newtype RSET r s e m a = RSET { runRSET :: r -> s -> m (Either e a, s) }

-- | Effectful computation, using modules as units of effect
newtype Eff mods a = Eff { runEff :: RSET (SystemRead mods) (SystemState mods) (SystemError mods) IO a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader (SystemRead mods)
    , MonadState (SystemState mods)
    , MonadError (SystemError mods)
    )

-- | embed smaller effect into larger effect
embedEff :: forall mods mods' a. (EmbedSubList mods mods', SubList mods mods') => Eff mods a -> Eff mods' a
embedEff eff = Eff $ RSET $ \rs' ss' -> do
  let rs = getSubListF @mods rs'
      ss = getSubListF @mods ss'
      modsEff = runRSET $ runEff eff
  (emods, ss1) <- modsEff rs ss
  return (first embedSubList emods, subListUpdateF ss' ss1)
{-# INLINE embedEff #-}

-------------------------------------- instances --------------------------------------

instance Monad m => MonadState ss (RSET rs ss es m) where
  get = RSET $ \_ ss -> return (Right ss, ss)
  {-# INLINE get #-}
  put ss = RSET $ \_ _ -> return (Right (), ss)
  {-# INLINE put #-}

instance Monad m => MonadError es (RSET rs ss es m) where
  throwError es  = RSET $ \_ ss -> return (Left es, ss)
  {-# INLINE throwError #-}
  catchError m h = RSET $ \rs ss -> do
    (a, ss')  <- runRSET m rs ss
    case a of
      Left es -> runRSET (h es) rs ss'
      Right a -> return  (Right a, ss')
  {-# INLINE catchError #-}

instance Monad m => MonadReader rs (RSET rs ss es m) where
  ask = RSET $ \rs ss -> return (Right rs, ss)
  {-# INLINE ask #-}
  local f m = RSET $ \rs ss -> runRSET m (f rs) ss
  {-# INLINE local #-}

instance Functor m => Functor (RSET rs ss es m) where
  fmap f m = RSET $ \rs ss -> fmap (\(a, ss') -> (f <$> a, ss')) $ runRSET m rs ss
  {-# INLINE fmap #-}

instance Monad m => Applicative (RSET rs ss es m) where
  pure a = RSET $ \_ ss -> pure (Right a, ss)
  {-# INLINE pure #-}
  f <*> a = RSET $ \rs ss -> do
    (f', ss')  <- runRSET f rs ss
    (a', ss'') <- runRSET a rs ss'
    return (f' <*> a', ss'')
  {-# INLINE (<*>) #-}

instance Monad m => Monad (RSET rs ss es m) where
  m >>= k = RSET $ \rs ss -> do
    (a, ss') <- runRSET m rs ss
    case a of
      Left es -> return (Left es, ss')
      Right a -> runRSET (k a) rs ss'
  {-# INLINE (>>=) #-}

instance MonadTrans (RSET rs ss es) where
  lift m = RSET $ \_ ss -> do
    a <- m
    return (Right a, ss)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (RSET rs ss es m) where
  liftIO m = RSET $ \_ ss -> do
    a <- liftIO m
    return (Right a, ss)
  {-# INLINE liftIO #-}

------------------------------------------ Module: unit for effect ------------------------------------------

class Module mod where
  data ModuleInitData mod :: Type
  data ModuleRead     mod :: Type
  data ModuleState    mod :: Type
  data ModuleError    mod :: Type
  data ModuleEvent    mod :: Type

type SystemInitData mods = FList ModuleInitData mods
type SystemState    mods = FList ModuleState    mods
type SystemRead     mods = FList ModuleRead     mods
type SystemError    mods = UList ModuleError    mods
type SystemEvent    mods = UList ModuleEvent    mods

-- | Specifies that the module can load after mods are loaded
-- in practice we could use 
-- instance SomeModuleWeNeed `In` mods => Loadable mods SomeModuleToLoad
class Loadable mod mods where
  initModule  :: ModuleInitData mod -> Eff mods (Either (ModuleError mod) (ModuleRead mod, ModuleState mod))

  beforeEvent :: Eff (mod : mods) ()
  beforeEvent = return ()
  {-# INLINE beforeEvent #-}

  afterEvent  :: Eff (mod : mods) ()
  afterEvent = return ()
  {-# INLINE afterEvent #-}

  moduleEvent :: Eff (mod : mods) (STM (ModuleEvent mod))
  moduleEvent = return empty
  {-# INLINE moduleEvent #-}

  handleEvent :: ModuleEvent mod -> Eff (mod : mods) ()
  handleEvent _ = return ()
  {-# INLINE handleEvent #-}

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
class System mods where
  initAllModules :: SystemInitData mods -> IO (Either (SystemError mods) (SystemRead mods, SystemState mods))

  listenToEvents :: Eff mods (STM (SystemEvent mods))

  handleEvents :: SystemEvent mods -> Eff mods ()

  beforeSystem :: Eff mods ()

  afterSystem  :: Eff mods ()

-- | base case for system
instance System '[] where
  initAllModules _ = return $ Right (FNil, FNil)
  {-# INLINE initAllModules #-}

  listenToEvents = return empty
  {-# INLINE listenToEvents #-}

  handleEvents _ = return ()
  {-# INLINE handleEvents #-}

  beforeSystem = return ()
  {-# INLINE beforeSystem #-}

  afterSystem = return ()
  {-# INLINE afterSystem #-}

-- | Inductive instance for system
instance (Module mod, System mods, Loadable mod mods) => System (mod ': mods) where
  initAllModules (x :** xs) = do
    initAllModules xs >>= \case
      Right (rs, ss) -> do
        (er, ss') <- runRSET (runEff @mods $ initModule @mod x) rs ss
        case er of
          Right (Right (r', s')) -> return $ Right (r' :** rs, s' :** ss')
          Right (Left e')        -> return $ Left $ UHead e'
          Left es                -> return $ Left $ UTail es
      Left es -> return $ Left $ UTail es
  {-# INLINE initAllModules #-}
  
  beforeSystem = do
    embedEff $ beforeSystem @mods
    beforeEvent @mod
  {-# INLINE beforeSystem #-}

  afterSystem = do
    embedEff $ afterSystem @mods
    afterEvent @mod
  {-# INLINE afterSystem #-}

  listenToEvents = do
    tailEvents <- embedEff $ listenToEvents @mods
    headEvent  <- moduleEvent @mod
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = handleEvent @mod x
  handleEvents (UTail xs) = embedEff $ handleEvents @mods xs
  {-# INLINE handleEvents #-}

--------------------------------------------------------------------------------------------------

data Logger = Logger { runLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO () }
instance Module Logger where
  data ModuleInitData Logger = LoggerInitData Logger
  data ModuleRead     Logger = LoggerRead { logger :: Logger }
  data ModuleState    Logger = LoggerState
  data ModuleEvent    Logger = LoggerEvent
  data ModuleError    Logger = LoggerError deriving Show

instance Loadable Logger mods where
  initModule (LoggerInitData logger) = return $ Right (LoggerRead logger, LoggerState)
  {-# INLINE initModule #-}

instance Logger `In` mods => MonadLogger (Eff mods) where
  monadLoggerLog loc logsource loglevel msg = do
    LoggerRead logger <- asks (getF @Logger)
    liftIO $ runLogger logger loc logsource loglevel (toLogStr msg)
  {-# INLINE monadLoggerLog #-}

instance Logger `In` mods => MonadLoggerIO (Eff mods) where
  askLoggerIO = asks (runLogger . logger . getF @Logger)
  {-# INLINE askLoggerIO #-}

--------------------------------------------------------------------------------------------------

data Database = Database
instance Module Database where
  data ModuleInitData Database = DatabaseInitData { dbPath :: Text, dbMigration :: Migration, dbPoolSize :: Int }
  data ModuleRead     Database = DatabaseRead { dbPool :: Pool SqlBackend }
  data ModuleState    Database = DatabaseState
  data ModuleEvent    Database = DatabaseEvent
  data ModuleError    Database = DatabaseError deriving Show

instance Logger `In` mods => Loadable Database mods where
  initModule (DatabaseInitData path migration poolSize) = do
    logger <- askLoggerIO
    pool <- liftIO $ runLoggingT 
      ( do
          pool <- createSqlitePool path poolSize
          runMigration migration `runSqlPool` pool
          return pool
      ) logger
    return $ Right (DatabaseRead pool, DatabaseState)
  {-# INLINE initModule #-}

class RunDB m where
  runDB :: ReaderT SqlBackend IO a -> m a

instance Database `In` mods => RunDB (Eff mods) where
  runDB action = do
    DatabaseRead pool <- asks (getF @Database)
    liftIO $ runSqlPool action pool
  {-# INLINE runDB #-}
