{-# LANGUAGE TypeFamilies, DataKinds, AllowAmbiguousTypes, UndecidableInstances #-}
-- | This module defines the system type, and the type classes that are used to define the data structure of the system.
--
-- * We need a data family associated to (mods, s)
--
-- given modules [m1, m2, m3, ...]
--
-- it will generate a data type isomorphic to (m1, m2, m3, ..., s)
--
-- * alternatively: we can use template haskell to generate the data type.
--
-- * another way to do this is to use typeable, and create a map together with some dynamic type wrapper wrapping different MonadState of different modules.
--
-- which is conceptually simpler.
--
-- We will use the first method, although a bit hard, it is the most type-safe way!
module System
  ( SystemT
  , Modules(..)
  , AllModuleLocalStates
  , AllModuleEarlyLocalStates
  , AllModuleGlobalStates
  , AllModuleEvents
  , AllModuleInitDataG
  , AllModuleInitDataL
  , module Control.Monad.Logger
  , module Module
  ) where

import Module
import Control.Applicative
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Concurrent.STM
import Data.HList
import Data.Bifunctor
-- | the system type
--
-- * @r@ is the global state of the system, the global state is shared among all modules and is read-only.
--   if you want to modify the global state, use TVar.
--
-- * @s@ is the local state of the system, the local state is per-instance and is modifiable in the monad.
--
-- * @mods@ is the list of modules available in the system, it has kind [Type].
--
-- * @m@ is the monad, it is usually IO.
--
-- * @a@ is the result type.
type SystemT r s mods m a = ReaderStateT (AllModuleGlobalStates mods, r) (AllModuleLocalStates mods, s) (LoggingT m) a

type AllModuleInitDataG    mods = FList ModuleInitDataG   mods
type AllModuleInitDataL    mods = FList ModuleInitDataL   mods
type AllModuleGlobalStates mods = FList ModuleGlobalState mods
type AllModuleLocalStates  mods = FList ModuleLocalState  mods
type AllModuleEarlyLocalStates mods = FList ModuleEarlyLocalState mods
type AllModuleEvents       mods = UList ModuleEvent       mods

-- handleEvents :: (Modules mods, MonadIO m) => AllModuleEvents mods -> SystemT r s mods m ()

class Modules r s mods where
  listenToEvents    :: SystemT r s mods IO (STM (AllModuleEvents mods))
  handleEvents      :: AllModuleEvents mods    -> SystemT r s mods IO ()
  initAllModulesG   :: AllModuleInitDataG mods -> LoggingT IO (AllModuleGlobalStates mods)
  initAllModulesL   :: r -> AllModuleInitDataG mods -> AllModuleInitDataL mods -> AllModuleEarlyLocalStates mods -> LoggingT IO (AllModuleLocalStates mods)
  initAllModulesEL  :: AllModuleInitDataG mods -> AllModuleInitDataL mods -> LoggingT IO (AllModuleEarlyLocalStates mods)
  beforeMeowActions :: SystemT r s mods IO ()
  afterMeowActions  :: SystemT r s mods IO ()

instance Modules r s '[] where
  listenToEvents = return $ empty -- ^ should block forever
  {-# INLINE listenToEvents #-}
  handleEvents UNil = return ()
  {-# INLINE handleEvents #-}
  initAllModulesG _ = return FNil
  {-# INLINE initAllModulesG #-}
  initAllModulesL _ _ _ _ = return FNil
  {-# INLINE initAllModulesL #-}
  initAllModulesEL _ _ = return FNil
  {-# INLINE initAllModulesEL #-}
  beforeMeowActions = return ()
  {-# INLINE beforeMeowActions #-}
  afterMeowActions = return ()
  {-# INLINE afterMeowActions #-}

moduleEnv :: (mod `In` mods, Monad m) => ModuleT r s mod m a -> SystemT r s mods m a
moduleEnv = ReaderStateT
  . (\arrow (allGlob, r) (allLoc, s) ->
      let locmod  = getF allLoc
          globmod = getF allGlob
      in second (first $ \l -> modifyF (const l) allLoc) <$> arrow (globmod, r) (locmod, s)
    )
  . runReaderStateT
{-# INLINE moduleEnv #-}

inductiveEnv :: Monad m => SystemT r s mods m a -> SystemT r s (mod ': mods) m a
inductiveEnv = ReaderStateT
  . (\arrowms (_ :** allGlobms, r) (!locm :** allLocms, s) ->
      second (first (locm :**)) <$> arrowms (allGlobms, r) (allLocms, s)
    )
  . runReaderStateT
{-# INLINE inductiveEnv #-}

instance (MeowModule r s mod, Modules r s mods) => Modules r s (mod ': mods) where

  listenToEvents = do
    tailEvents <- inductiveEnv $ listenToEvents @r @s @mods
    headEvent <- moduleEnv $ moduleEvent @r @s @mod (Proxy @mod)
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = do
    moduleEnv $ moduleEventHandler (Proxy @mod) x
  handleEvents (UTail xs) =
    inductiveEnv $ handleEvents @r @s @mods xs
  {-# INLINE handleEvents #-}

  initAllModulesG (init :** inits) = do
    mg   <- initModule @r @s @mod (Proxy @mod) init
    mgs  <- initAllModulesG @r @s inits
    return (mg :** mgs)
  {-# INLINE initAllModulesG #-}

  initAllModulesL r (initG :** initGs) (initL :** initLs) (initEL :** initELs) = do
    ml   <- initModuleLocal @r @s @mod (Proxy @mod) r initG initL initEL
    mls  <- initAllModulesL @r @s r initGs initLs initELs
    return (ml :** mls)
  {-# INLINE initAllModulesL #-}

  initAllModulesEL (initG :** initGs) (initL :** initLs) = do
    ml   <- initModuleEarlyLocal @r @s @mod (Proxy @mod) initG initL
    mels <- initAllModulesEL @r @s initGs initLs
    return (ml :** mels)
  {-# INLINE initAllModulesEL #-}

  beforeMeowActions = do
    moduleEnv $ beforeMeow @r @s @mod (Proxy @mod)
    inductiveEnv $ beforeMeowActions @r @s @mods
  {-# INLINE beforeMeowActions #-}

  afterMeowActions = do
    moduleEnv $ afterMeow @r @s @mod (Proxy @mod)
    inductiveEnv $ afterMeowActions @r @s @mods
  {-# INLINE afterMeowActions #-}
