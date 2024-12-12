{-# LANGUAGE TypeFamilies, DataKinds, AllowAmbiguousTypes #-}
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
module System where

import Module
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Data.Kind
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

class ModuleData r (s :: Type) (a :: [Type]) where
  data AllModuleLocalStates  a :: Type
  data AllModuleGlobalStates a :: Type
  --type AllStates a s :: Type

instance ModuleData r s '[] where
  data AllModuleLocalStates  '[] = ModuleLocalStatesNil
  data AllModuleGlobalStates '[] = ModuleGlobalStatesNil
  --type AllStates '[] s = (AllModuleLocalStates '[], s)

instance (ModuleData r s ms, MeowModule r s m) => ModuleData r s (m ': ms) where
  data AllModuleLocalStates  (m ': ms) = ModuleLocalStatesCons  (ModuleLocalState m)  (AllModuleLocalStates ms)
  data AllModuleGlobalStates (m ': ms) = ModuleGlobalStatesCons (ModuleGlobalState m) (AllModuleGlobalStates ms)
  --type AllStates (m ': ms) s = (AllModuleLocalStates (m ': ms), s)

class (MeowModule r s mod, ModuleData r s mods) => ModuleOperable r s mod mods where
  projectionToOneModuleL :: AllModuleLocalStates mods -> ModuleLocalState mod

  projectionToOneModuleG :: AllModuleGlobalStates mods -> ModuleGlobalState mod

  modifyOneModule :: Monad m => (ModuleLocalState mod -> m (ModuleLocalState mod)) -> AllModuleLocalStates mods -> m (AllModuleLocalStates mods)

  modifyOneModuleAndState :: Monad m =>
    ((ModuleLocalState mod, s) -> m (ModuleLocalState mod, s)) ->
    (AllModuleLocalStates mods, s) -> m (AllModuleLocalStates mods, s)

  moduleEnv :: Monad m => ModuleT r s mod m a -> SystemT r s mods m a

-- | The base case
instance {-# OVERLAPPING #-} (MeowModule r s mod, ModuleData r s (mod ': xs)) => ModuleOperable r s mod (mod ': xs) where
  projectionToOneModuleL (ModuleLocalStatesCons x _) = x
  {-# INLINE projectionToOneModuleL #-}

  projectionToOneModuleG (ModuleGlobalStatesCons x _) = x
  {-# INLINE projectionToOneModuleG #-}

  modifyOneModule f (ModuleLocalStatesCons x xs) = (`ModuleLocalStatesCons` xs) <$> (f x) 
  {-# INLINE modifyOneModule #-}

  modifyOneModuleAndState f (ModuleLocalStatesCons x xs, s) = do
    (x', s') <- f (x, s)
    return (ModuleLocalStatesCons x' xs, s')
  {-# INLINE modifyOneModuleAndState #-}

  moduleEnv = restrictRead (first $ projectionToOneModuleG @r @s @mod) . restrictState
    ( first (projectionToOneModuleL @r @s @mod)
    , modifyOneModuleAndState @r @s @mod
    )
  {-# INLINE moduleEnv #-}

-- | The inductive case
instance {-# OVERLAPPABLE #-}
  ( MeowModule r s mod
  , ModuleData r s (x ': xs)
  , ModuleOperable r s mod xs
  ) 
  => ModuleOperable r s mod (x ': xs) 
  where
  projectionToOneModuleL (ModuleLocalStatesCons _ xs) = projectionToOneModuleL @r @s @mod xs
  {-# INLINE projectionToOneModuleL #-}

  projectionToOneModuleG (ModuleGlobalStatesCons _ xs) = projectionToOneModuleG @r @s @mod xs
  {-# INLINE projectionToOneModuleG #-}

  modifyOneModule f (ModuleLocalStatesCons x xs) = ModuleLocalStatesCons x <$> modifyOneModule @r @s @mod f xs
  {-# INLINE modifyOneModule #-}

  modifyOneModuleAndState f (ModuleLocalStatesCons x xs, s) = do
    (xs', s') <- modifyOneModuleAndState @r @s @mod f (xs, s)
    return $ (ModuleLocalStatesCons x xs', s')
  {-# INLINE modifyOneModuleAndState #-}

  moduleEnv = restrictRead (first $ projectionToOneModuleG @r @s @mod) . restrictState
    ( first (projectionToOneModuleL @r @s @mod)
    , modifyOneModuleAndState @r @s @mod
    )
  {-# INLINE moduleEnv #-}

