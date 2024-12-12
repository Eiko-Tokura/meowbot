{-# LANGUAGE TupleSections, DerivingVia #-}
module Control.Monad.Trans.ReaderState
  ( module Control.Monad.Reader
  , module Control.Monad.State
  , ReaderStateT(..)
  , pullback
  , globalize
  , pureMaybe
  , onlyState
  , onlyStateT
  , restrictState
  , restrictRead
  ) where

import Data.Bifunctor
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Monoid

-- a mix of reader and state monad transformer

-- | A ReaderStateT r s m a value represents a computation that has a read-only environment of type r, a state of type s, and a result of type a.
newtype ReaderStateT r s m a = ReaderStateT { runReaderStateT :: r -> s -> m (a, s) }
  deriving (Semigroup, Monoid) via (Ap (ReaderStateT r s m) a)

instance Functor m => Functor (ReaderStateT r s m) where
  fmap f v = ReaderStateT $ \r s -> first f <$> runReaderStateT v r s
  {-# INLINE fmap #-}

instance Monad m => Applicative (ReaderStateT r s m) where
  pure a = ReaderStateT $ \_ s -> pure (a, s)
  f <*> v = ReaderStateT $ \r s -> do
    (g, s') <- runReaderStateT f r s
    (a, s'') <- runReaderStateT v r s'
    return (g a, s'')
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad m => Monad (ReaderStateT r s m) where
  v >>= f = ReaderStateT $ \r s -> do
    (a, s') <- runReaderStateT v r s
    runReaderStateT (f a) r s'
  {-# INLINE (>>=) #-}

instance MonadTrans (ReaderStateT r s) where
  lift v = ReaderStateT $ \_ s -> (,s) <$> v
  {-# INLINE lift #-}

instance Monad m => MonadState s (ReaderStateT r s m) where
  state f = ReaderStateT $ \_ s -> return (f s)
  {-# INLINE state #-}

instance Monad m => MonadReader r (ReaderStateT r s m) where
  ask = ReaderStateT $ \r s -> return (r, s)
  {-# INLINE ask #-}

  local f v = ReaderStateT $ \r s -> runReaderStateT v (f r) s
  {-# INLINE local #-}

pullback :: Monad m => (r -> r') -> ReaderStateT r' s m a -> ReaderStateT r s m a
pullback f v = ReaderStateT $ \r s -> runReaderStateT v (f r) s
{-# INLINE pullback #-}

globalize :: Monad m => (all -> r) -> (all -> s) -> (r -> s -> all) -> ReaderStateT r s m a -> StateT all m a
globalize readOnly stateOnly createAll v = StateT $ \all -> do
  let r = readOnly all
      s = stateOnly all
  (a, s') <- runReaderStateT v r s
  return (a, createAll r s')
{-# INLINE globalize #-}

restrictState :: Monad m => (s -> s', (s' -> m s') -> s -> m s) -> ReaderStateT r s' m a -> ReaderStateT r s m a
restrictState (stateProjection, stateEmbedding) mrs' = ReaderStateT $ \r s -> do
  let s0' = stateProjection s
      smallStateDynamics = fmap snd . runReaderStateT mrs' r
  (a, _) <- runReaderStateT mrs' r s0'
  s1 <- stateEmbedding smallStateDynamics s
  return (a, s1)
{-# INLINE restrictState #-}

restrictRead :: Monad m => (r -> r') -> ReaderStateT r' s m a -> ReaderStateT r s m a
restrictRead f v = ReaderStateT $ \r s -> runReaderStateT v (f r) s
{-# INLINE restrictRead #-}

pureMaybe :: Monad m => Maybe a -> MaybeT m a
pureMaybe = MaybeT . return
{-# INLINE pureMaybe #-}

onlyState :: Monad m => (s -> (a, s)) -> ReaderStateT r s m a
onlyState = onlyStateT . (return .)
{-# INLINE onlyState #-}

onlyStateT :: Monad m => (s -> m (a, s)) -> ReaderStateT r s m a
onlyStateT f = ReaderStateT $ \_ s -> f s
{-# INLINE onlyStateT #-}

instance MonadIO m => MonadIO (ReaderStateT r s m) where
  liftIO v = ReaderStateT $ \_ s -> (,s) <$> liftIO v
  {-# INLINE liftIO #-}
