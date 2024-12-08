module Control.Monad.Readable where

-- | A MoandReader class that provides a single function 'query' to read r.
-- the point is that we can have multiple instances of MonadReadable r for the same m.
class Monad m => MonadReadable r m where
  {-# MINIMAL query #-}
  query :: m r

  queries :: (r -> a) -> m a
  queries f = f <$> query
  {-# INLINE queries #-}
