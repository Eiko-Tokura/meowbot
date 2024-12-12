module Control.Monad.Readable where

import Control.Monad.Trans

-- | A MoandReader class that provides a single function 'query' to read r.
-- the point is that we can have multiple instances of MonadReadable r for the same m.
class Monad m => MonadReadable r m where
  {-# MINIMAL query #-}
  query :: m r

  queries :: (r -> a) -> m a
  queries f = f <$> query
  {-# INLINE queries #-}

instance {-# OVERLAPPABLE #-} (MonadReadable r m, MonadTrans t) => MonadReadable r (t m) where
  query = lift query
  {-# INLINE query #-}

class Monad m => MonadModifiable s m where
  {-# MINIMAL change #-}
  change :: (s -> s) -> m ()

instance {-# OVERLAPPABLE #-} (MonadModifiable s m, MonadTrans t) => MonadModifiable s (t m) where
  change f = lift $ change f
  {-# INLINE change #-}
