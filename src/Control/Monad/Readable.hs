module Control.Monad.Readable where

class Monad m => MonadReadable r m where
  {-# MINIMAL readable #-}
  readable :: m r

  readables :: (r -> a) -> m a
  readables f = f <$> readable
  {-# INLINE readables #-}
