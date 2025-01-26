{-# LANGUAGE UndecidableInstances, FlexibleInstances, OverloadedStrings, MultiParamTypeClasses #-}
module Control.Monad.ExceptionReturn
  ( ToText(..)
  , ExceptionReturn(..)
  , module Control.Monad.Trans.Except
  ) where

-- | Author: Eiko
-- this package creates modular and reusable functions that deals errors more easily,
-- you can use different error producing functions Either, Maybe, [] all embedded in ExceptT

import Data.Text (Text, append)
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Utils.ToText

class ExceptionReturn f emsg where
  -- | The only function you need to implement to make a new instance of UniversalExceptionReturn
  -- the fype f means a exception capable result wrapper, emsg means the error message type
  conversionToEither
    :: Maybe emsg           -- ^ the message to prepend to the error message
    -> f a                  -- ^ the result wrapper
    -> Either emsg a        -- ^ the result wrapper converted to Either
  {-# MINIMAL conversionToEither #-}

  -- | Wrap a effectul f a data into an ExceptT emsg m monad
  effectE :: Monad m => m (f a) -> ExceptT emsg m a
  effectE = ExceptT . fmap (conversionToEither Nothing)
  {-# INLINE effectE #-}

  -- | Wrap a pure f a data into an ExceptT emsg m monad
  pureE :: Monad m => f a -> ExceptT emsg m a
  pureE = effectE . return
  {-# INLINE pureE #-}

  -- | Wrap a effectul f a data with given function into an ExceptT emsg m monad
  effectEWith :: Monad m => (f a -> emsg) -> m (f a) -> ExceptT emsg m a
  effectEWith msgf mfa = ExceptT $ do
    fa <- mfa
    return $ conversionToEither (Just $ msgf fa) fa
  {-# INLINE effectEWith #-}

  -- | Wrap a pure f a data with given function into an ExceptT emsg m monad
  pureEWith :: Monad m => (f a -> emsg) -> f a -> ExceptT emsg m a
  pureEWith msgf = effectEWith msgf . return
  {-# INLINE pureEWith #-}

  -- | Wrap a effectul f a data with given message prepended, into an ExceptT emsg m monad
  effectEMsg :: Monad m => emsg -> m (f a) -> ExceptT emsg m a
  effectEMsg msg = effectEWith (const msg)
  {-# INLINE effectEMsg #-}

  -- | Wrap a pure f a data with given message prepended, into an ExceptT emsg m monad
  pureEMsg :: Monad m => emsg -> f a -> ExceptT emsg m a
  pureEMsg msg = effectEMsg msg . return
  {-# INLINE pureEMsg #-}

instance ToText e => ExceptionReturn (Either e) Text where
  conversionToEither Nothing     = first toText
  conversionToEither (Just msg)  = first $ (msg `append`) . toText
  {-# INLINE conversionToEither #-}

instance ExceptionReturn Maybe Text where
  conversionToEither Nothing     = maybe (Left "Returned Nothing") Right
  conversionToEither (Just msg)  = maybe (Left msg) Right
  {-# INLINE conversionToEither #-}

instance ExceptionReturn [] Text where
  conversionToEither Nothing []    = Left "Returned Empty List"
  conversionToEither Nothing xs    = Right (head xs)
  conversionToEither (Just msg) [] = Left msg
  conversionToEither (Just _) xs   = Right (head xs)
  {-# INLINE conversionToEither #-}

