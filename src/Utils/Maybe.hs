module Utils.Maybe where

import Control.Monad.Trans.Maybe

guardMaybeT :: Monad m => Bool -> MaybeT m ()
guardMaybeT True  = pure ()
guardMaybeT False = MaybeT $ pure Nothing

invertMaybe_ :: Maybe a -> Maybe ()
invertMaybe_ Nothing = Just ()
invertMaybe_ _       = Nothing
{-# INLINE invertMaybe_ #-}
