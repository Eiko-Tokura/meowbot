module Utils.Maybe where

boolMaybe :: Bool -> Maybe ()
boolMaybe True  = Just ()
boolMaybe False = Nothing
{-# INLINE boolMaybe #-}

invertMaybe_ :: Maybe a -> Maybe ()
invertMaybe_ Nothing = Just ()
invertMaybe_ _       = Nothing
{-# INLINE invertMaybe_ #-}
