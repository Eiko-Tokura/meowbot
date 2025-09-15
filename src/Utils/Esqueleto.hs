module Utils.Esqueleto where

import Database.Esqueleto.Experimental

type family UnValue a where
  UnValue (Value b) = b
  UnValue (Maybe a) = Maybe (UnValue a)
  UnValue [a]       = [UnValue a]
  UnValue (a, b)    = (UnValue a, UnValue b)
  UnValue (a, b, c) = (UnValue a, UnValue b, UnValue c)

class ToUnValue a where
  toUnValue :: a -> UnValue a

instance ToUnValue (Value a) where toUnValue (Value a) = a; {-# INLINE toUnValue #-}

instance ToUnValue a => ToUnValue (Maybe a) where
  toUnValue Nothing  = Nothing
  toUnValue (Just a) = Just (toUnValue a)
  {-# INLINE toUnValue #-}

instance ToUnValue a => ToUnValue [a] where
  toUnValue = fmap toUnValue
  {-# INLINE toUnValue #-}

instance (ToUnValue a, ToUnValue b) => ToUnValue (a, b) where
  toUnValue (a, b) = (toUnValue a, toUnValue b)
  {-# INLINE toUnValue #-}

instance (ToUnValue a, ToUnValue b, ToUnValue c) => ToUnValue (a, b, c) where
  toUnValue (a, b, c) = (toUnValue a, toUnValue b, toUnValue c)
  {-# INLINE toUnValue #-}

instance {-# OVERLAPPABLE #-} a ~ UnValue a => ToUnValue a where
  toUnValue = id
  {-# INLINE toUnValue #-}
