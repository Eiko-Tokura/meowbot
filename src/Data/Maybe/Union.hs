module Data.Maybe.Union where

import Control.Applicative

class Union a where
  union :: a -> a -> a

instance Union (Maybe a) where
  union = (<|>)


