module Data.Maybe.Union where

import Control.Applicative
import Data.Maybe
import GHC.Generics

class Union a where
  union :: a -> a -> a

instance Union (Maybe a) where
  union = (<|>)


