module MeowBot.Prelude
  ( module Control.Concurrent.STM
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.Trans.Maybe
  , module Data.Maybe
  , module Data.Time
  , module Database.Persist
  , module Utils.Text
  , (&)
  , DB
  , Generic, NFData
  , NonEmpty(..)
  , makeLenses_
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Time
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Utils.Lens
import Utils.Text

type DB = ReaderT SqlBackend IO
