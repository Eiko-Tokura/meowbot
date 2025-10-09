module Utils.RunDB
  ( DB, askDB
  , module Database.Persist.Sql
  , module Database.Persist
  , module Module.MeowTypes
  ) where

import Control.Monad.Effect
import Data.Pool
import Database.Persist
import Database.Persist.Sql hiding (In)
import MeowBot.Prelude
import Module.MeowTypes
import Module.Database.Sqlite

askDB :: (MeowDatabase `In` mods) => EffT mods es IO (Pool SqlBackend)
askDB = asksModule @MeowDatabase dbPool
