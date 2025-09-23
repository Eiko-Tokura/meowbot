module Utils.RunDB
  ( runDB, DB
  , module Database.Persist.Sql
  , module Database.Persist
  , askDB
  ) where

import Control.Monad.Reader
import Control.Monad.Effect
import Data.Pool
import Database.Persist
import Database.Persist.Sql hiding (In)
import MeowBot.Prelude
import Module.Database.Sqlite
import Module.MeowTypes

askDB :: (MeowDatabase `In` mods) => EffT mods es IO (Pool SqlBackend)
askDB = do
  r <- askModule @MeowDatabase
  return $ dbPool r

runDB :: (MeowDatabase `In` mods) => ReaderT SqlBackend IO b -> EffT mods es IO b
runDB acts = do
  pool <- askDB
  lift $ runSqlPool acts pool
