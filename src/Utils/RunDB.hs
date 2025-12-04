module Utils.RunDB
  ( DB, askCoreDB, askDataDB
  , module Database.Persist.Sql
  , module Database.Persist
  , module Module.MeowTypes
  , PG.runRawPostgreSqlPool
  ) where

import Control.Monad.Effect
import Data.Pool
import Database.Persist
import Database.Persist.Sql hiding (In)
import MeowBot.Prelude
import Module.MeowTypes
import qualified  Module.Database.PostgreSql as PG

askCoreDB :: (MeowCoreDb `In` mods) => EffT mods es IO (Pool (PG.RawPostgresql SqlBackend))
askCoreDB = asksModule @MeowCoreDb PG.dbPool

askDataDB :: (MeowDataDb `In` mods) => EffT mods es IO (Pool (PG.RawPostgresql SqlBackend))
askDataDB = asksModule @MeowDataDb PG.dbPool
