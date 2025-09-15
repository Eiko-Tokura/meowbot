module Utils.RunDB
  ( runDB, DB
  , module Database.Persist.Sql
  , module Database.Persist
  , askDB
  ) where

import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql hiding (In)
import Data.HList
import Module.LogDatabase
import System.General
import MeowBot.Prelude
import Data.Pool

askDB :: (LogDatabase `In` mods) => MeowT r mods IO (Pool SqlBackend)
askDB = asks (databasePool . getF @LogDatabase . fst . snd)

runDB :: (LogDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowT r mods IO b
runDB acts = do
  pool <- askDB
  lift $ runSqlPool acts pool
