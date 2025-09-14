module Utils.RunDB
  ( runDB, DB
  , module Database.Persist.Sql
  , module Database.Persist
  ) where

import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql hiding (In)
import Data.HList
import Module.LogDatabase
import System.General
import MeowBot.Prelude

runDB :: (LogDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowT r mods IO b
runDB acts = do
  pool <- asks (databasePool . getF @LogDatabase . fst . snd)
  lift $ runSqlPool acts pool
