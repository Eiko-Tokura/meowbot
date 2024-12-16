module Utils.RunDB
  ( runDB
  , module Database.Persist.Sql
  , module Database.Persist
  ) where

import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql hiding (In)
import Data.HList
import Module.LogDatabase
import System.General

runDB :: (LogDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowT r mods IO b
runDB acts = do
  pool <- databasePool <$> asks (getF @LogDatabase . fst . snd)
  lift $ runSqlPool acts pool

