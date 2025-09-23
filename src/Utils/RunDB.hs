module Utils.RunDB
  ( runDB, DB
  , module Database.Persist.Sql
  , module Database.Persist
  , askDB
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import Database.Persist
import Database.Persist.Sql hiding (In)
import Data.HList
import Module.LogDatabase
import System.General
import MeowBot.Prelude
import Data.Pool
import Control.Exception

askDB :: (LogDatabase `In` mods) => MeowT r mods IO (Pool SqlBackend)
askDB = asks (databasePool . getF @LogDatabase . fst . snd)

runDB :: (LogDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowT r mods IO b
runDB acts = do
  pool <- askDB
  eResult <- lift $ try $ runSqlPool acts pool
  case eResult of
    Left (SomeException e) -> do
      $logError $ "Database error (retry in 1s): " <> tshow e
      liftIO $ threadDelay (1 * 1000000)
      -- wait for 1 seconds before retrying
      -- retry at most once
      -- this is only a temporary workaround, real fix is
      -- * replace meowbot monad with effect system
      -- * use postgresql instead of sqlite
      lift $ runSqlPool acts pool
    Right result -> return result
