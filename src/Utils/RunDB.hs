module Utils.RunDB
  ( runDB, DB
  , module Database.Persist.Sql
  , module Database.Persist
  , askDB
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Effect
import Control.Monad.Reader
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
  eResult <- lift $ try $ runSqlPool acts pool
  case eResult of
    Left (SomeException _e) -> do
      -- $logError $ "Database error (retry in 1s): " <> tshow e
      liftIO $ threadDelay (1 * 1000000)
      -- wait for 1 seconds before retrying
      -- retry at most once
      -- this is only a temporary workaround, real fix is
      -- * replace meowbot monad with effect system
      -- * use postgresql instead of sqlite
      lift $ runSqlPool acts pool
    Right result -> return result
