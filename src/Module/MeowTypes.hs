module Module.MeowTypes where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Effect
import Control.Monad.Reader
import Module.Database
import Module.Database.Sqlite
import Utils.Text
import qualified Database.Sqlite as Sqlite

type MeowDatabase = DB DefaultDB Sqlite

runMeowDB ::
  ( In' c MeowDatabase mods
  , InList (ErrorText "meowdb") es
  ) => ReaderT SqlBackend IO b -> EffT' c mods es IO b
runMeowDB acts = do
  pool <- asksModule @MeowDatabase dbPool
  eResult <- lift $ try $ runSqlPool acts pool
  let retryDb = do
        liftIO $ threadDelay (1 * 200_000) -- 0.2 second
        lift $ runSqlPool acts pool
  case eResult of
    Left (e :: SomeException) -> do
      case fmap Sqlite.seError (fromException e) :: Maybe Sqlite.Error of
        Just Sqlite.ErrorBusy   -> retryDb
        Just Sqlite.ErrorLocked -> retryDb
        _                       -> effThrow (errorText @"meowdb" (toText e))
    Right result -> return result
