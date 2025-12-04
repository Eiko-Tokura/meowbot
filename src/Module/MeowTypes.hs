module Module.MeowTypes where

import Control.Exception
import Control.Monad.Effect
import Control.Monad.Reader
import Module.Database
import qualified Module.Database.PostgreSql as PG
import Utils.Text

type MeowCoreDb = DB DefaultDB PostgreSql
type MeowDataDb = DB "data_db" PostgreSql

runMeowCoreDB ::
  ( In' c MeowCoreDb mods
  , InList (ErrorText "meowCoreDb") es
  ) => ReaderT SqlBackend IO b -> EffT' c mods es IO b
runMeowCoreDB acts = do
  pool <- asksModule @MeowCoreDb PG.dbPool
  eResult <- lift $ try $ PG.runRawPostgreSqlPool acts pool
  case eResult of
    Left (e :: SomeException) -> effThrow (errorText @"meowCoreDb" (toText e))
    Right result -> return result

runMeowDataDB ::
  ( In' c MeowDataDb mods
  , InList (ErrorText "meowDataDb") es
  ) => ReaderT SqlBackend IO b -> EffT' c mods es IO b
runMeowDataDB acts = do
  pool <- asksModule @MeowDataDb PG.dbPool
  eResult <- lift $ try @SomeException $ PG.runRawPostgreSqlPool acts pool
  case eResult of
    Left (e :: SomeException) -> do
      effThrow (errorText @"meowDataDb" (toText e))
    Right result -> return result
