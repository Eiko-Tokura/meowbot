module Module.MeowTypes where

import Module.Database
import Control.Monad.IO.Class
import Control.Monad.Effect
import Control.Monad.Reader

type MeowDatabase = DB DefaultDB Sqlite

runMeowDB :: forall mods es m a c. (MonadIO m, In' c MeowDatabase mods) => ReaderT SqlBackend IO a -> EffT' c mods es m a
runMeowDB = runSQ
{-# INLINE runMeowDB #-}
