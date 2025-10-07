module Module.MeowTypes where

import Module.Database
import Control.Monad.IO.Class
import Control.Monad.Effect
import Control.Monad.Reader
import Control.Exception
import Utils.Text

type MeowDatabase = DB DefaultDB Sqlite

runMeowDB :: forall mods es m a c. (m ~ IO, In' c MeowDatabase mods, ErrorText "meowdb" `InList` es) => ReaderT SqlBackend IO a -> EffT' c mods es m a
runMeowDB = effTryIOInWith (\(e :: SomeException) -> errorText @"meowdb" $ toText e) . runSQ
{-# INLINE runMeowDB #-}
