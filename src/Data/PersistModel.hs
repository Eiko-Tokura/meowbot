{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies, DerivingStrategies, UndecidableInstances, DataKinds, TemplateHaskell, QuasiQuotes #-}
module Data.PersistModel where

import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist
import Data.Typeable

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Insert data types that are used in the project
-- For example:
-- User
--  name String
--  age Int
--  deriving Show
|]
