module MeowBot.Data.IgnoreMatchType where

import Database.Persist
import Database.Persist.Sql
import Utils.Persist

data IgnoreMatchType
  = IgnoreExact
  | IgnoreExactStrip
  | IgnorePrefix
  -- | IgnoreRegex
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow IgnoreMatchType)
