{-# LANGUAGE UndecidableInstances, DerivingVia, OverloadedStrings #-}
module Utils.Persist
  ( PersistUseShow(..)
  , PersistUseInt64(..)
  , PersistField, PersistFieldSql
  ) where

import Database.Persist
import Database.Persist.Sql
import Data.Bifunctor
import Control.Monad
import Text.Read
import Data.Text (pack)

-- | This is a newtype wrapper for PersistField and PersistFieldSql instances stored as Show
-- to use it, either wrap your data type with PersistUseInt64
-- or write
--   deriving (PersistField, PersistFieldSql) via (PersistUseInt64 YourType)
newtype PersistUseShow a = PersistUseShow a
  deriving (Show, Read) via a

-- | Newtype wrapper for PersistField and PersistFieldSql instances storing Enum as Int64
newtype PersistUseInt64 a = PersistUseInt64 a
  deriving (Bounded, Enum) via a

instance (Show a, Read a) => PersistField (PersistUseShow a) where
  toPersistValue = toPersistValue . show
  fromPersistValue = fromPersistValue >=> first pack . readEither
  {-# INLINE toPersistValue #-}
  {-# INLINE fromPersistValue #-}

instance (Show a, Read a) => PersistFieldSql (PersistUseShow a) where
  sqlType _ = SqlString
  {-# INLINE sqlType #-}

-- | The use of constraint class UseInt64 effectively eliminated the potential danger of mixing instances
-- although it did add some extra work to the compiler
instance (Bounded t, Enum t) => PersistField (PersistUseInt64 t) where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    {-# INLINE toPersistValue #-}
    fromPersistValue (PersistInt64 i) = Right . toEnum . fromIntegral $ i
    fromPersistValue _ = Left "expect Int64 in a derived PersistField for Bounded Enum"
    {-# INLINE fromPersistValue #-}

instance (Bounded t, Enum t) => PersistFieldSql (PersistUseInt64 t) where
    sqlType _ = SqlInt64
    {-# INLINE sqlType #-}
