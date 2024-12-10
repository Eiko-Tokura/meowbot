{-# OPTIONS_GHC -Wno-orphans #-} 
-- since we use a class to restrict, this orphan instance will not affect other types
{-# LANGUAGE UndecidableInstances, DerivingVia, OverloadedStrings #-}
module Utils.Persist where

import Database.Persist
import Database.Persist.Sqlite
import Data.Bifunctor
import Control.Monad
import Text.Read
import Data.Text (pack)

class (Read a, Show a) => PersistUseShow a where

newtype PersistUseInt64 a = PersistUseInt64 a
  deriving (Bounded, Enum) via a

instance {-# OVERLAPPABLE #-} PersistUseShow a => PersistField a where
  toPersistValue = toPersistValue . show
  fromPersistValue = fromPersistValue >=> first pack . readEither
  {-# INLINE toPersistValue #-}
  {-# INLINE fromPersistValue #-}

instance {-# OVERLAPPABLE #-} PersistUseShow a => PersistFieldSql a where
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
