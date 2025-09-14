module MeowBot.Affinity where

import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Data.Text (Text)

-- | For example 100
newtype Affinity = Affinity { unAffinity :: Int }
  deriving newtype (Eq, Ord, Show, PersistField, PersistFieldSql, Num)

-- | For example "小鱼干"
newtype FavoriteItem = FavoriteItem { unFavoriteItem :: Text }
  deriving newtype (Eq, Ord, Show, PersistField, PersistFieldSql)
