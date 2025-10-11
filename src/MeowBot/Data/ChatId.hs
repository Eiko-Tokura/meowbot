module MeowBot.Data.ChatId where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Default (Default(..))
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(SqlInt64))
import GHC.Generics (Generic)
import Data.Hashable

data Role = ROwner | RAdmin | RMember | RUnknown
  deriving (Show, Read, Eq, Generic, NFData, Bounded, Enum)

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "owner"  -> return ROwner
    "admin"  -> return RAdmin
    "member" -> return RMember
    _ -> return RUnknown

data ChatId = GroupChat GroupId | PrivateChat UserId
  deriving (Show, Eq, Ord, Read, Generic, NFData)

instance Default ChatId where
  def = GroupChat 0

newtype UserId  = UserId  Int deriving (Eq, Show, Ord, Read) deriving (ToJSON, FromJSON, Num, NFData, Hashable) via Int
newtype GroupId = GroupId Int deriving (Eq, Show, Ord, Read) deriving (ToJSON, FromJSON, Num, NFData, Hashable) via Int

unUserId :: UserId -> Int
unUserId (UserId uid) = uid

instance PersistField GroupId where
  toPersistValue (GroupId gid) = toPersistValue gid
  fromPersistValue = fmap GroupId . fromPersistValue

instance PersistField UserId where
  toPersistValue (UserId uid) = toPersistValue uid
  fromPersistValue = fmap UserId . fromPersistValue

instance PersistFieldSql UserId where sqlType _ = SqlInt64
instance PersistFieldSql GroupId where sqlType _ = SqlInt64

instance PersistField ChatId where
  toPersistValue (GroupChat gid) = toPersistValue gid
  toPersistValue (PrivateChat uid) = toPersistValue (-uid)
  fromPersistValue v = do
    gid <- fromPersistValue v
    if gid > 0
      then return $ GroupChat (GroupId gid)
      else return $ PrivateChat (UserId (-gid))

instance PersistFieldSql ChatId where sqlType _ = SqlInt64
