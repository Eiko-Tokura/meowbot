{-# LANGUAGE DerivingVia#-}
module MeowBot.CommandRule where

import Data.Aeson
import Control.DeepSeq
import Database.Persist
import Database.Persist.Sqlite

newtype UserId  = UserId  Int deriving (Eq, Show, Ord, Read) deriving (ToJSON, FromJSON, Num, NFData) via Int
newtype GroupId = GroupId Int deriving (Eq, Show, Ord, Read) deriving (ToJSON, FromJSON, Num, NFData) via Int

instance PersistField GroupId where
  toPersistValue (GroupId gid) = toPersistValue gid
  fromPersistValue = fmap GroupId . fromPersistValue

instance PersistField UserId where
  toPersistValue (UserId uid) = toPersistValue uid
  fromPersistValue = fmap UserId . fromPersistValue

instance PersistFieldSql UserId where sqlType _ = SqlInt64
instance PersistFieldSql GroupId where sqlType _ = SqlInt64

data UserGroup  = Admin | Allowed | Denied | CustomUserGroup String deriving (Show, Eq, Ord, Read)
data GroupGroup = AllowedGroup | CustomGroupGroup String deriving (Show, Eq, Ord, Read)

data CommandRule = Allow UserObject CommandObject | Deny UserObject CommandObject
  deriving (Show, Eq, Ord, Read)

data UserObject
  = AllUserAndGroups
  | AllUsers
  | AllGroups
  | UGroup UserGroup
  | GGroup GroupGroup
  | SingleUser UserId
  | SingleGroup GroupId
  | SubtractUserObject UserObject UserObject
  | ExceptUserObject UserObject
  deriving (Show, Eq, Ord, Read)

data CommandObject
  = AllCommands
  | CGroup [CommandId]
  | SingleCommand CommandId
  | SubtractCommand CommandObject CommandObject
  | ExceptCommands [CommandId]
  deriving (Show, Eq, Ord, Read)

data CommandId = Aokana | Cat | Help | Md | Random | Retract | System | User | Study | BookMan | Poll
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

safeCommandGroup :: CommandObject
safeCommandGroup = CGroup [Cat, Help, Md, Random]

advancedCommandGroup :: CommandObject
advancedCommandGroup = CGroup [Aokana, System]

inUserObject :: [(UserId, UserGroup)] -> UserId -> UserObject -> Bool
inUserObject _   _   AllUserAndGroups  = True
inUserObject _   _   AllUsers          = True
inUserObject _   _   AllGroups         = False
inUserObject _   uid (SingleUser uid') = uid == uid'
inUserObject _   _   (SingleGroup _)   = False
inUserObject ugs uid (UGroup ug)       = (uid, ug) `elem` ugs
inUserObject _   _   (GGroup _)        = False
inUserObject _   _   (SubtractUserObject uo1 uo2) = inUserObject [] 0 uo1 && not (inUserObject [] 0 uo2)
inUserObject _   _   (ExceptUserObject uo) = not (inUserObject [] 0 uo)

gInUserObject :: [(GroupId, GroupGroup)] -> GroupId -> UserObject -> Bool
gInUserObject _ _ AllUserAndGroups     = True
gInUserObject _ _ AllUsers             = False
gInUserObject _ _ AllGroups            = True
gInUserObject _ _ (SingleUser _)       = False
gInUserObject _ gid (SingleGroup gid') = gid == gid'
gInUserObject _ _ (UGroup _)           = False
gInUserObject ggs gid (GGroup gg)      = (gid, gg) `elem` ggs
gInUserObject _ _ (SubtractUserObject uo1 uo2) = gInUserObject [] 0 uo1 && not (gInUserObject [] 0 uo2)
gInUserObject _ _ (ExceptUserObject uo) = not (gInUserObject [] 0 uo)

inCommandObject :: CommandId -> CommandObject -> Bool
inCommandObject _   AllCommands               = True
inCommandObject cid (SingleCommand cid')      = cid == cid'
inCommandObject cid (CGroup cids)             = cid `elem` cids
inCommandObject cid (ExceptCommands cids)     = cid `notElem` cids
inCommandObject cid (SubtractCommand co1 co2) = inCommandObject cid co1 && not (inCommandObject cid co2)

