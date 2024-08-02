{-# LANGUAGE DerivingVia#-}
module MeowBot.CommandRule where

import Data.Aeson

newtype UserId = UserId Int deriving (Eq, Show, Ord, Read) deriving (ToJSON, FromJSON, Num) via Int
newtype GroupId = GroupId Int deriving (Eq, Show, Ord, Read) deriving (ToJSON, FromJSON, Num) via Int

data UserGroup  = Admin | Allowed | Denied | CustomUserGroup String deriving (Show, Eq, Ord, Read)
data GroupGroup = AllowedGroup deriving (Show, Eq, Ord, Read)

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
  deriving (Show, Eq, Ord, Read)

data CommandObject = AllCommands | CGroup [CommandId] | SingleCommand CommandId
  deriving (Show, Eq, Ord, Read)

data CommandId = Aokana | Cat | Help | Md | Random | Retract | System | User 
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

safeCommandGroup :: CommandObject
safeCommandGroup = CGroup [Cat, Help, Md, Random]

advancedCommandGroup :: CommandObject
advancedCommandGroup = CGroup [Aokana, System]

inUserObject :: [(UserId, UserGroup)] -> UserId -> UserObject -> Bool
inUserObject ugs _ AllUserAndGroups = True
inUserObject ugs _ AllUsers = True
inUserObject ugs _ AllGroups = False
inUserObject ugs uid (SingleUser uid') = uid == uid'
inUserObject ugs _ (SingleGroup _) = False
inUserObject ugs uid (UGroup ug) = (uid, ug) `elem` ugs
inUserObject ugs uid (GGroup _) = False

gInUserObject :: [(GroupId, GroupGroup)] -> GroupId -> UserObject -> Bool
gInUserObject _ _ AllUserAndGroups = True
gInUserObject _ _ AllUsers = False
gInUserObject _ _ AllGroups = True
gInUserObject _ _ (SingleUser _) = False
gInUserObject _ gid (SingleGroup gid') = gid == gid'
gInUserObject _ _ (UGroup _) = False
gInUserObject ggs gid (GGroup gg) = (gid, gg) `elem` ggs

inCommandObject :: CommandId -> CommandObject -> Bool
inCommandObject _ AllCommands = True
inCommandObject cid (SingleCommand cid') = cid == cid'
inCommandObject cid (CGroup cids) = cid `elem` cids

