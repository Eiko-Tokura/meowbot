{-# LANGUAGE DerivingVia#-}
module MeowBot.CommandRule where

import Database.Persist
import Database.Persist.Sqlite
import Language.Haskell.TH
import MeowBot.Data.ChatId
import Utils.Persist

-- | When adding new commands, add at the last to preserve Enum
-- do not change the names of existing commands because they are used in the database
data CommandId = Aokana | Balance | Cat | Chat | Help | Md | Random | Retract | System | User | Study | BookMan | Poll | Hangman | Updater -- | Haskell
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- | show each command id into string, and prepend "command" to each. return a list of these functions
makeBotCommands :: [CommandId] -> Q Exp
makeBotCommands cmdIds = do
  let cmdNames = map (\cid -> "command" ++ show cid) cmdIds
  let cmdExps = map (VarE . mkName) cmdNames
  return $ ListE cmdExps

data UserGroup  = Admin | Allowed | Denied | CustomUserGroup String
  deriving (Show, Eq, Ord, Read)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow UserGroup)

data GroupGroup = AllowedGroup | CustomGroupGroup String
  deriving (Show, Eq, Ord, Read)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow GroupGroup)

data CommandRule = Allow UserObject CommandObject | Deny UserObject CommandObject
  deriving (Show, Eq, Ord, Read)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CommandRule)

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
  deriving (PersistField, PersistFieldSql) via (PersistUseShow UserObject)

data CommandObject
  = AllCommands
  | CGroup [CommandId]
  | SingleCommand CommandId
  | SubtractCommand CommandObject CommandObject
  | ExceptCommands [CommandId]
  deriving (Show, Eq, Ord, Read)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CommandObject)

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

