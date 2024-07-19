module Command.User where

import Command
import MonParserF (ParserF(..))
import qualified MonParserF as MP
import Control.Monad.IOe
import MeowBot.BotStructure
import qualified Data.Text as T
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

data UserManagement 
  = UserManagement Action UserGroup   UserId
  | GroupManagement Action GroupGroup ChatId

data Action = Add | Remove | List
data UserGroup  = Admin | Allowed | Denied
data GroupGroup = AllowedGroup

commandUser :: BotCommand
commandUser = botT $ do
  (msg, cid, uid, _) <- MaybeT $ getEssentialContent <$> ask
  um <- pureMaybe $ MP.mRunParserF userParser msg
  other <- lift get
  let sd = savedData other
  _ <- pureMaybe $ checkAdminUsers sd uid
  let (actions, sd') = case um of
        UserManagement Add Admin uid                      -> ([reportUM other cid um], sd {adminUsers = insert uid $ adminUsers sd})
        UserManagement Add Allowed uid                    -> ([reportUM other cid um], sd {allowedUsers = insert uid $ allowedUsers sd})
        UserManagement Add Denied uid                     -> ([reportUM other cid um], sd {deniedUsers = insert uid $ deniedUsers sd})
        UserManagement Remove Admin uid                   -> ([reportUM other cid um], sd {adminUsers = filter (/= uid) $ adminUsers sd})
        UserManagement Remove Allowed uid                 -> ([reportUM other cid um], sd {allowedUsers = filter (/= uid) $ allowedUsers sd})
        UserManagement Remove Denied uid                  -> ([reportUM other cid um], sd {deniedUsers = filter (/= uid) $ deniedUsers sd})
        UserManagement List Admin _                       -> ([reportUM other cid um], sd)
        UserManagement List Allowed _                     -> ([reportUM other cid um], sd)
        GroupManagement Add AllowedGroup (GroupId gid)    -> ([reportUM other cid um], sd {allowedGroups = insert gid $ allowedGroups sd})
        GroupManagement Remove AllowedGroup (GroupId gid) -> ([reportUM other cid um], sd {allowedGroups = filter (/= gid) $ allowedGroups sd})
        GroupManagement List AllowedGroup _               -> ([reportUM other cid um], sd)
        _ -> ([reportUM other cid um], sd)
  lift . put $ other {savedData = sd'}
  return actions
  where reportUM other_data cid um = baSendToChatId cid $ T.pack $ case um of
          UserManagement Add Admin uid                      -> "已添加用户" ++ show uid ++ "为管理员。"
          UserManagement Add Allowed uid                    -> "已添加用户" ++ show uid ++ "为允许用户。"
          UserManagement Add Denied uid                    -> "已添加用户" ++ show uid ++ "为拒绝用户。"
          UserManagement Remove Admin uid                   -> "已移除用户" ++ show uid ++ "的管理员权限。"
          UserManagement Remove Allowed uid                 -> "已移除用户" ++ show uid ++ "的允许权限。"
          UserManagement Remove Denied uid                 -> "已移除用户" ++ show uid ++ "的拒绝组。"
          UserManagement List Admin _                       -> "管理员列表：" ++ show (adminUsers $ savedData other_data)
          UserManagement List Allowed _                     -> "允许用户列表：" ++ show (allowedUsers $ savedData other_data)
          GroupManagement Add AllowedGroup (GroupId gid)    -> "已添加群" ++ show gid ++ "为允许群。"
          GroupManagement Remove AllowedGroup (GroupId gid) -> "已移除群" ++ show gid ++ "的允许权限。"
          GroupManagement List AllowedGroup _               -> "允许群列表：" ++ show (allowedGroups $ savedData other_data)
          _ -> "?"

userParser :: ParserF Char UserManagement
userParser = 
  (MP.headCommand "user" >> MP.spaces >>
    UserManagement <$> actionParser <*> userGroupParser <*> (UserId <$> idParser))
  <>
  (MP.headCommand "group" >> MP.spaces >>
    GroupManagement <$> actionParser <*> groupGroupParser <*> (GroupId <$> idParser))
  where actionParser = foldr1 (<>) [ MP.string "add" >> MP.spaces >> return Add
                                   , MP.string "remove" >> MP.spaces >> return Remove
                                   , MP.string "list" >> MP.spaces >> return List ] 
        userGroupParser = foldr1 (<>) [ MP.string "admin" >> MP.spaces >> return Admin
                                      , MP.string "allowed" >> MP.spaces >> return Allowed ]
        groupGroupParser = MP.string "allowed" >> MP.spaces >> return AllowedGroup
        idParser = read <$> (do d <- MP.digits; if length d > 12 then MP.zero else return d)


checkIfIsGroupNeedBeAllowedUsers :: SavedData -> (ChatId, UserId) -> Maybe ()
checkIfIsGroupNeedBeAllowedUsers sd (GroupId _, uid) = mIf (uid `elem` allowedUsers sd) ()
checkIfIsGroupNeedBeAllowedUsers _ _ = Nothing

checkUidInAllowedUsers :: SavedData -> UserId -> Maybe UserId
checkUidInAllowedUsers sd uid = mIf (uid `elem` allowedUsers sd) uid

checkCidInAllowedGroups _ (PrivateId uid) = Just (PrivateId uid)
checkCidInAllowedGroups sd g@(GroupId gid) = mIf (gid `elem` allowedGroups sd) g

checkAdminUsers :: SavedData -> UserId -> Maybe UserId
checkAdminUsers sd uid = mIf (uid `elem` adminUsers sd) uid


