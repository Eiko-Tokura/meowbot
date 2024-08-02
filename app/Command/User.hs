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
  = UserManagement Action  UserGroup  UserId
  | GroupManagement Action GroupGroup GroupId

data Action = Add | Remove | List

commandUser :: BotCommand
commandUser = BotCommand User $ botT $ do
  (msg, cid, uid, _) <- MaybeT $ getEssentialContent <$> ask
  um <- pureMaybe $ MP.mRunParserF userParser msg
  other <- lift get
  let sd = savedData other
  -- _ <- pureMaybe $ checkAdminUsers sd uid
  let (actions, sd') = case um of
        UserManagement Add ug uid                      -> ([reportUM other cid um], sd {userGroups = insert (uid, ug) $ userGroups sd})
        UserManagement Remove ug uid                   -> ([reportUM other cid um], sd {userGroups = filter (/= (uid, ug)) $ userGroups sd})
        UserManagement List _ _                       -> ([reportUM other cid um], sd)
        GroupManagement Add gg gid    -> ([reportUM other cid um], sd {groupGroups = insert (gid, gg) $ groupGroups sd})
        GroupManagement Remove gg gid -> ([reportUM other cid um], sd {groupGroups = filter (/= (gid, gg)) $ groupGroups sd})
        GroupManagement List _ _               -> ([reportUM other cid um], sd)
  lift . put $ other {savedData = sd'}
  return actions
  where reportUM other_data cid um = baSendToChatId cid $ T.pack $ case um of
          UserManagement Add ug uid                      -> "已添加用户" ++ show uid ++ "为" ++ show ug ++ "组。"
          UserManagement Remove ug uid                   -> "已移除用户" ++ show uid ++ "的" ++ show ug ++ "组。"
          UserManagement List ug _                       -> show ug ++ "用户列表：" ++ show (map fst $ filter ((== ug) . snd) $ userGroups $ savedData other_data)
          GroupManagement Add gg gid    -> "已添加群" ++ show gid ++ "为" ++ show gg ++ "组。"
          GroupManagement Remove gg (GroupId gid) -> "已移除群" ++ show gid ++ "的" ++ show gg ++ "组。"
          GroupManagement List gg _               -> show gg ++ "群列表：" ++ show (map fst $ filter ((== gg) . snd) $ groupGroups $ savedData other_data)

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
checkIfIsGroupNeedBeAllowedUsers sd (GroupChat _, uid) = mIf ((uid, Allowed) `elem` userGroups sd) ()
checkIfIsGroupNeedBeAllowedUsers _ _ = Nothing

checkUidIn :: SavedData -> UserId -> UserGroup -> Maybe UserId
checkUidIn sd uid ug = mIf ((uid, ug) `elem` userGroups sd) uid

checkGroupIn :: SavedData -> ChatId -> GroupGroup -> Maybe ChatId
checkGroupIn _ (PrivateChat uid) _ = Just (PrivateChat uid)
checkGroupIn sd g@(GroupChat gid) gg = mIf ((gid, gg) `elem` groupGroups sd) g

check :: SavedData -> ChatId -> (SavedData -> [ChatId]) -> Maybe ChatId
check sd cid f = mIf (cid `elem` f sd) cid

checkAdminUsers :: SavedData -> UserId -> Maybe UserId
checkAdminUsers sd uid = mIf ((uid, Admin) `elem` userGroups sd) uid

