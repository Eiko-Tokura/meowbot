module Command.User where

import Command
import MonParserF (ParserF(..))
import qualified MonParserF as MP
import Control.Monad.IOe
import MeowBot.BotStructure
import MeowBot.CommandRule
import qualified Data.Text as T
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

data UserManagement
  = UserManagement  Action UserGroup  (Maybe UserId)
  | GroupManagement Action GroupGroup (Maybe GroupId)
  | RuleManagement  Action (Maybe CommandRule)

data Action = Add | Remove | List 

commandUser :: BotCommand
commandUser = BotCommand User $ botT $ do
  (msg, cid, _, _) <- MaybeT $ getEssentialContent <$> ask
  um <- pureMaybe $ MP.mRunParserF userParser msg
  other <- lift get
  let sd = savedData other
  let (actions, sd') = case um of
        UserManagement Add ug (Just uid)     -> ([reportUM other cid um], sd {userGroups = insert (uid, ug) $ userGroups sd})
        UserManagement Remove ug (Just uid)  -> ([reportUM other cid um], sd {userGroups = filter (/= (uid, ug)) $ userGroups sd})
        UserManagement List _ _              -> ([reportUM other cid um], sd)
        GroupManagement Add gg (Just gid)    -> ([reportUM other cid um], sd {groupGroups = insert (gid, gg) $ groupGroups sd})
        GroupManagement Remove gg (Just gid) -> ([reportUM other cid um], sd {groupGroups = filter (/= (gid, gg)) $ groupGroups sd})
        GroupManagement List _ _             -> ([reportUM other cid um], sd)
        RuleManagement Add (Just cr)         -> ([reportUM other cid um], sd {commandRules = insert cr $ commandRules sd})
        RuleManagement Remove (Just cr)      -> ([reportUM other cid um], sd {commandRules = filter (/= cr) $ commandRules sd})
        RuleManagement List _                -> ([reportUM other cid um], sd)
        _ -> ([], sd)
  lift . put $ other {savedData = sd'}
  return actions
  where reportUM other_data cid um = baSendToChatId cid $ T.pack $ case um of
          UserManagement Add ug (Just uid)     -> "已添加用户" ++ show uid ++ "为" ++ show ug ++ "组。"
          UserManagement Remove ug (Just uid)  -> "已移除用户" ++ show uid ++ "的" ++ show ug ++ "组。"
          UserManagement List ug _             -> show ug ++ "用户列表：" ++ show (map fst $ filter ((== ug) . snd) $ userGroups $ savedData other_data)
          GroupManagement Add gg (Just gid)    -> "已添加群" ++ show gid ++ "为" ++ show gg ++ "组。"
          GroupManagement Remove gg (Just gid) -> "已移除群" ++ show gid ++ "的" ++ show gg ++ "组。"
          GroupManagement List gg _            -> show gg ++ "群列表：" ++ show (map fst $ filter ((== gg) . snd) $ groupGroups $ savedData other_data)
          RuleManagement Add (Just cr)         -> "已添加规则" ++ show cr
          RuleManagement Remove (Just cr)      -> "已移除规则" ++ show cr
          RuleManagement List _                -> "规则列表：" ++ "\n[ " ++ intercalate "\n, " (show <$> commandRules (savedData other_data)) ++ "\n]"
          _ -> "user_id / group_id parameter cannot be empty o.o"

userParser :: ParserF Char UserManagement
userParser = 
  (MP.headCommand "user" >> MP.spaces >>
    UserManagement <$> actionParser <*> (MP.spaces *> userGroupParser) <*> (MP.spaces0 >> MP.canBeEmpty (UserId <$> idParser)))
  <>
  (MP.headCommand "group" >> MP.spaces >>
    GroupManagement <$> actionParser <*> (MP.spaces *> groupGroupParser) <*> (MP.spaces0 >> MP.canBeEmpty (GroupId <$> idParser)))
  <>
  (MP.headCommand "rule" >> MP.spaces >>
    RuleManagement <$> actionParser <*> (MP.spaces0 >> MP.canBeEmpty ruleParser))
  where actionParser = foldr1 (<>) [ MP.string "add" >> return Add
                                   , MP.string "remove" >> return Remove
                                   , MP.string "list" >> return List 
                                   ]
        userGroupParser = foldr1 (<>) [ MP.string "admin" >> return Admin
                                      , MP.string "allowed" >> return Allowed 
                                      , CustomUserGroup <$> MP.word
                                      ]
        groupGroupParser = mconcat [ MP.string "allowed" >> return AllowedGroup
                                   , CustomGroupGroup <$> MP.word 
                                   ]
        idParser = read <$> (do d <- MP.digits; if length d > 12 then MP.zero else return d)
        ruleParser = MP.parseByRead

checkIfIsGroupNeedBeAllowedUsers :: SavedData -> (ChatId, UserId) -> Maybe ()
checkIfIsGroupNeedBeAllowedUsers sd (GroupChat _, uid) = mIf ((uid, Allowed) `elem` userGroups sd) ()
checkIfIsGroupNeedBeAllowedUsers _ _ = Nothing

checkUidIn :: SavedData -> UserId -> UserGroup -> Maybe UserId
checkUidIn sd uid ug = mIf ((uid, ug) `elem` userGroups sd) uid

checkGroupIn :: SavedData -> ChatId -> GroupGroup -> Maybe ChatId
checkGroupIn _ (PrivateChat uid) _ = Just (PrivateChat uid)
checkGroupIn sd g@(GroupChat gid) gg = mIf ((gid, gg) `elem` groupGroups sd) g

checkAdminUsers :: SavedData -> UserId -> Maybe UserId
checkAdminUsers sd uid = mIf ((uid, Admin) `elem` userGroups sd) uid

