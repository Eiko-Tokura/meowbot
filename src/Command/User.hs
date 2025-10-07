{-# LANGUAGE TemplateHaskell #-}
module Command.User where

import Command
import MeowBot.Parser (Parser, (<|>))
import qualified MeowBot.Parser as MP
import Control.Monad.IOe
import MeowBot
import MeowBot.CommandRule
import qualified Data.Text as T
import qualified Data.List as L
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Module.RS
import Control.Monad.Effect
import Control.Monad.RS.Class
import Utils.RunDB hiding (Add)
import Data.PersistModel

data UserManagement
  = UserManagement  Action UserGroup  (Maybe UserId)
  | GroupManagement Action GroupGroup (Maybe GroupId)
  | RuleManagement  Action (Maybe CommandRule)

data Action = Add | Remove | List

commandUser :: BotCommand
commandUser = BotCommand User $ botT $ do
  (msg, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  userParser' <- lift $ commandParserTransformByBotName userParser
  botname <- queries maybeBotName
  botid   <- query
  um <- MaybeT $ pure $ MP.runParser userParser' msg
  other <- lift query
  let sd = savedData other
  let (actions, sd', db) = case um of
        UserManagement Add ug (Just uid)     -> ([reportUM other cid um], sd {userGroups = L.insert (uid, ug) $ userGroups sd}, runDB $ do p <- selectFirst [InUserGroupBotName ==. botname, InUserGroupUserId ==. uid, InUserGroupUserGroup ==. ug] []; maybe (insert_ $ InUserGroup botname botid uid ug) (const $ return ()) p)
        UserManagement Remove ug (Just uid)  -> ([reportUM other cid um], sd {userGroups = filter (/= (uid, ug)) $ userGroups sd}, runDB $ deleteWhere [InUserGroupBotName ==. botname, InUserGroupUserId ==. uid, InUserGroupUserGroup ==. ug])
        UserManagement List _ _              -> ([reportUM other cid um], sd, return ())
        GroupManagement Add gg (Just gid)    -> ([reportUM other cid um], sd {groupGroups = L.insert (gid, gg) $ groupGroups sd}, runDB $ do p <- selectFirst [InGroupGroupBotName ==. botname, InGroupGroupGroupId ==. gid, InGroupGroupGroupGroup ==. gg] []; maybe (insert_ $ InGroupGroup botname botid gid gg) (const $ return ()) p)
        GroupManagement Remove gg (Just gid) -> ([reportUM other cid um], sd {groupGroups = filter (/= (gid, gg)) $ groupGroups sd}, runDB $ deleteWhere [InGroupGroupBotName ==. botname, InGroupGroupGroupId ==. gid, InGroupGroupGroupGroup ==. gg])
        GroupManagement List _ _             -> ([reportUM other cid um], sd, return ())
        RuleManagement Add (Just cr)         -> ([reportUM other cid um], sd {commandRules = L.insert cr $ commandRules sd}, runDB $ do p <- selectFirst [CommandRuleDBBotName ==. botname, CommandRuleDBCommandRule ==. cr] []; maybe (insert_ $ CommandRuleDB botname botid cr) (const $ return ()) p)
        RuleManagement Remove (Just cr)      -> ([reportUM other cid um], sd {commandRules = filter (/= cr) $ commandRules sd}, runDB $ deleteWhere [CommandRuleDBBotName ==. botname, CommandRuleDBCommandRule ==. cr])
        RuleManagement List _                -> ([reportUM other cid um], sd, return ())
        _ -> ([], sd, return ())
  lift $ db
  lift . modify $ \other -> other {savedData = sd'}
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
          RuleManagement List _                -> "规则列表：" ++ "\n[ " ++ L.intercalate "\n, " (show <$> commandRules (savedData other_data)) ++ "\n]"
          _ -> "user_id / group_id parameter cannot be empty o.o"

userParser :: (MP.Chars sb) => Parser sb Char UserManagement
userParser =
  (MP.headCommand "user" >> MP.spaces >>
    UserManagement <$> actionParser <*> (MP.spaces *> userGroupParser) <*> (MP.spaces0 >> MP.canBeEmpty (UserId <$> idParser)))
  <|>
  (MP.headCommand "group" >> MP.spaces >>
    GroupManagement <$> actionParser <*> (MP.spaces *> groupGroupParser) <*> (MP.spaces0 >> MP.canBeEmpty (GroupId <$> idParser)))
  <|>
  (MP.headCommand "rule" >> MP.spaces >>
    RuleManagement <$> actionParser <*> (MP.spaces0 >> MP.canBeEmpty ruleParser))
  where actionParser = MP.asumE [ $(MP.stringQ "add") >> return Add
                                , $(MP.stringQ "remove") >> return Remove
                                , $(MP.stringQ "list") >> return List
                                ]
        userGroupParser = MP.asumE [ $(MP.stringQ "admin") >> return Admin
                                   , $(MP.stringQ "allowed") >> return Allowed
                                   , $(MP.stringQ "denied") >> return Denied
                                   , CustomUserGroup <$> MP.word
                                   ]
        groupGroupParser = ($(MP.stringQ "allowed") >> return AllowedGroup)
                                   <|> ( CustomGroupGroup <$> MP.word )
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

