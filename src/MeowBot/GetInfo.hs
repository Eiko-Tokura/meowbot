module MeowBot.GetInfo where

import MeowBot
import MeowBot.Data.CQHttp.CQCode
import Control.Monad.Trans.Maybe
import Data.Maybe
import MeowBot.Parser
-- import External.ChatAPI
import Data.List.NonEmpty (NonEmpty(..), prependList)
import Data.PersistModel
import Utils.RunDB

type IsAdmin = Bool
newtype IsSuperUser = IsSuperUser { boolIsSuperUser :: Bool }
isAdmin :: UserId -> Meow IsAdmin
isAdmin uid = do
  mRecord <- runDB $ selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] []
  return $ isJust mRecord

isSuperUser :: UserId -> Meow IsSuperUser
isSuperUser uid = do
  mRecord <- runDB $ getBy $ UniqueSuperUser uid
  return $ IsSuperUser $ isJust mRecord

-- | Whether the newest message contains @bot
beingAt :: Meow Bool
beingAt = fmap (fromMaybe False) . runMaybeT $ do
  cqmsg  <- MaybeT $ queries (fmap cqcodes . metaMessage . getNewMsg)
  selfId <- MaybeT $ queries (fmap ((\(UserId uid) -> uid) . selfId) . selfInfo)
  return $ any (\case
      CQAt uid _ -> uid == selfId
      _          -> False
    ) cqmsg
{-# INLINABLE beingAt #-}

-- | Whether the newest message replies to the bot
beingReplied :: Meow Bool
beingReplied = do
  firstTree <- queries getFirstTree
  case runParser replyContextParser firstTree of
    Just _  -> return True
    Nothing -> return False
{-# INLINABLE beingReplied #-}

-- | Parse if the newest message is a reply to the bot, if so return the full context ending with the reply
replyContextParser :: forall s. (IsStream s CQMessage) => Parser s CQMessage (NonEmpty CQMessage)
replyContextParser = do
  prevCQMsgs <- many (satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage, SelfMessage]))
  aCQMsg <- satisfy (\cqm -> eventType cqm == SelfMessage)
  uCQMsg <- satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage])
  return $ prevCQMsgs `prependList` (aCQMsg :| [uCQMsg])
{-# INLINABLE replyContextParser #-}

