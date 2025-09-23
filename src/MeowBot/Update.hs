{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- | This module contains the functions that update the bot's basic states and data structures.
module MeowBot.Update where

import MeowBot.BotStructure

import Data.Maybe (fromMaybe, listToMaybe)

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Effect
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.ReaderState
import Control.Parallel.Strategies
import Data.Additional
import Data.Bifunctor
import Data.Coerce
import Data.Time.Clock
import Data.UpdateMaybe
import Debug.Trace
import External.ProxyWS (cqhttpHeaders, Headers)
import MeowBot.Parser (Tree(..), flattenTree)
import Utils.List
import qualified MeowBot.Parser as MP
import Module.RS
import Module.RecvSentCQ

forestSizeForEachChat = 32 -- ^ controls how many trees to keep in each chat room

-- | Using a lifecycle event message, update the bot's self_id
updateSelfInfo :: (MonadReadable AllData m, MonadModifiable AllData m) => CQMessage -> m ()
updateSelfInfo cqmsg = do
  mselfInfo <- queries (selfInfo . otherdata)
  let msid = self_id cqmsg
  case (mselfInfo, msid) of
    (Nothing, Just sid) -> change $ _otherdata . _selfInfo ?~ SelfInfo (coerce sid) NothingYet
    _ -> return ()

-- | Specify the path to save the data according to the bot name.
savedDataPath :: BotName -> FilePath
savedDataPath (BotName Nothing)  = "savedData"
savedDataPath (BotName (Just n)) = "savedData-" ++ n

makeHeader :: MonadReadable AllData m => m (Maybe Headers)
makeHeader = do
  sid <- queries (fmap selfId . selfInfo . otherdata)
  return $ cqhttpHeaders <$> coerce @_ @(Maybe Int) sid

increaseAbsoluteId :: Monad m => EffT '[SModule OtherData] NoError m Int
increaseAbsoluteId = do
  modifyS $ \other_data -> let mid = message_number other_data in other_data {message_number = mid + 1}
  getsS message_number

updateSavedAdditionalData :: (MonadModifiable AllData m) => m ()
updateSavedAdditionalData = change $ \ad ->
  let od = otherdata ad
      sd = savedData od
      rd = runningData od
      sd' = sd { savedAdditional = coerce filterSavedAdditional rd } `using` rseqSavedData
  in ad { otherdata = od { savedData = sd' } }

-- The following should be listed as a separate module that is referenced by all bot command modules.
updateAllDataByMessage :: CQMessage -> AllData -> AllData
updateAllDataByMessage cqmsg (AllData whole_chat m other_data) =
  case eventType cqmsg of
    GroupMessage -> case groupId cqmsg of
      Just gid -> AllData
        (updateListByFuncKeyElement whole_chat id (attachRule cqmsg) (GroupChat gid) cqmsg)
        m
        other_data
      Nothing -> AllData whole_chat m other_data

    PrivateMessage -> case userId cqmsg of
      Just uid -> AllData
        (updateListByFuncKeyElement whole_chat id (attachRule cqmsg) (PrivateChat uid) cqmsg)
        m
        other_data
      Nothing -> AllData whole_chat m other_data

    Response -> let (rdata, mecho) = (responseData cqmsg, echoR cqmsg) in
      case rdata of
        Nothing     -> AllData whole_chat m other_data
        Just rsdata -> updateAllDataByResponse (rsdata, mecho) (AllData whole_chat m other_data)
    _ -> AllData whole_chat m other_data


updateAllDataByResponse :: (ResponseData, Maybe Text) -> AllData -> AllData
updateAllDataByResponse (rdata, mecho) alldata =
  case (message_id rdata, (sent_messages . otherdata) alldata) of
    (Nothing,_) -> alldata
    (_, []) -> alldata
    (Just mid, sentMessageList@(headSentMessageList:_)) ->
      let m0 = fromMaybe headSentMessageList $ listToMaybe [ m | m <- sentMessageList, echoR m == mecho ]
          ms = filter (/= m0) sentMessageList
      in
      case (groupId m0, userId m0) of -- attach to the message id that the bot is replying to
        (Just gid, _) -> alldata {wholechat = updateListByFuncKeyElement (wholechat alldata) id (attachRule m0) (GroupChat gid) m0{messageId = Just mid}, otherdata = (otherdata alldata){sent_messages = ms}}
        (Nothing, Just uid) -> alldata {wholechat = updateListByFuncKeyElement (wholechat alldata) id (attachRule m0) (PrivateChat uid) m0{messageId = Just mid}, otherdata = (otherdata alldata){sent_messages = ms}}
        _ -> alldata

type End a = a -> a
-- | The element will be put into the forest with the correct key, and inserted into a tree determined by the attachTo function.
-- and also put at the top of the list.
updateListByFuncKeyElement :: (Ord k)
  => [ ( k
       , ( [Tree a], [a] )
       )
     ]
  -> End [ (k, ([Tree a], [a])) ]
  -> Maybe (a -> Bool)
  -> k
  -> a
  -> [ (k, ([Tree a], [a])) ]
updateListByFuncKeyElement [] past _ key element = (key, ([Node element []], [element])) : past []
updateListByFuncKeyElement (l: !ls) past attachTo key element
  | keyl == key   =  ( keyl
                     , ( strictTake forestSizeForEachChat $ putElementIntoForest attachTo element treel
                       , strictTake forestSizeForEachChat $ element : list
                       ) `using` evalTuple2 rseq rseq -- when eval this tuple, evaluate the two lists
                     ) : let !pastls = past ls in pastls
  | otherwise     = updateListByFuncKeyElement ls (past . (l:)) attachTo key element
  where (keyl, (treel, list)) = l

-- | Helper function to put an element into a forest according to the attachTo function.
putElementIntoForest :: Maybe (a -> Bool) -> a -> [Tree a] -> [Tree a]
putElementIntoForest attachTo element forest = case attachTo of
    Nothing -> Node element []:forest
    Just f -> let (before, rest) = break (any f . flattenTree) forest
              in case rest of
                   (tree:after) -> let modifiedTree = attachToFirstNode f element tree
                                   in  modifiedTree : before ++ after
                   []           -> Node element []:forest

attachToFirstNode :: (a -> Bool) -> a -> Tree a -> Tree a
attachToFirstNode f element (Node a ts)
  | f a       = Node a (Node element [] : ts)
  | otherwise = Node a (map (attachToFirstNode f element) ts)
attachToFirstNode _ _ EmptyTree = trace "attempt to attach to an empty tree, this should not happen" EmptyTree

-- | Attach if the providing message is a reply to the message in the chat history (by detecting the message id and the replyTo field in meta message of the message)
-- return false if the message in history is not a reply to the providing message, or does not have a message id.
attachRule :: CQMessage -> Maybe (CQMessage -> Bool)
attachRule msg1 = do
  mtm <- metaMessage msg1
  repId <- MP.replyTo mtm
  return $ (Just repId ==) . messageId

-- | This will put meowmeow's response into the chat history and increase the message number (absolute id)
-- also updates the tvarSCQmsg to notify all the modules that a message has been sent.
insertMyResponseHistory :: MonadIO m => ChatId -> MetaMessage -> EffT '[SModule OtherData, RecvSentCQ] NoError m ()
insertMyResponseHistory (GroupChat gid) meta = do
  utc <- liftIO getCurrentTime
  other_data <- getS
  let my = emptyCQMessage
            { eventType   = SelfMessage
            , utcTime     = Just utc
            , absoluteId  = Just aid
            , groupId     = Just gid
            , metaMessage = Just meta
            , echoR       = Just $ pack $ show aid
            } `using` rdeepseq
      (aid, other') = ( message_number other_data + 1, other_data {message_number = message_number other_data + 1} )
  putS $ other' & _sent_messages .~ (my:filter (withInDate utc) (sent_messages other_data) `using` evalList rseq)
  tvarSCQmsg <- asksModule meowSentCQ
  liftIO . atomically $ writeTVar tvarSCQmsg $ Just $ SentCQMessage my
insertMyResponseHistory (PrivateChat uid) meta = do
  utc <- liftIO getCurrentTime
  other_data <- getS
  let my = emptyCQMessage
            { eventType   = SelfMessage
            , utcTime     = Just utc
            , absoluteId  = Just aid
            , userId      = Just $ coerce uid
            , metaMessage = Just meta
            , echoR       = Just $ pack $ show aid
            } `using` rdeepseq
      (aid, other') = ( message_number other_data + 1, other_data {message_number = message_number other_data + 1} )
  putS $ other' & _sent_messages .~ (my:filter (withInDate utc) (sent_messages other_data) `using` evalList rseq)
  tvarSCQmsg <- asksModule meowSentCQ
  liftIO . atomically $ writeTVar tvarSCQmsg $ Just $ SentCQMessage my

-- | Filter out the messages that are not within 60 seconds of the current time.
withInDate :: UTCTime -> CQMessage -> Bool
withInDate utc cqmsg = case utcTime cqmsg of
  Just utc' -> diffUTCTime utc utc' < 60
  Nothing -> False
