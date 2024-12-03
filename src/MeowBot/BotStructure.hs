{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module MeowBot.BotStructure
  ( module MeowBot.Data
  , Meow
  , BotCommand(..), BotModules(..), CommandValue
  , GroupId(..), UserId(..), ChatId(..)
  , BotAction(..), AllData(..), OtherData(..), SavedData(..), Saved(..)
  , UserGroup(..), GroupGroup(..)
  , SendMessageForm(..), Params(..)
  , MetaMessageItem(..)
  , saveData, savedDataPath
  , gIncreaseAbsoluteId, increaseAbsoluteId
  , updateAllDataByMessage, updateAllDataByResponse, insertMyResponseHistory, updateSavedAdditionalData

  , CQMessage(..), ResponseData(..), CQEventType(..)

  , getEssentialContent, getEssentialContentAtN
  , sendIOeToChatId, sendToChatId
  , sendIOeToChatIdAsync
  , baSendToChatId, baSendToChatIdFull
  , getFirstTree, getNewMsg, getNewMsgN
  , getTimeLine, getTimeLineCid
  , mT

  , rseqWholeChat

  , Async, async
  ) where

import MeowBot.CommandRule
import MeowBot.Data
import Control.Parallel.Strategies
import Control.Concurrent.Async (Async, asyncThreadId, async)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.ReaderState as RST
import Command.Aokana.Scripts
import Data.Coerce
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing, Down(..))
import Data.List (maximumBy, sortOn)
import qualified Data.Set as S
import Data.Aeson (object, ToJSON, toJSON, (.=))
import Data.Additional
import Data.Additional.Saved
import GHC.Generics (Generic)
import MeowBot.Parser (Tree(..), flattenTree)
import MeowBot.Parser (ChatSetting(..))
import qualified MeowBot.Parser as MP
import MeowBot.Data.Book
import Debug.Trace
--import Database.Persist -- implement proper database later

data BotAction
  = BASendPrivate
      UserId     -- ^ the user to send to
      Text       -- ^ Text, the message to send
  | BASendGroup
      GroupId    -- ^ the group chat to send to
      Text       -- ^ Text, the message to send
  | BARetractMsg
      MessageId  -- ^ MessageId, the message to delete (retract)
  | BAAsync
      (Async (Meow [BotAction])) -- ^ the action to run asynchronously, which allows much powerful even continuously staged actions.
  | BAPureAsync
      (Async [BotAction]) -- ^ the action to run asynchronously, which is pure and will not further read or modify the data.

type Meow a = ReaderStateT WholeChat OtherData IO a 
-- ^ the monad that the bot runs in
-- running in this monad it is necessary to block other threads from modifying the data.
-- so avoid running long blocking operations in this monad, use async and staged actions instead.

instance Show (Async (Meow [BotAction])) where
  show a = "Async (Meow BotAction) " ++ show (asyncThreadId a)

forestSizeForEachChat = 256 -- ^ controls how many trees to keep in each chat room

data AllData = AllData
  { wholechat :: WholeChat
  , otherdata :: OtherData
  } deriving Show

data OtherData = OtherData -- In the future one can add course data.. etc
  { message_number :: !Int -- ^ all messages, will be used to create an absolute message id number ordered by time of receipt or time of send.
  , sent_messages  :: ![CQMessage]
  , savedData      :: !SavedData
  , botModules     :: !BotModules
  , runningData    :: ![AdditionalData]  -- ^ additional data that is running, not saved.
  , asyncActions   :: !(S.Set (Async (Meow [BotAction]))) -- ^ actions that are running asynchronously
  , aokana         :: [ScriptBlock]
  } deriving Show

data SavedData = SavedData
  { chatSettings    :: [(ChatId, ChatSetting)]
  , userGroups      :: [(UserId, UserGroup)]
  , groupGroups     :: [(GroupId, GroupGroup)]
  , commandRules    :: [CommandRule]
  , books           :: [Book]
  , savedAdditional :: [Saved AdditionalData]
  } deriving (Show, Eq, Read)

updateSavedAdditionalData :: (Monad m) => StateT AllData m ()
updateSavedAdditionalData = do
  ad <- ST.get
  let od = otherdata ad
      sd = savedData od
      rd = runningData od
      sd' = sd {savedAdditional = coerce filterSavedAdditional rd}
  ST.put ad { otherdata = od { savedData = sd' } }

type CommandValue = Meow [BotAction]
-- data ReaderStateT r s m a = ReaderStateT {runReaderStateT :: r -> s -> m (a, s)}
-- CommandValue is a monadic value of the monad (Meow)

data BotCommand = BotCommand
  { identifier :: CommandId
  , command    :: CommandValue
  }

instance HasAdditionalData OtherData where
  getAdditionalData = runningData
  modifyAdditionalData f od = od {runningData = f $ runningData od}

rseqWholeChat :: Strategy AllData
rseqWholeChat (AllData wc od) = do
  wc' <- evalList (evalTuple2 r0 rseq) wc
  od' <- rseq od
  return $ AllData wc' od'

-- | if savedData changed, save it to file
saveData :: AllData -> StateT AllData IO ()
saveData prev_data = do
  new_data <- ST.get
  lift $ when (savedData (otherdata new_data) /= savedData (otherdata prev_data)) do
    putStrLn "Saved data changed, I'm saving it to file! owo"
    writeFile (savedDataPath $ nameOfBot $ botModules $ otherdata new_data) $ show $ savedData (otherdata new_data)

savedDataPath :: BotName -> FilePath
savedDataPath Nothing = "savedData"
savedDataPath (Just n) = "savedData-" ++ n

gIncreaseAbsoluteId :: (Monad m) => StateT AllData m Int
gIncreaseAbsoluteId = globalize wholechat otherdata AllData increaseAbsoluteId

increaseAbsoluteId :: (Monad m) => ReaderStateT r OtherData m Int
increaseAbsoluteId = do
  other_data <- RST.get
  let mid = message_number other_data
  RST.put $ other_data {message_number = mid + 1}
  return $ mid + 1

data SendMessageForm = SendMessageForm
  { action :: Text
  , params :: Params
  , echo   :: Maybe Text
  } deriving (Generic, Show, ToJSON)

data Params
  = PrivateParams
    { user_id     :: UserId
    , messageText :: Text
    }
  | GroupParams
    { group_id    :: GroupId
    , messageText :: Text
    }
  | DeleteParams
    { messageIdDelete  :: MessageId
    }
  deriving (Show)

instance ToJSON Params where
  toJSON (PrivateParams uid msg) =
    object [ "user_id" .= uid
           , "message" .= msg
           ]
  toJSON (GroupParams gid msg) =
    object [ "group_id" .= gid
           , "message" .= msg
           ]
  toJSON (DeleteParams mid) =
    object [ "message_id" .= mid
           ]

-- The following should be listed as a separate module that is referenced by all bot command modules.
updateAllDataByMessage :: CQMessage -> AllData -> AllData
updateAllDataByMessage cqmsg (AllData whole_chat other_data) =
  case eventType cqmsg of
    GroupMessage -> case groupId cqmsg of
      Just gid -> AllData
        (updateListByFuncKeyElement whole_chat [] (attachRule cqmsg) (GroupChat gid) cqmsg)
        other_data
      Nothing -> AllData whole_chat other_data

    PrivateMessage -> case userId cqmsg of
      Just uid -> AllData
        (updateListByFuncKeyElement whole_chat [] (attachRule cqmsg) (PrivateChat uid) cqmsg)
        other_data
      Nothing -> AllData whole_chat other_data

    Response -> let (rdata, mecho) = (responseData cqmsg, echoR cqmsg) in
      case rdata of
        Nothing     -> AllData whole_chat other_data
        Just rsdata -> updateAllDataByResponse (rsdata, mecho) (AllData whole_chat other_data)
    _ -> AllData whole_chat other_data


updateAllDataByResponse :: (ResponseData, Maybe Text) -> AllData -> AllData
updateAllDataByResponse (rdata, mecho) alldata =
  case (message_id rdata, (sent_messages . otherdata) alldata) of
    (Nothing,_) -> alldata
    (_, []) -> alldata
    (Just mid, sentMessageList@(headSentMessageList:_)) ->
      let m0 = fromMaybe headSentMessageList $ listToMaybe [ m | m <- sentMessageList, echoR m == mecho ]
          ms = filter (/= m0) sentMessageList
      in
      case (groupId m0, userId m0) of
        (Just gid, _) -> alldata {wholechat = updateListByFuncKeyElement (wholechat alldata) [] (attachRule m0) (GroupChat gid) m0{messageId = Just mid}, otherdata = (otherdata alldata){sent_messages = ms}}
        (Nothing, Just uid) -> alldata {wholechat = updateListByFuncKeyElement (wholechat alldata) [] (attachRule m0) (PrivateChat uid) m0{messageId = Just mid}, otherdata = (otherdata alldata){sent_messages = ms}}
        _ -> alldata

attachRule :: CQMessage -> Maybe (CQMessage -> Bool)
attachRule msg1 =
  case do {mtm <- metaMessage msg1; MP.replyTo mtm} of
    Nothing -> Nothing
    Just id -> Just (\m -> case messageId m of
      Nothing -> False
      Just mid -> mid == id)

updateListByFuncKeyElement :: (Eq k) => [(k, [Tree a])] -> [(k, [Tree a])] -> Maybe (a -> Bool) -> k -> a -> [(k, [Tree a])]
updateListByFuncKeyElement [] past _ key element = (key, [Node element []]) : reverse past
updateListByFuncKeyElement (l:ls) past attachTo key element
  | keyl == key   =  (keyl, putElementIntoForest attachTo element treel) : reverse past ++ ls
  | otherwise     = updateListByFuncKeyElement ls (l:past) attachTo key element
  where (keyl, treel) = l

putElementIntoForest :: Maybe (a -> Bool) -> a -> [Tree a] -> [Tree a]
putElementIntoForest attachTo element forest = (`using` evalList rseq) $ take forestSizeForEachChat $
  case attachTo of
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

largestInTree :: (Ord b) => (a -> b) -> Tree a -> (b, a)
largestInTree f (Node a children) =
  let currentValue = (f a, a)
      childValues = map (largestInTree f) children
  in case childValues of
              [] -> currentValue
              _  -> maximumBy (comparing fst) $ currentValue:childValues
largestInTree _ EmptyTree = error "largestInTree : EmptyTree"

getNewMsg :: WholeChat -> CQMessage
getNewMsg [] = emptyCQMessage
getNewMsg wholechat = snd $ largestInTree (fromMaybe 0 . absoluteId) (getFirstTree wholechat)

getFirstTree :: WholeChat -> Tree CQMessage
getFirstTree wc =
  case wc of
    [] -> Node emptyCQMessage []
    (p:_) -> case p of
      (_, []) -> Node emptyCQMessage []
      (_, t0:_) -> t0

getNewMsgN :: Int -> WholeChat -> [CQMessage]
getNewMsgN _ [] = []
getNewMsgN n (headWholeChat:_) = take n $ concatMap flattenTree $ snd (headWholeChat)

getEssentialContent :: WholeChat -> Maybe EssentialContent
getEssentialContent wchat = cqmsgToEssentialContent (getNewMsg wchat)

getEssentialContentAtN :: Int -> WholeChat -> Maybe EssentialContent
getEssentialContentAtN n wchat = cqmsgToEssentialContent =<< (getNewMsgN n wchat !? (n-1))
  where (!?) :: [a] -> Int -> Maybe a
        (!?) [] _ = Nothing
        (!?) (x:_) 0 = Just x
        (!?) (_:xs) n = xs !? (n-1)

-- | get the timeline of the most recent chat, i.e. sort the chat room of the most recent message by time.
getTimeLine :: WholeChat -> [CQMessage]
getTimeLine ((_, forest):_) = sortOn (Down . time) $ concatMap flattenTree forest
getTimeLine [] = []

-- | Get the timeline of a chat id.
getTimeLineCid :: ChatId -> WholeChat -> [CQMessage]
getTimeLineCid cid wc = case lookup cid wc of
  Just forest -> sortOn (Down . time) . concatMap flattenTree $ forest
  Nothing -> []

mT :: (Applicative t) => Maybe (t a) -> t (Maybe a)
mT Nothing = pure Nothing
mT (Just ioa) = Just <$> ioa

-- | Abstract representation of sending a message to a chat id.
baSendToChatId :: ChatId -> Text -> BotAction
baSendToChatId (GroupChat gid)   txt = BASendGroup gid txt
baSendToChatId (PrivateChat uid) txt = BASendPrivate uid txt

-- | runing an ExceptT String IO String action with string result, and send the result to a chat id. Handles exceptions.
sendIOeToChatId :: EssentialContent -> ExceptT Text IO Text -> ReaderStateT r OtherData IO [BotAction]
sendIOeToChatId (_, cid, _, mid) ioess = do
  ess <- lift $ runExceptT ioess
  case ess of
    Right str -> do
      RST.modify $ insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid str ]
    Left err -> return [ baSendToChatId cid ("喵~出错啦：" <> err) ]

sendIOeToChatIdAsync :: EssentialContent -> ExceptT Text IO Text -> IO (Async (Meow [BotAction]))
sendIOeToChatIdAsync (_, cid, _, mid) ioess = async $ do
  ess <- runExceptT ioess
  case ess of
    Right str -> return $ do
      RST.modify $ insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid str ]
    Left err -> return $ return [ baSendToChatId cid ("喵~出错啦：" <> err) ]

-- | send message to a chat id, recording the message as reply.
sendToChatId :: EssentialContent -> Text -> OtherData -> ([BotAction], OtherData)
sendToChatId (_, cid, _, mid) str other_data =
  ([baSendToChatId cid str], insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid]) other_data )

-- | send message to a chat id, recording the message as reply (optional in Maybe MessageId), with additional data and meta items.
-- Also increase the message number (absolute id)
baSendToChatIdFull :: Monad m => ChatId -> Maybe MessageId -> [AdditionalData] -> [MetaMessageItem] -> Text -> ReaderStateT r OtherData m [BotAction]
baSendToChatIdFull cid mid adt items str = do
  let meta = generateMetaMessage str adt ([MReplyTo mid' | Just mid' <- pure mid ] ++ items)
  RST.modify $ insertMyResponseHistory cid meta
  return [ baSendToChatId cid str ]

-- | This will put meowmeow's response into the chat history and increase the message number (absolute id)
insertMyResponseHistory :: ChatId -> MetaMessage -> OtherData -> OtherData
insertMyResponseHistory (GroupChat gid) meta other_data =
  other' { sent_messages = my:sent_messages other_data } where
    my = emptyCQMessage
      { eventType   = SelfMessage
      , absoluteId  = Just aid
      , groupId     = Just gid
      , metaMessage = Just meta
      , echoR       = Just $ pack $ show aid
      }
    (aid, other') = ( message_number other_data + 1, other_data {message_number = message_number other_data + 1} )
insertMyResponseHistory (PrivateChat uid) meta other_data =
  other' { sent_messages = my:sent_messages other_data } where
    my = emptyCQMessage
      { eventType   = SelfMessage
      , absoluteId  = Just aid
      , userId      = Just $ coerce uid
      , metaMessage = Just meta
      , echoR       = Just $ pack $ show aid
      }
    (aid, other') = ( message_number other_data + 1, other_data {message_number = message_number other_data + 1} )

