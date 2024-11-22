{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module MeowBot.BotStructure
  ( BotCommand(..), BotModules(..), CommandValue
  , GroupId(..), UserId(..), ChatId(..), Chat, MessageId, BotName
  , BotAction(..), ChatRoom, WholeChat, AllData(..), OtherData(..), SavedData(..)
  , UserGroup(..), GroupGroup(..)
  , SendMessageForm(..), Params(..)
  , MetaMessageItem(..)
  , showCQ, saveData, savedDataPath
  , gIncreaseAbsoluteId, increaseAbsoluteId
  , updateAllDataByMessage, updateAllDataByResponse, insertMyResponseHistory, generateMetaMessage

  , CQMessage(..), ResponseData(..), CQEventType(..)
  , emptyCQMessage

  , EssentialContent
  , cqmsgToEssentialContent
  , getEssentialContent, getEssentialContentAtN, sendIOeToChatId, sendToChatId, baSendToChatId, baSendToChatIdFull
  , getFirstTree, getNewMsg, getNewMsgN, getTimeLine
  , mT

  , rseqWholeChat
  ) where

import MeowBot.CQCode
import MeowBot.CommandRule
import MeowBot.Data.Book
import Control.Parallel.Strategies
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.ReaderState as RST
import Command.Aokana.Scripts
import Data.Coerce
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, pack, unpack)
import Data.Ord (comparing, Down(..))
import Data.List (maximumBy, sortOn)
import Data.Aeson (object, FromJSON(..), ToJSON, toJSON, withObject, (.=), withText)
import Data.Aeson.Types (Parser, (.:?))
import Data.Additional
import GHC.Generics (Generic)
import External.ChatAPI
import MeowBot.Parser (MetaMessage(..), cqmsg, Tree(..), flattenTree)
import qualified MeowBot.Parser as MP
import Debug.Trace

--import Database.Persist

type BotName = Maybe String

data ChatId = GroupChat GroupId | PrivateChat UserId
  deriving (Show, Eq, Ord, Read, Generic, NFData)

type Chat = [MP.Tree CQMessage]

forestSizeForEachChat = 200

type ChatRoom = (ChatId, Chat)

type WholeChat = [ChatRoom]  -- [(ChatId, [Tree CQMessage])]

data BotAction
  = BASendPrivate
    UserId       -- ^ the user to send to
    Text         -- ^ Text, the message to send
  | BASendGroup
    GroupId      -- ^ the group chat to send to
    Text         -- ^ Text, the message to send
  | BARetractMsg
    MessageId    -- ^ MessageId, the message to delete (retract)
  deriving Show

data AllData = AllData
  { wholechat :: WholeChat
  , otherdata :: OtherData
  } deriving Show

rseqWholeChat :: Strategy AllData
rseqWholeChat (AllData wc od) = do
  wc' <- evalList (evalTuple2 r0 rseq) wc
  od' <- rseq od
  return $ AllData wc' od'

type CommandValue = ReaderStateT WholeChat OtherData IO [BotAction]
-- data ReaderStateT r s m a = ReaderStateT {runReaderStateT :: r -> s -> m (a, s)}
-- CommandValue is a monadic value of the monad (ReaderStateT WholeChat OtherData IO)

data BotCommand = BotCommand
  { identifier :: CommandId
  , command    :: CommandValue
  }

data BotModules = BotModules
  { canUseGroupCommands   :: [CommandId]
  , canUsePrivateCommands :: [CommandId]
  , nameOfBot :: BotName
  , globalSysMsg :: Maybe String
  } deriving (Show)

data OtherData = OtherData -- In the future one can add course data.. etc
  { message_number :: !Int -- ^ all messages, will be used to create an absolute message id number ordered by time of receipt or time of send.
  , sent_messages :: ![CQMessage]
  , savedData     :: !SavedData
  , botModules    :: !BotModules
  , runningData   :: ![AdditionalData] -- ^ additional data that is running, not saved.
  , aokana        :: [ScriptBlock]
  } deriving (Show)

instance HasAdditionalData OtherData where
  getAdditionalData = runningData
  modifyAdditionalData f od = od {runningData = f $ runningData od}

data SavedData = SavedData
  { chatSettings :: [(ChatId, ChatSetting)]
  , userGroups   :: [(UserId, UserGroup)]
  , groupGroups  :: [(GroupId, GroupGroup)]
  , commandRules :: [CommandRule]
  , books        :: [Book]
  } deriving (Show, Eq, Read)

data CQEventType = GroupMessage | PrivateMessage | Response | HeartBeat | SelfMessage | UnknownMessage
  deriving (Show, Eq, Read, Generic, NFData)

data CQMessage = CQMessage
  { eventType    :: CQEventType
  , messageId    :: Maybe Int
  , groupId      :: Maybe GroupId
  , userId       :: Maybe UserId
  , sender       :: Maybe Sender
  , message      :: Maybe Text
  , time         :: Maybe Int
  , responseData :: Maybe ResponseData
  , echoR        :: Maybe Text
  , absoluteId   :: Maybe Int
  , metaMessage  :: Maybe MetaMessage
  } deriving (Show, Eq, Generic, NFData)

instance HasAdditionalData CQMessage where
  getAdditionalData = maybe [] additionalData . metaMessage
  modifyAdditionalData f cqmsg = cqmsg {metaMessage = modifyAdditionalData f <$> metaMessage cqmsg}

data Sender = Sender
  { nickname :: Maybe Text
  , card     :: Maybe Text
  , role     :: Maybe Role
  } deriving (Show, Read, Eq, Generic, FromJSON, NFData)

data Role = ROwner | RAdmin | RMember | RUnknown
  deriving (Show, Read, Eq, Generic, NFData)

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "owner" -> return ROwner
    "admin" -> return RAdmin
    "member" -> return RMember
    _ -> return RUnknown

emptyCQMessage = CQMessage UnknownMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newtype ResponseData = ResponseData
  { message_id :: Maybe Int
  } deriving (Show, Read, Eq, NFData) via (Maybe Int)
    deriving (Generic)

instance FromJSON ResponseData where
  parseJSON = withObject "ResponseData" $ \o -> do
    messageid <- o .:? "message_id"
    return ResponseData { message_id = messageid }

instance FromJSON CQMessage where
  parseJSON = withObject "CQMessage" $ \obj -> do
    postType      <- obj .:? "post_type"       :: Parser (Maybe Text)
    messageType   <- obj .:? "message_type"    :: Parser (Maybe Text)
    metaEventType <- obj .:? "meta_event_type" :: Parser (Maybe Text)
    dataObj       <- obj .:? "data"
    message       <- obj .:? "raw_message"
    strmsg        <- obj .:? "raw_message" :: Parser (Maybe Text)
    let eventType = case (postType, metaEventType, messageType, dataObj) of
          (Just "message", _,  Just "private", _) -> PrivateMessage
          (Just "message", _, Just "group", _) -> GroupMessage
          (Nothing, _, Nothing, Just _) -> Response
          (Just "meta_event", Just "heartbeat", Nothing, Nothing) -> HeartBeat
          _ -> UnknownMessage
    CQMessage <$> pure eventType
              <*> obj .:? "message_id"
              <*> obj .:? "group_id"
              <*> obj .:? "user_id"
              <*> obj .:? "sender"
              <*> pure message
              <*> obj .:? "time"
              <*> pure dataObj
              <*> obj .:? "echo"
              <*> pure Nothing
              <*> pure ( case message of
                Nothing -> Nothing
                Just _ -> MP.runParser cqmsg $ fromMaybe "" strmsg )

showCQ :: CQMessage -> String
showCQ cqmsg = concat [absId, messageType, " ",  chatId, senderId, ": ", messageContent, mcqcodes]
  where messageType    = show $ eventType cqmsg
        absId          = maybe "" (\c -> "[" <> show c <> "] ") . absoluteId $ cqmsg
        chatId         = maybe "" show . groupId $ cqmsg
        senderId       = maybe "" (\c -> "(" ++ show c ++ ")") . userId $ cqmsg
        messageContent = maybe "" unpack $ message cqmsg
        mcqcodes       = maybe "" (("\n"++) . show) $ mNonEmpty . cqcodes =<< metaMessage cqmsg
        mNonEmpty []   = Nothing
        mNonEmpty l    = Just l

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

type MessageId = Int
type EssentialContent = (Text, ChatId, UserId, MessageId)

getEssentialContent :: WholeChat -> Maybe EssentialContent
getEssentialContent wchat = cqmsgToEssentialContent (getNewMsg wchat)

getEssentialContentAtN :: Int -> WholeChat -> Maybe EssentialContent
getEssentialContentAtN n wchat = cqmsgToEssentialContent =<< (getNewMsgN n wchat !? (n-1))
  where (!?) :: [a] -> Int -> Maybe a
        (!?) [] _ = Nothing
        (!?) (x:_) 0 = Just x
        (!?) (_:xs) n = xs !? (n-1)

-- | get the timeline of the most recent chat
getTimeLine :: WholeChat -> [CQMessage]
getTimeLine = sortOn (Down . time) . flattenTree . getFirstTree

cqmsgToEssentialContent :: CQMessage -> Maybe EssentialContent
cqmsgToEssentialContent cqmsg =
  (,,,) <$> (fmap onlyMessage . metaMessage $ cqmsg)
        <*> (case eventType cqmsg of
              GroupMessage -> GroupChat <$> groupId cqmsg
              PrivateMessage -> PrivateChat <$> userId cqmsg
              _ -> Nothing
            )
        <*> userId cqmsg
        <*> messageId cqmsg

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

data MetaMessageItem = MCQCode CQCode | MReplyTo MessageId | MChatSetting ChatSetting
generateMetaMessage :: Text -> [AdditionalData] -> [MetaMessageItem] -> MetaMessage
generateMetaMessage str adt items = MetaMessage
  { onlyMessage = str
  , cqcodes     = [cqcode | MCQCode cqcode <- items]
  , replyTo     = listToMaybe [mid | MReplyTo mid <- items]
  , withChatSetting = listToMaybe [set | MChatSetting set <- items]
  , additionalData = adt
  }

