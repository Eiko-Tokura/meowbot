{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module MeowBot.BotStructure where

import MeowBot.CQCode
import Control.Monad.Trans.State as ST
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.ReaderState as RST
import Command.Aokana.Scripts
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, pack, unpack)
import Data.Ord (comparing)
import Control.Monad 
import Data.List (maximumBy)
import Data.Aeson (object, FromJSON(..), ToJSON, toJSON, withObject, (.=))
import Data.Aeson.Types (Parser, (.:?))
import GHC.Generics (Generic)
import External.ChatAPI 
import MonParserF (MetaMessage(..), cqmsg, Tree(..), flatten)
import qualified MonParserF as MP
import Debug.Trace

newtype UserId = UserId Int deriving (Eq, Show, Ord, Read)

data ChatId = GroupId Int | PrivateId Int deriving (Eq, Show, Ord, Read)

type Chat = [MP.Tree CQMessage]

data BotAction = BASendPrivate ChatId Text | BASendGroup ChatId Text deriving Show
-- | BAReplyGroup Int Int | BAReplyPrivate Int Int | BASendImagePrivate ImagePath Int | BASendImageGroup ImagePath Int -- | ...More actions, like Poke, ... etc

type ChatRoom = (ChatId, Chat)

type WholeChat = [ChatRoom] 

data AllData = AllData
  { wholechat :: WholeChat
  , otherdata :: OtherData
  } deriving Show

data OtherData = OtherData 
  { message_number :: Int -- all messages, will be used to create an absolute message id number ordered by time of receipt or time of send.
  , sent_messages :: [CQMessage]
    -- In the future one can add course data.. etc
  , savedData     :: SavedData
  , aokana        :: [ScriptBlock]
  } deriving (Show)

data SavedData = SavedData
  { sysMessages :: [(ChatId, Message)]
  , allowedGroups :: [Int]
  , allowedUsers  :: [UserId]
  , deniedUsers   :: [UserId]
  , adminUsers    :: [UserId]
  } deriving (Show, Eq, Read)

initialAdminUsers = UserId <$> [754829466] -- my qq number

initialAllowedGroups :: [Int]
initialAllowedGroups = [437447251] -- my qq group number

initialAllowedUsers = UserId <$> [754829466]

initialDeniedUsers = UserId <$> []

data CQEventType = GroupMessage | PrivateMessage | Response | HeartBeat | SelfMessage | UnknownMessage
  deriving (Show, Eq, Read)

data CQMessage = CQMessage
  { eventType    :: CQEventType
  , messageId    :: Maybe Int
  , groupId      :: Maybe Int
  , userId       :: Maybe Int
  , message      :: Maybe Text
  , time         :: Maybe Int
  , responseData :: Maybe ResponseData
  , echoR        :: Maybe Text
  , absoluteId   :: Maybe Int
  , metaMessage  :: Maybe MetaMessage
  } deriving (Show, Read, Generic)

emptyCQMessage = CQMessage UnknownMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

newtype ResponseData = ResponseData
  { message_id :: Maybe Int
  } deriving (Show, Read, Generic)

instance FromJSON ResponseData where
  parseJSON = withObject "ResponseData" $ \o -> do
    messageid <- o .:? "message_id"
    return ResponseData { message_id = messageid }

instance FromJSON CQMessage where
  parseJSON = withObject "CQMessage" $ \obj -> do
    postType <- obj .:? "post_type" :: Parser (Maybe Text)
    messageType <- obj .:? "message_type" :: Parser (Maybe Text)
    metaEventType <- obj .:? "meta_event_type" :: Parser (Maybe Text)
    dataObj <- obj .:? "data"
    message <- obj .:? "raw_message" 
    strmsg <- obj .:? "raw_message" :: Parser (Maybe String)
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
              <*> pure message
              <*> obj .:? "time"
              <*> pure dataObj
              <*> obj .:? "echo"
              <*> pure Nothing
              <*> pure ( case message of 
                Nothing -> Nothing
                Just msgl -> let lcqmsg = MP.runParserF cqmsg $ fromMaybe "" strmsg in listToMaybe $ map fst lcqmsg ) 

showCQ :: CQMessage -> String
showCQ cqmsg = concat [absId, messageType, " ",  chatId, senderId, ": ", messageContent]
  where messageType = show $ eventType cqmsg
        absId = maybe "" (\c -> "[" <> show c <> "] ") . absoluteId $ cqmsg
        chatId = maybe "" show . groupId $ cqmsg
        senderId = maybe "" (\c -> "(" ++ show c ++ ")") . userId $ cqmsg
        messageContent = maybe "" unpack $ message cqmsg

saveData :: AllData -> StateT AllData IO () -- if savedData changed, save it to file
saveData prev_data = do
  new_data <- ST.get
  lift $ when (savedData (otherdata new_data) /= savedData (otherdata prev_data)) do
    putStrLn "Saved data changed, I'm saving it to file! owo"
    writeFile savedDataPath $ show $ savedData (otherdata new_data)

savedDataPath = "savedData"

increaseAbsoluteId :: (Monad m) => StateT AllData m Int
increaseAbsoluteId = globalize wholechat otherdata AllData $ do
  other_data <- RST.get
  let mid = message_number other_data
  RST.put $ other_data {message_number = mid + 1}
  return $ mid + 1

--increaseAbsoluteId :: OtherData -> (Int, OtherData)
--increaseAbsoluteId otherdata = let mid = message_number otherdata in (mid + 1, otherdata {message_number = mid + 1})

data SendMessageForm = SendMessageForm {
  action :: Text,
  params :: Params,
  echo   :: Maybe Text
} deriving (Generic, Show)

data Params = PrivateParams {
  user_id     :: Int,
  messageText :: Text
} | GroupParams {
  group_id    :: Int,
  messageText :: Text
} deriving (Show)

instance ToJSON Params where
  toJSON (PrivateParams uid msg) =
    object [ "user_id" .= uid
           , "message" .= msg
           ]
  toJSON (GroupParams gid msg) =
    object [ "group_id" .= gid
           , "message" .= msg
           ]

instance ToJSON SendMessageForm

-- The following should be listed as a separate module that is referenced by all bot command modules.

updateAllDataByMessage :: CQMessage -> AllData -> AllData
updateAllDataByMessage cqmsg (AllData whole_chat other_data) = 
  case eventType cqmsg of
    GroupMessage -> case groupId cqmsg of
      Just gid -> AllData
        (updateListByFuncKeyElement whole_chat [] (attachRule cqmsg) (GroupId gid) cqmsg)  --let (before, rest) = break (\rm -> (chatid rm) == gid ) in 
        other_data
      Nothing -> AllData whole_chat other_data

    PrivateMessage -> case userId cqmsg of
      Just uid -> AllData
        (updateListByFuncKeyElement whole_chat [] (attachRule cqmsg) (PrivateId uid) cqmsg)  --let (before, rest) = break (\rm -> (chatid rm) == gid ) in 
        other_data
      Nothing -> AllData whole_chat other_data

    Response -> let rdata = responseData cqmsg in
      case rdata of
        Nothing     -> AllData whole_chat other_data
        Just rsdata -> updateAllDataByResponse rsdata (AllData whole_chat other_data)
    _ -> AllData whole_chat other_data

updateAllDataByResponse :: ResponseData -> AllData -> AllData
updateAllDataByResponse rdata alldata = 
  case (message_id rdata, (sent_messages . otherdata) alldata) of
    (Nothing,_) -> alldata
    (_, []) -> alldata
    (Just mid, m0:ms) -> case (groupId m0, userId m0) of
      (Just gid, _) -> alldata {wholechat = updateListByFuncKeyElement (wholechat alldata) [] (attachRule m0) (GroupId gid) m0{messageId = Just mid}, otherdata = (otherdata alldata){sent_messages = ms}}
      (Nothing, Just uid) -> alldata {wholechat = updateListByFuncKeyElement (wholechat alldata) [] (attachRule m0) (PrivateId uid) m0{messageId = Just mid}, otherdata = (otherdata alldata){sent_messages = ms}}
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
putElementIntoForest attachTo element forest = take 200 $ 
  case attachTo of
    Nothing -> Node element []:forest
    Just f -> let (before, rest) = break (any f . flatten) forest
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
    (p:ps) -> case p of
      (_, []) -> Node emptyCQMessage []
      (_, t0:ts) -> t0

type MessageId = Int
type EssentialContent = (String, ChatId, UserId, MessageId)

getEssentialContent :: WholeChat -> Maybe EssentialContent
getEssentialContent wchat = let cqmsg = getNewMsg wchat in
  (,,,) <$> (fmap onlyMessage . metaMessage $ cqmsg)
        <*> (case eventType cqmsg of
              GroupMessage -> GroupId <$> groupId cqmsg
              PrivateMessage -> PrivateId <$> userId cqmsg
              _ -> Nothing
            )
        <*> (UserId <$> userId cqmsg)
        <*> messageId cqmsg
 
mT :: (Applicative t) => Maybe (t a) -> t (Maybe a)
mT Nothing = pure Nothing
mT (Just ioa) = Just <$> ioa 

baSendToChatId :: ChatId -> Text -> BotAction
baSendToChatId (GroupId gid) txt = BASendGroup (GroupId gid) txt
baSendToChatId (PrivateId gid) txt = BASendPrivate (PrivateId gid) txt

sendIOeToChatId :: EssentialContent -> ExceptT String IO String -> OtherData -> IO ([BotAction], OtherData)
sendIOeToChatId (_, cid, _, mid) ioess other_data = do
  ess <- runExceptT ioess
  return ( [either 
               (baSendToChatId cid . pack . ("喵~出错啦：" ++ )) 
               (baSendToChatId cid . pack) ess 
             ], 
              either 
                (const other_data)
                (\str -> insertMyResponse other_data cid (generateMetaMessage str [MReplyTo mid]) ) ess
           )

sendToChatId :: EssentialContent -> String -> OtherData -> ([BotAction], OtherData)
sendToChatId (_, cid, _, mid) str other_data = 
  ([baSendToChatId cid (pack str)], insertMyResponse other_data cid (generateMetaMessage str [MReplyTo mid]) )

insertMyResponse :: OtherData -> ChatId -> MetaMessage -> OtherData
insertMyResponse other_data (GroupId gid) meta = 
  other' { sent_messages = my:sent_messages other_data } where 
    my = emptyCQMessage {eventType = SelfMessage, absoluteId = Just aid, groupId = Just gid, metaMessage = Just meta}
    (aid, other') = ( message_number other_data + 1, other_data {message_number = message_number other_data + 1} )
insertMyResponse other_data (PrivateId uid) meta = 
  other' { sent_messages = my:sent_messages other_data } where 
    my = emptyCQMessage {eventType = SelfMessage, absoluteId = Just aid, userId = Just uid, metaMessage = Just meta}
    (aid, other') = ( message_number other_data + 1, other_data {message_number = message_number other_data + 1} )

data MetaMessageItem = MCQCode CQCode | MReplyTo MessageId | MSysMessage Message
generateMetaMessage :: String -> [MetaMessageItem] -> MetaMessage
generateMetaMessage str items = MetaMessage 
  { onlyMessage = str
  , cqcodes     = [cqcode | MCQCode cqcode <- items]
  , replyTo     = listToMaybe [mid | MReplyTo mid <- items]
  , withSystemMessage = listToMaybe [msg | MSysMessage msg <- items]
  }

