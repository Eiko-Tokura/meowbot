module MeowBot.Data.CQHttp.CQMessage where

import Data.Aeson
import Data.Aeson.Types (Parser)
import MeowBot.Data.ChatId
import MeowBot.Data.CQHttp.Notice
import MeowBot.Prelude
import MeowBot.MetaMessage
import Data.Additional
import qualified MeowBot.Parser as MP
import Utils.Time

-------------------------------------------------------------------------------------------
-- CQMessage

data CQEventType = GroupMessage | PrivateMessage | Response | HeartBeat | LifeCycle | SelfMessage | UnknownMessage | RequestEvent | NoticeEvent
  deriving (Show, Eq, Read, Generic, NFData, Bounded, Enum)

data CQMessage = CQMessage
  { eventType    :: CQEventType
  , messageId    :: Maybe Int
  , groupId      :: Maybe GroupId
  , userId       :: Maybe UserId   -- ^ user id
  , sender       :: Maybe Sender   -- ^ sender information
  , message      :: Maybe Text     -- ^ raw message
  , time         :: Maybe Int
  , utcTime      :: Maybe UTCTime
  , self_id      :: Maybe Int
  , responseData :: Maybe ResponseData
  , echoR        :: Maybe Text
  , absoluteId   :: Maybe Int
  , metaMessage  :: Maybe MetaMessage
  , noticeType   :: Maybe NoticeType
  , requestType  :: Maybe RequestType
  } deriving (Show, Eq, Generic, NFData)

newtype SentCQMessage = SentCQMessage CQMessage
newtype ReceCQMessage = ReceCQMessage CQMessage

instance HasAdditionalData CQMessage where
  getAdditionalData = maybe [] additionalData . metaMessage
  modifyAdditionalData f cqmsg = cqmsg {metaMessage = modifyAdditionalData f <$> metaMessage cqmsg}

data Sender = Sender
  { senderNickname :: Maybe Text
  , senderCard     :: Maybe Text
  , senderRole     :: Maybe Role
  } deriving (Show, Read, Eq, Generic, NFData)

instance FromJSON Sender where
  parseJSON = withObject "Sender" $ \o -> do
    nickname <- o .:? "nickname"
    card     <- o .:? "card"
    role     <- o .:? "role"
    return Sender { senderNickname = nickname, senderCard = card, senderRole = role }

roleToText :: Role -> Maybe Text
roleToText ROwner   = Just "群主"
roleToText RAdmin   = Just "群管理"
roleToText RMember  = Nothing -- ^ not displaying if it's a member
roleToText RUnknown = Just "未知"

emptyCQMessage :: CQMessage
emptyCQMessage = CQMessage UnknownMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
    timeUnixSec   <- obj .:? "time"            :: Parser (Maybe Int)
    noticeType    <- obj .:? "notice_type"     :: Parser (Maybe Text)
    noticeSubType <- obj .:? "sub_type"        :: Parser (Maybe Text)
    requestType   <- obj .:? "request_type"    :: Parser (Maybe Text)
    requestSubType<- obj .:? "sub_type"        :: Parser (Maybe Text)
    comment       <- obj .:? "comment"         :: Parser (Maybe Text)
    flag          <- obj .:? "flag"            :: Parser (Maybe Flag)
    let eventType = case (postType, metaEventType, messageType, dataObj) of
          (Just "message"    , _                , Just "private" , _      ) -> PrivateMessage
          (Just "message"    , _                , Just "group"   , _      ) -> GroupMessage
          (Nothing           , _                , Nothing        , Just _ ) -> Response
          (Just "meta_event" , Just "heartbeat" , Nothing        , Nothing) -> HeartBeat
          (_                 , Just "lifecycle" , _              , _      ) -> LifeCycle
          (Just "notice"     , _                , _              , _      ) -> NoticeEvent
          (Just "request"    , _                , _              , _      ) -> RequestEvent
          _                                                                 -> UnknownMessage
        timeUTC = posixSecondsToUTCTime . fromIntegral <$> timeUnixSec
    CQMessage <$> pure eventType
              <*> obj .:? "message_id"
              <*> obj .:? "group_id"
              <*> obj .:? "user_id"
              <*> obj .:? "sender"
              <*> pure message
              <*> obj .:? "time"
              <*> return timeUTC
              <*> obj .:? "self_id"
              <*> pure dataObj
              <*> obj .:? "echo"
              <*> pure Nothing
              <*> pure ( case message of
                Nothing -> Nothing
                Just _ -> MP.runParser MP.cqmsg $ fromMaybe "" message )
              <*> pure ( case (eventType, noticeType, noticeSubType) of
                (NoticeEvent , Just "group_upload"   , _                 ) -> Just NoticeGroupUpload
                (NoticeEvent , Just "group_admin"    , _                 ) -> Just NoticeGroupAdmin
                (NoticeEvent , Just "group_decrease" , Just "leave"      ) -> Just $ NoticeGroupDecrease GroupDecreaseLeave
                (NoticeEvent , Just "group_decrease" , Just "kick"       ) -> Just $ NoticeGroupDecrease GroupDecreaseKick
                (NoticeEvent , Just "group_decrease" , Just "kick_me"    ) -> Just $ NoticeGroupDecrease GroupDecreaseKickMe
                (NoticeEvent , Just "group_increase" , Just "approve"    ) -> Just $ NoticeGroupIncrease GroupIncreaseApprove
                (NoticeEvent , Just "group_increase" , Just "invite"     ) -> Just $ NoticeGroupIncrease GroupIncreaseInvite
                (NoticeEvent , Just "group_ban"      , _                 ) -> Just NoticeGroupBan
                (NoticeEvent , Just "group_recall"   , _                 ) -> Just NoticeGroupRecall
                (NoticeEvent , Just "group_add"      , _                 ) -> Just NoticeGroupAdd
                (NoticeEvent , Just "friend_add"     , _                 ) -> Just NoticeFriendAdd
                (NoticeEvent , Just "friend_recall"  , _                 ) -> Just NoticeFriendRecall
                (NoticeEvent , Just "notify"         , Just "poke"       ) -> Just $ NoticeNotify NoticeSubTypePoke
                (NoticeEvent , Just "notify"         , Just "lucky_king" ) -> Just $ NoticeNotify NoticeSubTypeLuckyKing
                (NoticeEvent , Just "notify"         , Just "honor"      ) -> Just $ NoticeNotify NoticeSubTypeHonor
                _ -> Nothing )
              <*> pure ( case (eventType, requestType, requestSubType) of
                (RequestEvent, Just "friend"       , _                 ) -> Just $ RequestFriend comment flag
                (RequestEvent, Just "group"        , Just "add"        ) -> Just $ RequestGroup RequestGroupAdd comment flag
                (RequestEvent, Just "group"        , Just "invite"     ) -> Just $ RequestGroup RequestGroupInvite comment flag
                _                                                        -> Nothing )


showCQ :: CQMessage -> String
showCQ cqmsg = concat [absId, messageType, " ",  chatId, senderId, " ", senderName, senderCard', ": ", MP.htmlDecodeFunction messageContent] --, mcqcodes]
  where messageType    = show $ eventType cqmsg
        absId          = maybe "" (\c -> "[" <> show c <> "] ") . absoluteId $ cqmsg
        chatId         = maybe "" show . groupId $ cqmsg
        senderId       = maybe "" (\c -> "(" ++ show c ++ ")") . userId $ cqmsg
        senderName     = maybe "" unpack $ senderNickname =<< sender cqmsg
        senderCard'    = surround $ maybe "" unpack $ senderCard =<< sender cqmsg
        messageContent = maybe "" unpack $ message cqmsg
        surround s     = if null s then s else "(" ++ s ++ ")"

type EssentialContent = (Text, ChatId, UserId, CQMessageId, Sender)
cqmsgToEssentialContent :: CQMessage -> Maybe EssentialContent
cqmsgToEssentialContent cqmsg =
  (,,,,) <$> (fmap onlyMessage . metaMessage $ cqmsg)
         <*> cqmsgToCid cqmsg
         <*> userId cqmsg
         <*> messageId cqmsg
         <*> sender cqmsg

cqmsgToCid :: CQMessage -> Maybe ChatId
cqmsgToCid cqmsg = case eventType cqmsg of
  GroupMessage -> GroupChat <$> groupId cqmsg
  PrivateMessage -> PrivateChat <$> userId cqmsg
  _ -> Nothing

makeLenses_ ''CQMessage
