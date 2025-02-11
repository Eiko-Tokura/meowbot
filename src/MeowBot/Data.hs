{-# LANGUAGE DerivingStrategies, DeriveAnyClass, OverloadedStrings, DerivingVia #-}
module MeowBot.Data
  ( module MeowBot.MetaMessage
  , UserId(..), GroupId(..), ChatId(..), BotId(..)
  , ChatRoom
  , WholeChat--, AllData(..), OtherData(..)
  --, SavedData(..)

  , CQMessage(..), CQEventType(..)
  , ReceCQMessage(..), SentCQMessage(..)
  , Sender(..), Role(..), roleToText
  , ResponseData(..)

  , Flag(..)
  , NoticeType(..), NoticeSubType(..), GroupDecreaseSubType(..), GroupIncreaseSubType(..)
  , RequestType(..), RequestGroupSubType(..)

  , ActionAPI(..), ActionForm(..)

  , BotName(..)
  , BotModules(..)
  , BotInstance(..)
  , RunningMode, DebugFlag(..), RunningFlag(..), IdentityFlag(..), ProxyFlag(..), LogFlag(..), CommandFlags(..)
  --, CommandValue
  , EssentialContent

  , module Utils.Text

  , showCQ, cqmsgToEssentialContent, emptyCQMessage, cqmsgToCid
  ) where

import MeowBot.CommandRule
import MeowBot.MetaMessage
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson -- (FromJSON(..), withObject, withText, ToJSON(..))
import Data.Aeson.Types (Parser)
import Data.Additional
import qualified MeowBot.Parser as MP
import MeowBot.Parser (cqmsg)
import Data.Maybe
import External.ProxyWS (ProxyData)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Utils.Text

import Database.Persist.Sqlite

data ChatId = GroupChat GroupId | PrivateChat UserId
  deriving (Show, Eq, Ord, Read, Generic, NFData)

newtype BotId = BotId { unBotId :: Int } deriving (Show, Eq, Ord, Read, Generic)
  deriving newtype (PersistField, PersistFieldSql, NFData, Num)

-- | Structured and Unstructured Chat
-- recent messages top, older messages bottom
type ChatRoom = (ChatId, ([MP.Tree CQMessage], [CQMessage]))

type WholeChat = [ChatRoom]  -- [(ChatId, [Tree CQMessage])]
newtype BotName = BotName { maybeBotName :: Maybe String } deriving (Eq, Show)

type RunningMode     = [DebugFlag]
data DebugFlag       = DebugJson | DebugCQMessage deriving (Eq, Show)
data RunningFlag     = RunClient String Int | RunServer String Int deriving (Eq, Show)
data IdentityFlag    = UseName String | UseId BotId | UseSysMsg String deriving (Eq, Show)
data ProxyFlag       = ProxyFlag String Int deriving (Eq, Show)
data LogFlag         = LogFlag FilePath deriving (Eq, Show)
newtype CommandFlags = CommandFlag CommandId deriving (Eq, Show)

data BotInstance = BotInstance
  { botRunFlag       :: RunningFlag
  , botIdentityFlags :: [IdentityFlag]
  , botCommandFlags  :: [CommandFlags]
  , botDebugFlags    :: [DebugFlag]
  , botProxyFlags    :: [ProxyFlag]
  , botLogFlags      :: [LogFlag]
  } deriving (Eq, Show)

data BotModules = BotModules
  { canUseGroupCommands   :: [CommandId]
  , canUsePrivateCommands :: [CommandId]
  , nameOfBot             :: BotName
  , botId                 :: BotId
  , globalSysMsg          :: Maybe Text
  , proxyTChans           :: [ProxyData]
  , logFile               :: [FilePath]
  , botInstance           :: BotInstance
  } deriving (Show)

data CQEventType = GroupMessage | PrivateMessage | Response | HeartBeat | LifeCycle | SelfMessage | UnknownMessage | RequestEvent | NoticeEvent
  deriving (Show, Eq, Read, Generic, NFData, Bounded, Enum)

instance PersistField ChatId where
  toPersistValue (GroupChat gid) = toPersistValue gid
  toPersistValue (PrivateChat uid) = toPersistValue (-uid)
  fromPersistValue v = do
    gid <- fromPersistValue v
    if gid > 0
      then return $ GroupChat (GroupId gid)
      else return $ PrivateChat (UserId (-gid))

instance PersistFieldSql ChatId where sqlType _ = SqlInt64

-------------------------------------------------------------------------------------------
-- Notifcation and Request
data NoticeType
  = NoticeGroupUpload
  | NoticeGroupAdmin
  | NoticeGroupDecrease GroupDecreaseSubType
  | NoticeGroupIncrease GroupIncreaseSubType
  | NoticeGroupBan
  | NoticeGroupAdd
  | NoticeFriendAdd
  | NoticeGroupRecall
  | NoticeFriendRecall
  | NoticeNotify NoticeSubType
  deriving (Show, Eq, Read)

data NoticeSubType = NoticeSubTypePoke | NoticeSubTypeLuckyKing | NoticeSubTypeHonor
  deriving (Show, Eq, Read)

data GroupDecreaseSubType = GroupDecreaseLeave | GroupDecreaseKick | GroupDecreaseKickMe
  deriving (Show, Eq, Read)

data GroupIncreaseSubType = GroupIncreaseApprove | GroupIncreaseInvite
  deriving (Show, Eq, Read)

newtype Flag = Flag { unFlag :: Text }
  deriving (Show, Eq, Read, FromJSON, ToJSON) via Text

data RequestType
  = RequestFriend
    { requestComment :: Maybe Text
    , requestFlag    :: Maybe Flag
    }
  | RequestGroup
    { requestGroupSubType :: RequestGroupSubType
    , requestComment :: Maybe Text
    , requestFlag    :: Maybe Flag
    }
  deriving (Show, Eq, Read)

data RequestGroupSubType = RequestGroupAdd | RequestGroupInvite
  deriving (Show, Eq, Read)

-------------------------------------------------------------------------------------------
-- Action API
data ActionAPI
  = SendPrivateMessage
    { sendPrivateUserId     :: UserId
    , sendPrivateMessage    :: Text
    , sendPrivateAutoEscape :: Maybe Bool
    }
  | SendGroupMessage
    { sendGroupGroupId      :: GroupId
    , sendGroupMessage      :: Text
    , sendGroupAutoEscape   :: Maybe Bool
    }
  | DeleteMessage
    { deleteMessageMessageId :: MessageId
    }
  | GetMessage
    { getMessageMessageId    :: MessageId
    }
  | SendLike
    { sendLikeUserId         :: UserId
    , sendLikeTimes          :: Int
    }
  | SetGroupKick
    { setGroupKickGroupId    :: GroupId
    , setGroupKickUserId     :: UserId
    , setGroupKickRejectAdd  :: Bool
    }
  | SetGroupBan
    { setGroupBanGroupId     :: GroupId
    , setGroupBanUserId      :: UserId
    , setGroupBanDuration    :: Int
    }
  | SetGroupCard
    { setGroupCardGroupId    :: GroupId
    , setGroupCardUserId     :: UserId
    , setGroupCardCard       :: Text
    }
  | SetGroupLeave
    { setGroupLeaveGroupId   :: GroupId
    , setGroupLeaveIsDismiss :: Bool
    }
  | SetGroupSpecialTitle
    { setGroupSpecialTitleGroupId :: GroupId
    , setGroupSpecialTitleUserId  :: UserId
    , setGroupSpecialTitleTitle   :: Text
    }
  | SetFriendAddRequest
    { setFriendAddRequestId     :: UserId
    , setFriendAddRequestFlag   :: Flag
    , setFriendAddRequestRemark :: Text
    }
  | SetGroupAddRequest
    { setGroupAddRequestFlag         :: Flag
    , setGroupAddRequestSubType      :: RequestGroupSubType
    , setGroupAddRequestApprove      :: Bool
    , setGroupAddRequestRefuseReason :: Maybe Text -- ^ only when you refuse
    }
  | SendPoke
    { sendPokeUserId :: UserId
    , sendPokeChatId :: ChatId
    }
  -- | GetFriendList
  deriving (Show, Eq, Read, Generic)

instance ToJSON ActionAPI where
  toJSON (SendPrivateMessage uid msg auto) = object
    [ "user_id" .= uid
    , "message" .= msg
    , "auto_escape" .= auto
    ]
  toJSON (SendGroupMessage gid msg auto) = object
    [ "group_id" .= gid
    , "message" .= msg
    , "auto_escape" .= auto
    ]
  toJSON (DeleteMessage mid) = object
    [ "message_id" .= mid
    ]
  toJSON (GetMessage mid) = object
    [ "message_id" .= mid
    ]
  toJSON (SendLike uid times) = object
    [ "user_id" .= uid
    , "times" .= times
    ]
  toJSON (SetGroupKick gid uid rej) = object
    [ "group_id" .= gid
    , "user_id" .= uid
    , "reject_add_request" .= rej
    ]
  toJSON (SetGroupBan gid uid dur) = object
    [ "group_id" .= gid
    , "user_id" .= uid
    , "duration" .= dur
    ]
  toJSON (SetGroupCard gid uid card) = object
    [ "group_id" .= gid
    , "user_id" .= uid
    , "card" .= card
    ]
  toJSON (SetGroupLeave gid dis) = object
    [ "group_id" .= gid
    , "is_dismiss" .= dis
    ]
  toJSON (SetGroupSpecialTitle gid uid title) = object
    [ "group_id" .= gid
    , "user_id" .= uid
    , "special_title" .= title
    ]
  toJSON (SetFriendAddRequest uid flag remark) = object
    [ "user_id" .= uid
    , "flag" .= flag
    , "remark" .= remark
    ]
  toJSON (SetGroupAddRequest flag subtype approve reason) = object
    [ "flag" .= flag
    , "approve" .= approve
    , "sub_type" .= case subtype of
        RequestGroupAdd    -> "add" :: Text
        RequestGroupInvite -> "invite"
    , "reason" .= reason
    ]
  toJSON (SendPoke uid (GroupChat gid)) = object
    [ "user_id" .= uid
    , "group_id" .= gid
    ]
  toJSON (SendPoke uid (PrivateChat _)) = object
    [ "user_id" .= uid
    ]

actionString :: ActionAPI -> Text
actionString SendPrivateMessage{}   = "send_private_msg"
actionString SendGroupMessage{}     = "send_group_msg"
actionString DeleteMessage{}        = "delete_msg"
actionString GetMessage{}           = "get_msg"
actionString SendLike{}             = "send_like"
actionString SetGroupKick{}         = "set_group_kick"
actionString SetGroupBan{}          = "set_group_ban"
actionString SetGroupCard{}         = "set_group_card"
actionString SetGroupLeave{}        = "set_group_leave"
actionString SetGroupSpecialTitle{} = "set_group_special_title"
actionString SetFriendAddRequest{}  = "set_friend_add_request"
actionString SetGroupAddRequest{}   = "set_group_add_request"
actionString SendPoke{}             = "send_poke"

data ActionForm = ActionForm
  { action :: ActionAPI
  , echo   :: Maybe Text
  } deriving (Show, Eq, Read, Generic)

instance ToJSON ActionForm where
  toJSON (ActionForm act mecho) = object
    [ "action" .= actionString act
    , "params" .= act
    , "echo"   .= mecho
    ]

-------------------------------------------------------------------------------------------
-- CQMessage

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
  } deriving (Show, Eq, Generic)

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

data Role = ROwner | RAdmin | RMember | RUnknown
  deriving (Show, Read, Eq, Generic, NFData, Bounded, Enum)

roleToText :: Role -> Maybe Text
roleToText ROwner   = Just "群主"
roleToText RAdmin   = Just "群管理"
roleToText RMember  = Nothing -- ^ not displaying if it's a member
roleToText RUnknown = Just "未知"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "owner" -> return ROwner
    "admin" -> return RAdmin
    "member" -> return RMember
    _ -> return RUnknown

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
                Just _ -> MP.runParser cqmsg $ fromMaybe "" message )
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
        senderCard'    = surround $ maybe "" (unpack) $ senderCard =<< sender cqmsg
        messageContent = maybe "" unpack $ message cqmsg
        surround s     = if null s then s else "(" ++ s ++ ")"
        -- mcqcodes       = maybe "" (("\n"++) . show) $ mNonEmpty . cqcodes =<< metaMessage cqmsg
        -- mNonEmpty []   = Nothing
        -- mNonEmpty l    = Just l

type EssentialContent = (Text, ChatId, UserId, MessageId, Sender)
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

