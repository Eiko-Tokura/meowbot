module MeowBot.Data.CQHttp.Query where

import MeowBot.Prelude
import Data.Aeson
import Data.Aeson.Types (Parser)
import MeowBot.Data.ChatId
import MeowBot.Data.CQHttp.Action
import Utils.Time

-------------------------------------------------------------------------------------------
-- Query API
data QueryType
  = QueryGetStatus
  | QueryGroupList
  | QueryGroupMemberInfo
  | QueryGroupInfo
  | QueryGetForwardMessage

data WithEcho a = WithEcho
  { maybeEcho :: Maybe Text
  , params    :: a
  } deriving (Show, Eq, Read, Generic)

newtype ForwardMessageId = ForwardMessageId { unForwardMessageId :: Int }
  deriving (Show, Eq, Read, Generic)
  deriving newtype (NFData)

data QueryAPI (q :: QueryType) where
  GetStatus    :: QueryAPI 'QueryGetStatus
  GetGroupList :: { queryGroupListNoCache :: Bool } -> QueryAPI 'QueryGroupList
  -- ^ It seems that different to CQHTTP, nap-cat must requre the no_cache field
  -- so we switch from Maybe Bool to Bool
  GetGroupMemberInfo ::
    { queryGroupMemberGroupId :: GroupId
    , queryGroupMemberUserId  :: UserId
    , queryGroupMemberNoCache :: Bool
    } -> QueryAPI 'QueryGroupMemberInfo
  GetGroupInfo ::
    { queryGroupInfoGroupId :: GroupId
    , queryGroupInfoNoCache :: Bool
    } -> QueryAPI 'QueryGroupMemberInfo
  GetForwardMessage :: ForwardMessageId -> QueryAPI 'QueryGetForwardMessage

data Sex = SexMale | SexFemale
  deriving (Show, Eq, Read, Generic, NFData)

data QueryAPIResponse (q :: QueryType) where
  GetStatusResponse ::
    { getStatusOnline :: Bool, getStatusGood :: Bool } -> QueryAPIResponse 'QueryGetStatus
  GetGroupListResponse ::
    { getGroupList :: [GroupBasicInfo] } -> QueryAPIResponse 'QueryGroupList
  GetGroupMemberInfoResponse ::
    { getGroupMemberInfoGroupId         :: GroupId
    , getGroupMemberInfoUserId          :: UserId
    , getGroupMemberInfoNickname        :: Maybe Text
    , getGroupMemberInfoCard            :: Maybe Text
    , getGroupMemberInfoSex             :: Maybe Sex
    , getGroupMemberInfoArea            :: Maybe Text
    , getGroupMemberInfoAge             :: Maybe Int
    , getGroupMemberInfoJoinTime        :: Maybe UTCTime
    , getGroupMemberInfoLastSentTime    :: Maybe UTCTime
    , getGroupMemberInfoLevel           :: Text
    , getGroupMemberInfoRole            :: Role
    , getGroupMemberInfoUnfriendly      :: Bool
    , getGroupMemberInfoTitle           :: Maybe Text
    , getGroupMemberInfoCardChangeable  :: Bool
    -- , shutUpTimestamp :: Maybe UTCTime -- ^ what? gocqhttp gives you Int64?
    } -> QueryAPIResponse 'QueryGroupMemberInfo
  GetGroupInfoResponse ::
    { getGroupInfoGroupId        :: GroupId
    , getGroupInfoGroupName      :: Maybe Text
    , getGroupInfoGroupMemo      :: Maybe Text
    , getGroupInfoCreateTime     :: Maybe UTCTime
    , getGroupInfoLevel          :: Maybe Int
    , getGroupInfoMemberCount    :: Maybe Int
    , getGroupInfoMaxMemberCount :: Maybe Int
    } -> QueryAPIResponse 'QueryGroupInfo

-- GetMessageResponse ::
--   { getMessageTime    :: UTCTime
--   , getMessageType    :: CQEventType
--   , getMessageId      :: MessageId
--   , getMessageSender  :: Sender
--   , getMessageMessage :: CQMessage
--   }
--   -> QueryAPIResponse 'QueryGetMessage
-- GetForwardMessageResponse :: [CQMessage] -> QueryAPIResponse 'QueryGetForwardMessage
-- instance ToJSON (QueryAPI q) where
--   toJSON GetStatus = object ["action" .= ("get_status" :: Text)]

deriving instance Show (QueryAPI q)
deriving instance Show (QueryAPIResponse q)

instance ToJSON (ActionForm (QueryAPI q)) where
  toJSON (ActionForm GetStatus mecho) = object
    [ "action" .= ("get_status" :: Text)
    , "echo"   .= mecho
    ]
  toJSON (ActionForm (GetGroupList nocache) mecho) = object
    [ "action" .= ("get_group_list" :: Text)
    , "params" .= object
        [ "no_cache" .= nocache
        ]
    , "echo"   .= mecho
    ]
  toJSON (ActionForm (GetGroupMemberInfo gid uid nocache) mecho) = object
    [ "action" .= ("get_group_member_info" :: Text)
    , "params" .= object
        [ "group_id" .= gid
        , "user_id"  .= uid
        , "no_cache" .= nocache
        ]
    , "echo"   .= mecho
    ]
  toJSON (ActionForm (GetGroupInfo gid nocache) mecho) = object
    [ "action" .= ("get_group_info" :: Text)
    , "params" .= object
        [ "group_id" .= gid
        , "no_cache" .= nocache
        ]
    , "echo"   .= mecho
    ]
  toJSON (ActionForm (GetForwardMessage (ForwardMessageId fmid)) mecho) = object
    [ "action" .= ("get_forward_msg" :: Text)
    , "params" .= object
        [ "message_id" .= fmid
        ]
    , "echo"   .= mecho
    ]

instance {-# OVERLAPPABLE #-} FromJSON resp => FromJSON (WithEcho resp) where
  parseJSON = withObject "WithEcho" $ \o -> do
    mecho <- o .:? "echo"
    dataObj <- o .: "data"
    resp <- parseJSON (Object dataObj)
    return $ WithEcho mecho resp

-- {"status":"ok","retcode":0,"data":{"online":true,"good":true,"stat":{"message_received":1081,"message_sent":88,"last_message_time":1742683958,"startup_time":1742667809}},"message":"","wording":"","echo":"echo-6049836937965548964"}
instance FromJSON (WithEcho (QueryAPIResponse 'QueryGetStatus)) where
  parseJSON = withObject "QueryAPIResponse" $ \o -> do
    dataObj <- o .: "data"
    online  <- dataObj .: "online"
    good    <- dataObj .: "good"
    mecho  <- o .:? "echo"
    return $ WithEcho mecho $ GetStatusResponse { getStatusOnline = online, getStatusGood = good }

instance FromJSON (WithEcho (QueryAPIResponse 'QueryGroupMemberInfo)) where
  parseJSON = withObject "QueryAPIResponse" $ \o -> do
    dataObj        <- o .: "data"
    gid            <- dataObj .: "group_id"
    uid            <- dataObj .: "user_id"
    nickname       <- dataObj .:? "nickname"
    card           <- dataObj .:? "card"
    sexText        <- dataObj .:? "sex" :: Parser (Maybe Text)
    area           <- dataObj .:? "area"
    age            <- dataObj .:? "age"
    jointime       <- dataObj .:? "join_time"
    lastsent       <- dataObj .:? "last_sent_time"
    level          <- dataObj .: "level"
    role           <- dataObj .: "role"
    unfriendly     <- dataObj .: "unfriendly"
    title          <- dataObj .:? "title"
    cardchangeable <- dataObj .: "card_changeable"
    -- shutUpTimestamp <- dataObj .:? "shut_up_timestamp"
    mecho  <- o .:? "echo"
    return $ WithEcho mecho $ GetGroupMemberInfoResponse
      { getGroupMemberInfoGroupId        = gid
      , getGroupMemberInfoUserId         = uid
      , getGroupMemberInfoNickname       = nickname
      , getGroupMemberInfoCard           = card
      , getGroupMemberInfoSex            = case sexText of
          Just "male"   -> Just SexMale
          Just "female" -> Just SexFemale
          Just "unknown"-> Nothing
          _             -> Nothing
      , getGroupMemberInfoArea           = area
      , getGroupMemberInfoAge            = age
      , getGroupMemberInfoJoinTime       = unixSecToUTCTime <$> jointime
      , getGroupMemberInfoLastSentTime   = unixSecToUTCTime <$> lastsent
      , getGroupMemberInfoLevel          = level
      , getGroupMemberInfoRole           = role
      , getGroupMemberInfoUnfriendly     = unfriendly
      , getGroupMemberInfoTitle          = title
      , getGroupMemberInfoCardChangeable = cardchangeable
      -- , shutUpTimestamp = posixSecondsToUTCTime . fromIntegral <$> shutUpTimestamp
      }

instance FromJSON (QueryAPIResponse 'QueryGroupInfo) where
  parseJSON = withObject "QueryAPIResponse" $ \o -> do
    dataObj        <- o .: "data"
    gid            <- dataObj .: "group_id"
    gname          <- dataObj .:? "group_name"
    gmemo          <- dataObj .:? "group_memo"
    createtime     <- dataObj .:? "create_time"
    level          <- dataObj .:? "level"
    membercount    <- dataObj .:? "member_count"
    maxmembercount <- dataObj .:? "max_member_count"
    return $ GetGroupInfoResponse
      { getGroupInfoGroupId        = gid
      , getGroupInfoGroupName      = gname
      , getGroupInfoGroupMemo      = gmemo
      , getGroupInfoCreateTime     = unixSecToUTCTime <$> createtime
      , getGroupInfoLevel          = level
      , getGroupInfoMemberCount    = membercount
      , getGroupInfoMaxMemberCount = maxmembercount
      }

data GroupBasicInfo = GroupBasicInfo
  { groupBasicInfoGroupId        :: GroupId
  , groupBasicInfoGroupName      :: Text
  , groupBasicInfoMemberCount    :: Int
  , groupBasicInfoMaxMemberCount :: Int
  } deriving (Show, Eq, Read, Generic, NFData)

instance FromJSON GroupBasicInfo where
  parseJSON = withObject "GroupBasicInfo" $ \o -> do
    gid            <- o .: "group_id"
    gname          <- o .: "group_name"
    membercount    <- o .: "member_count"
    maxmembercount <- o .: "max_member_count"
    return GroupBasicInfo
      { groupBasicInfoGroupId        = gid
      , groupBasicInfoGroupName      = gname
      , groupBasicInfoMemberCount    = membercount
      , groupBasicInfoMaxMemberCount = maxmembercount
      }

instance FromJSON (WithEcho (QueryAPIResponse 'QueryGroupList)) where
  parseJSON = withObject "QueryAPIResponse" $ \o -> do
    dataObj <- o .: "data"
    mecho  <- o .:? "echo"
    return $ WithEcho mecho $ GetGroupListResponse { getGroupList = dataObj }
