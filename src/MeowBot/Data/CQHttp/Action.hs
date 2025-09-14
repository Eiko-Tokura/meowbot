module MeowBot.Data.CQHttp.Action where

import MeowBot.Prelude
import Data.Aeson
import MeowBot.Data.ChatId
import MeowBot.MetaMessage
import MeowBot.Data.CQHttp.Notice

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
  | SetEssenceMessage
    { setEssenceMessageMessageId :: MessageId
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
  | SetGroupWholeBan
    { setGroupWholeBanGroupId :: GroupId
    , setGroupWholeBanEnable  :: Bool
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
  -- | SendForwardMessage
  --   { sendForwardMessageChatId :: ChatId
  --   , sendForwardMessageMessages :: [CQMessage]
  --   }
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
  toJSON (SetEssenceMessage mid) = object
    [ "message_id" .= mid
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
  toJSON (SetGroupWholeBan gid enable) = object
    [ "group_id" .= gid
    , "enable" .= enable
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
actionString SetEssenceMessage{}    = "set_essence_msg"
actionString SetGroupKick{}         = "set_group_kick"
actionString SetGroupBan{}          = "set_group_ban"
actionString SetGroupWholeBan{}     = "set_group_whole_ban"
actionString SetGroupCard{}         = "set_group_card"
actionString SetGroupLeave{}        = "set_group_leave"
actionString SetGroupSpecialTitle{} = "set_group_special_title"
actionString SetFriendAddRequest{}  = "set_friend_add_request"
actionString SetGroupAddRequest{}   = "set_group_add_request"
actionString SendPoke{}             = "send_poke"

data ActionForm a = ActionForm
  { action :: a
  , echo   :: Maybe Text
  } deriving (Show, Eq, Read, Generic)

instance ToJSON (ActionForm ActionAPI) where
  toJSON (ActionForm act mecho) = object
    [ "action" .= actionString act
    , "params" .= act
    , "echo"   .= mecho
    ]

