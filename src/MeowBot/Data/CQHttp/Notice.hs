module MeowBot.Data.CQHttp.Notice where

import MeowBot.Prelude
import Data.Aeson

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
  deriving (Show, Eq, Read, Generic, NFData)

data NoticeSubType = NoticeSubTypePoke | NoticeSubTypeLuckyKing | NoticeSubTypeHonor
  deriving (Show, Eq, Read, Generic, NFData)

data GroupDecreaseSubType = GroupDecreaseLeave | GroupDecreaseKick | GroupDecreaseKickMe
  deriving (Show, Eq, Read, Generic, NFData)

data GroupIncreaseSubType = GroupIncreaseApprove | GroupIncreaseInvite
  deriving (Show, Eq, Read, Generic, NFData)

newtype Flag = Flag { unFlag :: Text }
  deriving (Show, Eq, Read, FromJSON, ToJSON) via Text
  deriving Generic
  deriving newtype NFData

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
  deriving (Show, Eq, Read, Generic, NFData)

data RequestGroupSubType = RequestGroupAdd | RequestGroupInvite
  deriving (Show, Eq, Read, Generic, NFData)

