module MeowBot.MeowMsg.CQHttp where

import Data.Aeson (ToJSON, FromJSON)
import MeowBot.Prelude
import MeowBot.MeowMsg
import MeowBot.MetaMessage
import MeowBot.Data.CQHttp.CQCode
import MeowBot.Data.CQHttp.CQMessage

data CQHttp

newtype QQUserId = QQUserId { unQQUserId :: Int }
  deriving newtype (Eq, Ord, Show, Read, Num, ToJSON, FromJSON, NFData)

newtype QQGroupId = QQGroupId { unQQGroupId :: Int }
  deriving newtype (Eq, Ord, Show, Read, Num, ToJSON, FromJSON, NFData)

newtype CQFaceId = CQFaceId { unCQFaceId :: Int }
  deriving newtype (Eq, Ord, Show, Read, Num, ToJSON, FromJSON, NFData)

instance Platform CQHttp where
  type MUserId    CQHttp = QQUserId
  type MGroupId   CQHttp = QQGroupId
  type MUserInfo  CQHttp = CQSenderInfo
  type MMsgId     CQHttp = CQMessageId
  type MType      CQHttp = Either QQGroupId QQUserId -- ^ group message or private message
  type MMsgInline CQHttp = Either Text CQCode
  type MFace      CQHttp = CQFaceId
