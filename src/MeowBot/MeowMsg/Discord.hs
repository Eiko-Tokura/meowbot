module MeowBot.MeowMsg.Discord where

import Data.Void (Void)
import Data.Aeson (ToJSON, FromJSON)
import MeowBot.Prelude
import MeowBot.MeowMsg
import qualified Discord.Types as DC

data Discord

newtype DCUserId = DCUserId { unDCUserId :: DC.UserId }
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON)

newtype DCChannelId = DCChannelId { unDCChannelId :: DC.ChannelId }
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON)

newtype DCMessageId = DCMessageId { unDCMessageId :: DC.MessageId }
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON)

instance Platform Discord where
  type MUserId    Discord = DCUserId
  type MGroupId   Discord = DCChannelId
  type MUserInfo  Discord = DC.User
  type MMsgId     Discord = DCMessageId
  type MType      Discord = DCChannelId
  type MAttach    Discord = DC.Attachment

  type MMsgInline Discord = Text
  type MFace      Discord = Void
