{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
module MeowBot.MetaMessage where

import MeowBot.CQCode
import External.ChatAPI (ChatSetting(..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Additional
import Data.Text (Text)

data MetaMessage = MetaMessage
  { onlyMessage :: Text
  , cqcodes :: [CQCode]
  , replyTo :: Maybe Int
  , withChatSetting :: Maybe ChatSetting
  , additionalData :: [AdditionalData]
  } deriving (Show, Eq, Generic, NFData)

instance HasAdditionalData MetaMessage where
  getAdditionalData = additionalData
  modifyAdditionalData f meta = meta { additionalData = f $ additionalData meta }
  {-# INLINE getAdditionalData #-}
  {-# INLINE modifyAdditionalData #-}

