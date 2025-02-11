{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
module MeowBot.MetaMessage where

import MeowBot.CQCode
import External.ChatAPI (ChatSetting(..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Additional
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (rights)

import External.ChatAPI (Message(..), ChatStatus(..))

data MetaMessage = MetaMessage
  { cqcodes :: [CQCode]
  , mixedMessage :: [Either CQCode Text]
  , replyTo :: Maybe Int
  , metaMessageItems :: [MetaMessageItem]
  , additionalData :: [AdditionalData]
  } deriving (Show, Eq, Generic, NFData)

onlyMessage :: MetaMessage -> Text
onlyMessage = T.concat . rights . mixedMessage

instance HasAdditionalData MetaMessage where
  getAdditionalData = additionalData
  modifyAdditionalData f meta = meta { additionalData = f $ additionalData meta }
  {-# INLINE getAdditionalData #-}
  {-# INLINE modifyAdditionalData #-}

data MetaMessageItem = MCQCode CQCode | MReplyTo MessageId | MChatSetting ChatSetting | MMessage Message | MChatStatus ChatStatus
  deriving (Show, Eq, Generic, NFData)

generateMetaMessage :: Text -> [AdditionalData] -> [MetaMessageItem] -> MetaMessage
generateMetaMessage str adt items = MetaMessage
  { cqcodes     = [cqcode | MCQCode cqcode <- items]
  , mixedMessage = [Left cqcode | MCQCode cqcode <- items] ++ [Right str]
  , replyTo     = listToMaybe [mid | MReplyTo mid <- items]
  , metaMessageItems = items
  , additionalData = adt
  }

type MessageId = Int
