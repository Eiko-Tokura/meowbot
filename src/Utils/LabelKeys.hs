{-# OPTIONS_GHC -Wno-orphans #-}
module Utils.LabelKeys where

import Data.PersistModel
import External.ChatAPI.Models
import MeowBot
import Module.Prometheus
import Utils.Persist ( keyToInt )
import qualified Data.Text as T

instance LabelKey "message_type" where
  type LabelValue "message_type" = CQEventType
  labelValueToText GroupMessage    = "group"
  labelValueToText PrivateMessage  = "private"
  labelValueToText Response        = "response"
  labelValueToText HeartBeat       = "heartbeat"
  labelValueToText LifeCycle       = "lifecycle"
  labelValueToText SelfMessage     = "self_message"
  labelValueToText UnknownMessage  = "unknown"
  labelValueToText RequestEvent    = "request"
  labelValueToText NoticeEvent     = "notice"

instance LabelKey "bot_id" where
  type LabelValue "bot_id" = BotId
  labelValueToText (BotId i) = toText i

instance LabelKey "sender_id" where
  type LabelValue "sender_id" = UserId
  labelValueToText (UserId i) = toText i

instance LabelKey "chat_id" where
  type LabelValue "chat_id" = ChatId
  labelValueToText (GroupChat i)   = toText i
  labelValueToText (PrivateChat i) = toText i

instance LabelKey "chat_model" where
  type LabelValue "chat_model" = ChatModel
  labelValueToText = toText

instance LabelKey "api_key" where
  type LabelValue "api_key" = Text
  labelValueToText x = let
    x1    = T.take    n x
    x2    = T.takeEnd n x
    n     = T.length x `div` 3
    stars = T.replicate (T.length x - T.length x1 - T.length x2) "*"
    in x1 <> stars <> x2

instance LabelKey "wallet_id" where
  type LabelValue "wallet_id" = WalletId
  labelValueToText = toText . keyToInt

instance LabelKey "periodic_cost_reason" where
  type LabelValue "periodic_cost_reason" = Text
  labelValueToText = id
