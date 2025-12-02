-- | less important but huge amounts of data.
module Data.PersistModel.Data where

import Command.Hangman.Model
import Control.Applicative
import Cron.Parser
import Data.Additional.Saved
import Data.Coerce
import Data.Default
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Persist.Sql
import Database.Persist.TH
import External.ChatAPI
import GHC.Generics
import MeowBot.Data.CQHttp.CQCode
import MeowBot.CommandRule
import MeowBot.CronTab.CronMeowAction
import MeowBot.CostModel.Types
import MeowBot.Data
import MeowBot.Data.Book
import MeowBot.Data.IgnoreMatchType
import Utils.Persist

share [mkPersist sqlSettings, mkMigrate "migrateData"] [persistLowerCase|
ChatMessage
  botName        String        Maybe
  botId          BotId
  time           UTCTime
  eventType      (PersistUseInt64 CQEventType)
  messageId      CQMessageId     Maybe
  chatId         ChatId
  userId         UserId        Maybe
  absoluteId     AbsoluteMsgId
  pureContent    Text
  cqCodes        [CQCode]
  replyTo        CQMessageId     Maybe
  senderNickname Text          Maybe
  senderCard     Text          Maybe
  senderRole     (PersistUseInt64 Role)  Maybe
|]


cqMessageToChatMessage :: BotId -> BotName -> CQMessage -> Maybe ChatMessage
cqMessageToChatMessage botid botname cqm = do
  cid <- GroupChat <$> groupId cqm <|> PrivateChat <$> userId cqm
  aid <- absoluteId cqm
  utc <- posixSecondsToUTCTime . fromIntegral <$> time cqm <|> utcTime cqm
  return $ ChatMessage
    { chatMessageBotName        = coerce botname
    , chatMessageBotId          = botid
    , chatMessageTime           = utc
    , chatMessageEventType      = PersistUseInt64 $ eventType cqm
    , chatMessageMessageId      = messageId cqm
    , chatMessageChatId         = cid
    , chatMessageUserId         = userId cqm
    , chatMessageAbsoluteId     = aid
    , chatMessagePureContent    = fromMaybe "" $ onlyMessage <$> metaMessage cqm
    , chatMessageCqCodes        = cqcodes =<< maybe [] pure (metaMessage cqm)
    , chatMessageReplyTo        = replyTo =<< metaMessage cqm
    , chatMessageSenderNickname = senderNickname =<< sender cqm
    , chatMessageSenderCard     = senderCard =<< sender cqm
    , chatMessageSenderRole     = PersistUseInt64 <$> (senderRole =<< sender cqm)
    }

