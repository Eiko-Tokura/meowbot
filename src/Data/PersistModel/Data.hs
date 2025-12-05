-- | less important but huge amounts of data.
module Data.PersistModel.Data where

import Control.Applicative
import Data.Coerce
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Persist.Sql
import Database.Persist.TH
import MeowBot.Data.CQHttp.CQCode
import MeowBot.Data
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
    , chatMessagePureContent    = maybe "" onlyMessage (metaMessage cqm)
    , chatMessageCqCodes        = cqcodes =<< maybe [] pure (metaMessage cqm)
    , chatMessageReplyTo        = replyTo =<< metaMessage cqm
    , chatMessageSenderNickname = senderNickname =<< sender cqm
    , chatMessageSenderCard     = senderCard =<< sender cqm
    , chatMessageSenderRole     = PersistUseInt64 <$> (senderRole =<< sender cqm)
    }

chatMessageToCQMessage :: ChatMessage -> CQMessage
chatMessageToCQMessage cm = CQMessage
  { eventType    = coerce $ chatMessageEventType cm
  , messageId    = chatMessageMessageId cm
  , groupId      = case chatMessageChatId cm of
                      GroupChat gid   -> Just gid
                      _               -> Nothing
  , userId       = case chatMessageChatId cm of
                      PrivateChat uid -> Just uid
                      _               -> Nothing
  , sender       = Just $ CQSenderInfo
                      { senderNickname = chatMessageSenderNickname cm
                      , senderCard     = chatMessageSenderCard cm
                      , senderRole     = coerce $ chatMessageSenderRole cm
                      }
  , message      = Just $ chatMessagePureContent cm
  , time         = Just . round . utcTimeToPOSIXSeconds $ chatMessageTime cm
  , utcTime      = Just $ chatMessageTime cm
  , self_id      = Nothing
  , target_id    = Nothing
  , responseData = Nothing
  , echoR        = Nothing
  , absoluteId   = Nothing
  , metaMessage  = Just $ generateMetaMessage (chatMessagePureContent cm) [] $
                      [ MCQCode cq | cq <- chatMessageCqCodes cm ] ++
                      maybe [] (\rid -> [MReplyTo rid]) (chatMessageReplyTo cm)
  , noticeType   = Nothing
  , requestType  = Nothing
  }
