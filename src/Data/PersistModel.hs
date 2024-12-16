{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies, DerivingStrategies, UndecidableInstances, OverloadedStrings, DataKinds, TemplateHaskell, QuasiQuotes #-}
module Data.PersistModel where

import Database.Persist.Sqlite
import Database.Persist.TH
import MeowBot.Data
import MeowBot.CQCode
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Utils.Persist
import Data.Maybe
import Data.Coerce
import Control.Applicative
import Command.Hangman.Model

import qualified Data.Set as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Insert data types that are used in the project
-- For example:
-- User
--  name String
--  age Int
--  deriving Show
ChatMessage
  botName        String        Maybe
  time           UTCTime
  eventType      (PersistUseInt64 CQEventType)
  messageId      MessageId     Maybe
  chatId         ChatId
  userId         UserId        Maybe
  absoluteId     Int
  pureContent    Text
  cqCodes        [CQCode]
  replyTo        MessageId     Maybe
  senderNickname Text          Maybe
  senderCard     Text          Maybe
  senderRole     (PersistUseInt64 Role)  Maybe

HangmanRecord
  userId         UserId
  word           Text
  guessed        [Char]
  mods           [HangmanMod]
  hp             Int
  startTime      UTCTime
  ended          Bool
  score          Double        Maybe

HangmanRanking
  userId         UserId
  UniqueUserId   userId
  userNickName   Text
  totalPP        Double
  rank           Int
  deriving Show
|]

cqMessageToChatMessage :: BotName -> CQMessage -> Maybe ChatMessage
cqMessageToChatMessage botname cqm = do
  cid <- GroupChat <$> groupId cqm <|> PrivateChat <$> userId cqm
  aid <- absoluteId cqm
  utc <- posixSecondsToUTCTime . fromIntegral <$> time cqm <|> utcTime cqm
  return $ ChatMessage
    { chatMessageBotName        = coerce botname
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

hangmanStateToRecord :: UserId -> HangmanState -> HangmanRecord
hangmanStateToRecord uid hs = HangmanRecord
  { hangmanRecordUserId   = uid
  , hangmanRecordWord     = hangmanWord hs
  , hangmanRecordGuessed  = hangmanGuessed hs
  , hangmanRecordMods     = S.toList $ hangmanMods hs
  , hangmanRecordHp       = hangmanHP hs
  , hangmanRecordStartTime = hangmanStartTime hs
  , hangmanRecordEnded    = hangmanEnded hs
  , hangmanRecordScore    = hangmanScore hs
  }
