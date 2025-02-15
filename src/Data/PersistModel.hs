{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies, DerivingStrategies, UndecidableInstances, OverloadedStrings, DataKinds, TemplateHaskell, QuasiQuotes #-}
module Data.PersistModel where

import Database.Persist.Sqlite
import Database.Persist.TH
import MeowBot.CommandRule
import MeowBot.CQCode
import MeowBot.Data
import MeowBot.Data.Book
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Utils.Persist
import Data.Maybe
import Data.Coerce
import Data.Default
import Control.Applicative
import Command.Hangman.Model
import External.ChatAPI
import Data.Additional.Saved

import qualified Data.Set as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Insert data types that are used in the project
-- For example:
-- User
--  name String
--  age Int
--  deriving Show

BotSetting -- Overlappable by BotSettingPerChat
  botName          String                     Maybe
  botId            BotId
  UniqueBotId      botId
  defaultModel     (PersistUseShow ChatModel) Maybe
  defaultModelS    (PersistUseShow ChatModel) Maybe
  displayThinking         Bool                 Maybe
  systemMessage           Text                 Maybe
  systemTemp              Double               Maybe
  systemMaxToolDepth      Int                  Maybe
  systemAPIKeyOpenAI      Text                 Maybe
  systemAPIKeyDeepSeek    Text                 Maybe
  systemAPIKeyOpenRouter  Text                 Maybe
  systemAPIKeySiliconFlow Text                Maybe
  activeChat              Bool                 Maybe
  atReply                 Bool                 Maybe
  activeProbability       Double               Maybe
  maxMessageInState       Int                  Maybe

BotSettingPerChat -- Overlapping BotSetting
  botName          String                     Maybe
  botId            BotId
  chatId           ChatId
  UniqueBotIdChatId botId chatId
  defaultModel     (PersistUseShow ChatModel) Maybe
  defaultModelS    (PersistUseShow ChatModel) Maybe
  displayThinking         Bool                 Maybe
  systemMessage           Text                 Maybe
  systemTemp              Double               Maybe
  systemMaxToolDepth      Int                  Maybe
  systemAPIKeyOpenAI      Text                 Maybe
  systemAPIKeyDeepSeek    Text                 Maybe
  systemAPIKeyOpenRouter  Text                 Maybe
  systemAPIKeySiliconFlow Text                 Maybe
  activeChat              Bool                 Maybe
  atReply                 Bool                 Maybe
  activeProbability       Double               Maybe
  maxMessageInState       Int                  Maybe

BotStatistics
  botId                       BotId
  UniqueBotStatisticsBotId    botId
  totalMessageRecv            Int
  totalMessageSent            Int
  totalApiCall                Int
  totalApiCallError           Int
  totalApiCallSkip            Int

BotStatisticsPerChat
  botId                       BotId
  chatId                      ChatId
  UniqueBotStatisticsPerChat  botId chatId
  totalMessageRecv            Int
  totalMessageSent            Int
  totalApiCall                Int
  totalApiCallError           Int
  totalApiCallSkip            Int

AssistantNote
  botName        String      Maybe
  chatId         ChatId
  noteId         Int
  title          Text
  content        Text
  time           UTCTime

CommandRuleDB
  botName        String      Maybe
  botId          BotId
  commandRule    CommandRule

InUserGroup
  botName        String      Maybe
  botId          BotId
  userId         UserId
  userGroup      UserGroup

InGroupGroup
  botName        String      Maybe
  botId          BotId
  groupId        GroupId
  groupGroup     GroupGroup

SavedAdditionalData
  botName        String      Maybe
  botId          BotId
  additionalData (PersistUseShow Saved_AdditionalData)

BookDB
  bookName    Text
  bookPdfPath FilePath       Maybe
  bookPages   [BookPage]
  bookInfo    BookInfo

ChatMessage
  botName        String        Maybe
  botId          BotId
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
  totalMiss      Int
  totalGuess     Int
  passCount      Int
  playcount      Int
  deriving Show
|]

instance Default BotSetting where
  def = BotSetting
    { botSettingBotName                 = Nothing
    , botSettingBotId                   = BotId 0
    , botSettingDefaultModel            = Nothing
    , botSettingDefaultModelS           = Nothing
    , botSettingDisplayThinking         = Nothing
    , botSettingSystemMessage           = Nothing
    , botSettingSystemTemp              = Nothing
    , botSettingSystemMaxToolDepth      = Nothing
    , botSettingSystemAPIKeyOpenAI      = Nothing
    , botSettingSystemAPIKeyDeepSeek    = Nothing
    , botSettingSystemAPIKeyOpenRouter  = Nothing
    , botSettingSystemAPIKeySiliconFlow = Nothing
    , botSettingActiveChat              = Nothing
    , botSettingAtReply                 = Nothing
    , botSettingActiveProbability       = Nothing
    , botSettingMaxMessageInState       = Nothing
    }

instance Default BotSettingPerChat where
  def = BotSettingPerChat
    { botSettingPerChatBotName                 = Nothing
    , botSettingPerChatBotId                   = BotId 0
    , botSettingPerChatChatId                  = PrivateChat 0
    , botSettingPerChatDefaultModel            = Nothing
    , botSettingPerChatDefaultModelS           = Nothing
    , botSettingPerChatDisplayThinking         = Nothing
    , botSettingPerChatSystemMessage           = Nothing
    , botSettingPerChatSystemTemp              = Nothing
    , botSettingPerChatSystemMaxToolDepth      = Nothing
    , botSettingPerChatSystemAPIKeyOpenAI      = Nothing
    , botSettingPerChatSystemAPIKeyDeepSeek    = Nothing
    , botSettingPerChatSystemAPIKeyOpenRouter  = Nothing
    , botSettingPerChatSystemAPIKeySiliconFlow = Nothing
    , botSettingPerChatActiveChat              = Nothing
    , botSettingPerChatAtReply                 = Nothing
    , botSettingPerChatActiveProbability       = Nothing
    , botSettingPerChatMaxMessageInState       = Nothing
    }

instance Default BotStatistics where
  def = BotStatistics
    { botStatisticsBotId             = BotId 0
    , botStatisticsTotalMessageRecv  = 0
    , botStatisticsTotalMessageSent  = 0
    , botStatisticsTotalApiCall      = 0
    , botStatisticsTotalApiCallError = 0
    , botStatisticsTotalApiCallSkip  = 0
    }

instance Default BotStatisticsPerChat where
  def = BotStatisticsPerChat
    { botStatisticsPerChatBotId             = BotId 0
    , botStatisticsPerChatChatId            = PrivateChat 0
    , botStatisticsPerChatTotalMessageRecv  = 0
    , botStatisticsPerChatTotalMessageSent  = 0
    , botStatisticsPerChatTotalApiCall      = 0
    , botStatisticsPerChatTotalApiCallError = 0
    , botStatisticsPerChatTotalApiCallSkip  = 0
    }

botSettingPerChatSystemAPIKey :: BotSettingPerChat -> Maybe APIKey
botSettingPerChatSystemAPIKey bspc = case
  ( botSettingPerChatSystemAPIKeyOpenAI      bspc
  , botSettingPerChatSystemAPIKeyDeepSeek    bspc
  , botSettingPerChatSystemAPIKeyOpenRouter  bspc
  , botSettingPerChatSystemAPIKeySiliconFlow bspc
  ) of
  (Nothing, Nothing, Nothing, Nothing) -> Nothing
  (a, b, c, d) -> Just $ APIKey a b c d

botSettingSystemAPIKey :: BotSetting -> Maybe APIKey
botSettingSystemAPIKey bs = case
  ( botSettingSystemAPIKeyOpenAI      bs
  , botSettingSystemAPIKeyDeepSeek    bs
  , botSettingSystemAPIKeyOpenRouter  bs
  , botSettingSystemAPIKeySiliconFlow bs
  ) of
  (Nothing, Nothing, Nothing, Nothing) -> Nothing
  (a, b, c, d) -> Just $ APIKey a b c d

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

hangmanStateToRecord :: UserId -> HangmanState -> HangmanRecord
hangmanStateToRecord uid hs = HangmanRecord
  { hangmanRecordUserId    = uid
  , hangmanRecordWord      = hangmanWord hs
  , hangmanRecordGuessed   = hangmanGuessed hs
  , hangmanRecordMods      = S.toList $ hangmanMods hs
  , hangmanRecordHp        = hangmanHP hs
  , hangmanRecordStartTime = hangmanStartTime hs
  , hangmanRecordEnded     = hangmanEnded hs
  , hangmanRecordScore     = hangmanScore hs
  }

hangmanRecordToState :: HangmanRecord -> HangmanState
hangmanRecordToState hr = HangmanState
  { hangmanWord      = hangmanRecordWord hr
  , hangmanGuessed   = hangmanRecordGuessed hr
  , hangmanMods      = S.fromList $ hangmanRecordMods hr
  , hangmanHP        = hangmanRecordHp hr
  , hangmanStartTime = hangmanRecordStartTime hr
  , hangmanEnded     = hangmanRecordEnded hr
  , hangmanScore     = hangmanRecordScore hr
  }
