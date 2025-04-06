{-# LANGUAGE FlexibleContexts, DeriveAnyClass, GADTs, TypeFamilies, DerivingStrategies, UndecidableInstances, OverloadedStrings, DataKinds, TemplateHaskell, QuasiQuotes #-}
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
import GHC.Generics

import qualified Data.Set as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

BotSetting -- Overlappable by BotSettingPerChat
  botName          String                      Maybe
  botId            BotId
  UniqueBotId      botId
  defaultModel     (PersistUseShow ChatModel)  Maybe
  defaultModelS    (PersistUseShow ChatModel)  Maybe
  displayThinking         Bool                 Maybe
  displayToolMessage      Bool                 Maybe
  systemMessage           Text                 Maybe
  systemTemp              Double               Maybe
  systemMaxToolDepth      Int                  Maybe
  systemAPIKeyOpenAI      Text                 Maybe
  systemAPIKeyDeepSeek    Text                 Maybe
  systemAPIKeyOpenRouter  Text                 Maybe
  systemAPIKeySiliconFlow Text                 Maybe
  systemAPIKeyAnthropic   Text                 Maybe
  systemAPIKeyXcApi       Text                 Maybe
  activeChat              Bool                 Maybe
  atReply                 Bool                 Maybe
  activeProbability       Double               Maybe
  maxMessageInState       Int                  Maybe
  deriving Generic
  deriving Default

BotSettingPerChat -- Overlapping BotSetting
  botName          String                      Maybe
  botId            BotId
  chatId           ChatId
  UniqueBotIdChatId botId chatId
  defaultModel     (PersistUseShow ChatModel)  Maybe
  defaultModelS    (PersistUseShow ChatModel)  Maybe
  displayThinking         Bool                 Maybe
  displayToolMessage      Bool                 Maybe
  systemMessage           Text                 Maybe
  systemTemp              Double               Maybe
  systemMaxToolDepth      Int                  Maybe
  systemAPIKeyOpenAI      Text                 Maybe
  systemAPIKeyDeepSeek    Text                 Maybe
  systemAPIKeyOpenRouter  Text                 Maybe
  systemAPIKeySiliconFlow Text                 Maybe
  systemAPIKeyAnthropic   Text                 Maybe
  systemAPIKeyXcApi       Text                 Maybe
  activeChat              Bool                 Maybe
  atReply                 Bool                 Maybe
  activeProbability       Double               Maybe
  maxMessageInState       Int                  Maybe
  deriving Generic
  deriving Default

BotRequestSetting -- how to handle requests received by bot
  botName          String                    Maybe
  botId            BotId
  UniqueBotRequestSettingBotId botId
  approveFriendRequest  Bool                 Maybe
  approveGroupRequest   Bool                 Maybe
  deriving Generic
  deriving Default

BotStatistics
  botId                       BotId
  UniqueBotStatisticsBotId    botId
  totalMessageRecv            Int
  totalMessageSent            Int
  totalApiCall                Int
  totalApiCallError           Int
  totalApiCallSkip            Int
  deriving Generic
  deriving Default

BotStatisticsPerChat
  botId                       BotId
  chatId                      ChatId
  UniqueBotStatisticsPerChat  botId chatId
  totalMessageRecv            Int
  totalMessageSent            Int
  totalApiCall                Int
  totalApiCallError           Int
  totalApiCallSkip            Int
  deriving Generic
  deriving Default

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

botSettingPerChatSystemAPIKey :: BotSettingPerChat -> Maybe APIKey
botSettingPerChatSystemAPIKey bspc = case
  ( botSettingPerChatSystemAPIKeyOpenAI      bspc
  , botSettingPerChatSystemAPIKeyDeepSeek    bspc
  , botSettingPerChatSystemAPIKeyOpenRouter  bspc
  , botSettingPerChatSystemAPIKeySiliconFlow bspc
  , botSettingPerChatSystemAPIKeyAnthropic   bspc
  , botSettingPerChatSystemAPIKeyXcApi       bspc
  ) of
  (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -> Nothing
  (a, b, c, d, e, f) -> Just $ APIKey a b c d e f

botSettingSystemAPIKey :: BotSetting -> Maybe APIKey
botSettingSystemAPIKey bs = case
  ( botSettingSystemAPIKeyOpenAI      bs
  , botSettingSystemAPIKeyDeepSeek    bs
  , botSettingSystemAPIKeyOpenRouter  bs
  , botSettingSystemAPIKeySiliconFlow bs
  , botSettingSystemAPIKeyAnthropic   bs
  , botSettingSystemAPIKeyXcApi       bs
  ) of
  (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -> Nothing
  (a, b, c, d, e, f) -> Just $ APIKey a b c d e f

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
