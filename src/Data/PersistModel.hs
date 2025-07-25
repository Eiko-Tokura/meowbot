{-# LANGUAGE FlexibleContexts, DeriveAnyClass, GADTs, TypeFamilies, DerivingStrategies, UndecidableInstances, OverloadedStrings, DataKinds, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.PersistModel where

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
import Database.Persist.Sqlite
import Database.Persist.TH
import External.ChatAPI
import GHC.Generics
import MeowBot.CQCode
import MeowBot.CommandRule
import MeowBot.CronTab.CronMeowAction
import MeowBot.Data
import MeowBot.Data.Book
import MeowBot.Data.IgnoreMatchType
import Utils.Persist

import qualified Data.Set as S

instance Default Day where
  def = ModifiedJulianDay 0

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

BotIgnore
  botId     BotId
  botName   String          Maybe
  string    Text            Maybe
  matchType IgnoreMatchType
  atPattern Bool            Maybe  -- nothing for no requirement, Just bool for require at / non-at
  private   Bool            Maybe  -- nothing for no requirement
  group     Bool            Maybe

BotCronJob
  botName          String         Maybe
  botId            BotId
  chatId           ChatId         Maybe
  -- ^ nothing means global cron job
  -- this field is used for easy searching and lookup i.e. management
  cronSchedule     CronText
  cronRepeatFinite Int            Maybe  -- nothing means always repeat
  cronMeowAction   CronMeowAction
  cronDetail       Text           Maybe  -- display / override the MeowActionMessage
  deriving Show

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
  mentionReply            Bool                 Maybe
  activeProbability       Double               Maybe
  maxMessageInState       Int                  Maybe
  enableNotes             Bool                 Maybe
  enableCronTab           Bool                 Maybe
  enableSetEssence        Bool                 Maybe
  enableSetGroupBan       Bool                 Maybe
  enableLeaveGroup        Bool                 Maybe
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
  mentionReply            Bool                 Maybe
  activeProbability       Double               Maybe
  maxMessageInState       Int                  Maybe
  enableNotes             Bool                 Maybe
  enableCronTab           Bool                 Maybe
  enableSetEssence        Bool                 Maybe
  enableSetGroupBan       Bool                 Maybe
  enableLeaveGroup        Bool                 Maybe
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
  totalInputEstimateTokens    Int
  totalOutputEstimateTokens   Int
  totalApiCalls               Int
  totalApiCallErrors          Int
  totalApiCallSkips           Int
  deriving Generic
  deriving Default

BotStatisticsPerChat
  botId                       BotId
  chatId                      ChatId
  UniqueBotStatisticsPerChat  botId chatId
  totalMessageRecv            Int
  totalMessageSent            Int
  totalInputEstimateTokens    Int
  totalOutputEstimateTokens   Int
  totalApiCalls               Int
  totalApiCallErrors          Int
  totalApiCallSkips           Int
  deriving Generic
  deriving Default

BotStatisticsPerChatPerDay
  botId                       BotId
  chatId                      ChatId
  day                         Day
  UniqueBotStatisticsPerChatPerDay botId chatId day
  totalMessageRecv            Int
  totalMessageSent            Int
  totalInputEstimateTokens    Int
  totalOutputEstimateTokens   Int
  totalApiCalls               Int
  totalApiCallErrors          Int
  totalApiCallSkips           Int
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
  additionalData (PersistUseShow SavedSPACEAdditionalData)

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

noteDisplayText :: AssistantNote -> Text
noteDisplayText note = let
  noteId = assistantNoteNoteId note
  titleText   = "id=" <> toText noteId <> ": " <> assistantNoteTitle note <> " - "
  contentText = assistantNoteContent note
  in titleText <> contentText

cronTabDisplayText :: Entity BotCronJob -> Text
cronTabDisplayText enCron = let
  cronId = keyToInt $ entityKey enCron
  cronJob = entityVal enCron
  cronText = botCronJobCronSchedule cronJob
  repeatText = case botCronJobCronRepeatFinite cronJob of
    Nothing -> "repeat forever"
    Just n  -> "repeat " <> toText n <> " times"
  action = botCronJobCronMeowAction cronJob
  in "id=" <> toText cronId <> ". " <> toText cronText <> " - " <> repeatText <> " - " <> toText action

cronTabDisplayTextWithCid :: ChatId -> Entity BotCronJob -> Maybe Text
cronTabDisplayTextWithCid cid enCron = let
  cronId = keyToInt $ entityKey enCron
  cronJob = entityVal enCron
  cronText = botCronJobCronSchedule cronJob
  repeatText = case botCronJobCronRepeatFinite cronJob of
    Nothing -> "repeat forever"
    Just n  -> "repeat " <> toText n <> " times"
  action = botCronJobCronMeowAction cronJob
  in case cronMeowActionChatId action == cid of
      True -> Just $ toText cronId <> ". " <> toText cronText <> " - " <> repeatText <> " - " <> toText action
      False -> Nothing
