{-# LANGUAGE RecordWildCards #-}
module MeowBot.BotStatistics
  ( logBotStatistics
  , StatType(..)
  , APIInfo(..)
  , TokenPrice(..)

  -- * Chat statistics
  , ChatStatistics(..)
  , chatStatistics
  ) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Default
import Data.PersistModel
import Data.Time
import Data.Maybe (fromMaybe)
import External.ChatAPI
import External.ChatAPI.Cost
import MeowBot
import MeowBot.CostModel
import Utils.RunDB
import Utils.LabelKeys ()
import Module.Prometheus.Manager
import qualified Database.Esqueleto.Experimental as E
import qualified Data.Vector.Unboxed as V

data StatType
  = StatTokens ChatStatus
  | StatRecv
  | StatSent

logBotStatistics :: ChatId -> StatType -> Meow ()
logBotStatistics chatId StatRecv = do
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  let apiKey' = "STATISTICS"
  runMeowCoreDB $ do
    upsertBy
      (UniqueBotStatisticsPerApiKeyPerChatPerDay apiKey' botId chatId day)
      def
        { botStatisticsPerApiKeyPerChatPerDayApiKey           = apiKey'
        , botStatisticsPerApiKeyPerChatPerDayBotId            = botId
        , botStatisticsPerApiKeyPerChatPerDayChatId           = chatId
        , botStatisticsPerApiKeyPerChatPerDayDay              = day
        , botStatisticsPerApiKeyPerChatPerDayTotalMessageRecv = 1
        }
      [ BotStatisticsPerApiKeyPerChatPerDayTotalMessageRecv +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsPerChat botId chatId)
      def
        { botStatisticsPerChatBotId            = botId
        , botStatisticsPerChatChatId           = chatId
        , botStatisticsPerChatTotalMessageRecv = 1
        }
      [ BotStatisticsPerChatTotalMessageRecv +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsBotId botId)
      def
        { botStatisticsBotId            = botId
        , botStatisticsTotalMessageRecv = 1
        }
      [ BotStatisticsTotalMessageRecv +=. 1
      ]
    pure ()
logBotStatistics chatId StatSent = do
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  let apiKey' = "STATISTICS"
  runMeowCoreDB $ do
    upsertBy
      (UniqueBotStatisticsPerApiKeyPerChatPerDay apiKey' botId chatId day)
      def
        { botStatisticsPerApiKeyPerChatPerDayApiKey           = apiKey'
        , botStatisticsPerApiKeyPerChatPerDayBotId            = botId
        , botStatisticsPerApiKeyPerChatPerDayChatId           = chatId
        , botStatisticsPerApiKeyPerChatPerDayDay              = day
        , botStatisticsPerApiKeyPerChatPerDayTotalMessageSent = 1
        }
      [ BotStatisticsPerApiKeyPerChatPerDayTotalMessageSent +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsPerChat botId chatId)
      def
        { botStatisticsPerChatBotId            = botId
        , botStatisticsPerChatChatId           = chatId
        , botStatisticsPerChatTotalMessageSent = 1
        }
      [ BotStatisticsPerChatTotalMessageSent +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsBotId botId)
      def
        { botStatisticsBotId            = botId
        , botStatisticsTotalMessageSent = 1
        }
      [ BotStatisticsTotalMessageSent +=. 1
      ]
    pure ()
logBotStatistics chatId (StatTokens ChatStatus { chatEstimateTokens = EstimateTokens {..} }) = do
  utcTime <- liftIO getCurrentTime
  let day = utctDay utcTime
  botId <- query
  let apiKey' = maybe "NO_API_KEY" apiKey apiInfo
      consumption = TokenConsumption inputTokens outputTokens $ Just meowBotCacheHitRate
      labels = [ Label      @"bot_id"     botId
               , Label      @"chat_id"    chatId
               , LabelMaybe @"api_key"    (apiKey <$> apiInfo)
               , LabelMaybe @"chat_model" (model  <$> apiInfo)
               ]
  managedCounter "meowbot_total_api_calls"       labels (AddCounter apiCalls)
  managedCounter "meowbot_total_api_call_errors" labels (AddCounter apiErrors)
  managedCounter "meowbot_total_api_call_skips"  labels (AddCounter apiSkips)
  managedCounter "meowbot_total_input_estimate_tokens"  labels (AddCounter inputTokens)
  managedCounter "meowbot_total_output_estimate_tokens" labels (AddCounter outputTokens)
  costRecord <- runMeowCoreDB $ insertApiCostRecord utcTime botId chatId (model <$> apiInfo) (apiKey <$> apiInfo) consumption
  let labelsWid = labels <> [ LabelMaybe @"wallet_id" (apiCostRecordWalletId =<< costRecord) ]
  managedGauge   "meowbot_total_actual_cost"  labelsWid (GaugeAdd (maybe 0 coerce $ apiCostRecordActualCost =<< costRecord))
  managedGauge   "meowbot_total_nominal_cost" labelsWid (GaugeAdd (maybe 0 coerce $ apiCostRecordNominalCost =<< costRecord))
  runMeowCoreDB $ do
    upsertBy
      (UniqueBotStatisticsPerApiKeyPerChatPerDay apiKey' botId chatId day)
      def
        { botStatisticsPerApiKeyPerChatPerDayApiKey                    = apiKey'
        , botStatisticsPerApiKeyPerChatPerDayBotId                     = botId
        , botStatisticsPerApiKeyPerChatPerDayChatId                    = chatId
        , botStatisticsPerApiKeyPerChatPerDayDay                       = day
        , botStatisticsPerApiKeyPerChatPerDayTotalInputEstimateTokens  = inputTokens
        , botStatisticsPerApiKeyPerChatPerDayTotalOutputEstimateTokens = outputTokens
        , botStatisticsPerApiKeyPerChatPerDayTotalApiCalls             = apiCalls
        , botStatisticsPerApiKeyPerChatPerDayTotalApiCallErrors        = apiErrors
        , botStatisticsPerApiKeyPerChatPerDayTotalApiCallSkips         = apiSkips
        , botStatisticsPerApiKeyPerChatPerDayTotalCost = do
            price <- price =<< apiInfo
            pure $ tokenCost price consumption
        }
      [ BotStatisticsPerApiKeyPerChatPerDayTotalInputEstimateTokens  +=. inputTokens
      , BotStatisticsPerApiKeyPerChatPerDayTotalOutputEstimateTokens +=. outputTokens
      , BotStatisticsPerApiKeyPerChatPerDayTotalApiCalls +=. apiCalls
      , BotStatisticsPerApiKeyPerChatPerDayTotalApiCallErrors +=. apiErrors
      , BotStatisticsPerApiKeyPerChatPerDayTotalApiCallSkips +=. apiSkips
      , BotStatisticsPerApiKeyPerChatPerDayTotalCost +=. do
          price <- price =<< apiInfo
          pure $ tokenCost price consumption
      ]
    upsertBy
      (UniqueBotStatisticsPerChat botId chatId)
      def
        { botStatisticsPerChatBotId                     = botId
        , botStatisticsPerChatChatId                    = chatId
        , botStatisticsPerChatTotalInputEstimateTokens  = inputTokens
        , botStatisticsPerChatTotalOutputEstimateTokens = outputTokens
        , botStatisticsPerChatTotalApiCalls             = apiCalls
        , botStatisticsPerChatTotalApiCallErrors        = apiErrors
        , botStatisticsPerChatTotalApiCallSkips         = apiSkips
        }
      [ BotStatisticsPerChatTotalInputEstimateTokens  +=. inputTokens
      , BotStatisticsPerChatTotalOutputEstimateTokens +=. outputTokens
      , BotStatisticsPerChatTotalApiCalls +=. apiCalls
      , BotStatisticsPerChatTotalApiCallErrors +=. apiErrors
      , BotStatisticsPerChatTotalApiCallSkips +=. apiSkips
      ]
    upsertBy
      (UniqueBotStatisticsBotId botId)
      def
        { botStatisticsBotId                     = botId
        , botStatisticsTotalInputEstimateTokens  = inputTokens
        , botStatisticsTotalOutputEstimateTokens = outputTokens
        , botStatisticsTotalApiCalls             = apiCalls
        , botStatisticsTotalApiCallErrors        = apiErrors
        , botStatisticsTotalApiCallSkips         = apiSkips
        }
      [ BotStatisticsTotalInputEstimateTokens  +=. inputTokens
      , BotStatisticsTotalOutputEstimateTokens +=. outputTokens
      , BotStatisticsTotalApiCalls +=. apiCalls
      , BotStatisticsTotalApiCallErrors +=. apiErrors
      , BotStatisticsTotalApiCallSkips +=. apiSkips
      ]
    pure ()

data ChatStatistics = ChatStatistics
  { recvMessagesPerDay      :: V.Vector Int -- ^ Indexed by day offset from today
  , sentMessagesPerDay      :: V.Vector Int -- ^ Indexed by day offset from today
  , totalInputTokensPerDay  :: V.Vector Int -- ^ Indexed by day offset from today
  , totalOutputTokensPerDay :: V.Vector Int -- ^ Indexed by day offset from today
  }
chatStatistics :: Int -> BotId -> ChatId -> DB ChatStatistics
chatStatistics nDays botId chatId = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let dayOffsets = [0 .. nDays - 1]
      days = map (addDays . negate . fromIntegral) dayOffsets <*> pure today
  recvMessagesPerDay <- V.fromList <$> mapM (getRecvMessages botId chatId) days
  sentMessagesPerDay <- V.fromList <$> mapM (getSentMessages botId chatId) days
  totalInputTokensPerDay <- V.fromList <$> mapM (getInputTokens botId chatId) days
  totalOutputTokensPerDay <- V.fromList <$> mapM (getOutputTokens botId chatId) days
  pure ChatStatistics {..}
  where
    getRecvMessages bId cId day = do
      mStat <- getBy $ UniqueBotStatisticsPerApiKeyPerChatPerDay "STATISTICS" bId cId day
      pure $ maybe 0 (botStatisticsPerApiKeyPerChatPerDayTotalMessageRecv . entityVal) mStat
    getSentMessages bId cId day = do
      mStat <- getBy $ UniqueBotStatisticsPerApiKeyPerChatPerDay "STATISTICS" bId cId day
      pure $ maybe 0 (botStatisticsPerApiKeyPerChatPerDayTotalMessageSent . entityVal) mStat
    getSumApiStats item bId cId day = do
      mStat <- E.selectOne $ do
        stat <- E.from $ E.table @BotStatisticsPerApiKeyPerChatPerDay
        E.where_ (     stat E.^. BotStatisticsPerApiKeyPerChatPerDayBotId  E.==. E.val bId
                 E.&&. stat E.^. BotStatisticsPerApiKeyPerChatPerDayChatId E.==. E.val cId
                 E.&&. stat E.^. BotStatisticsPerApiKeyPerChatPerDayDay    E.==. E.val day
                 )
        pure $ E.sum_ $ stat E.^. item
      case mStat of
        Nothing -> pure 0
        Just (E.Value mbSum) -> pure $ fromMaybe 0 mbSum
    getInputTokens = getSumApiStats BotStatisticsPerApiKeyPerChatPerDayTotalInputEstimateTokens
    getOutputTokens = getSumApiStats BotStatisticsPerApiKeyPerChatPerDayTotalOutputEstimateTokens
