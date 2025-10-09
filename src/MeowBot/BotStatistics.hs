{-# LANGUAGE RecordWildCards #-}
module MeowBot.BotStatistics
  ( logBotStatistics
  , StatType(..)
  , APIInfo(..)
  , TokenPrice(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Default
import Data.PersistModel
import Data.Time
import External.ChatAPI
import External.ChatAPI.Cost
import MeowBot
import MeowBot.CostModel
import Utils.RunDB

data StatType
  = StatTokens ChatStatus
  | StatRecv
  | StatSent

logBotStatistics :: ChatId -> StatType -> Meow ()
logBotStatistics chatId StatRecv = do
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  runMeowDB $ do
    let apiKey' = "STATISTICS"
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
  runMeowDB $ do
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
  runMeowDB $ do
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
  void . runMeowDB $ insertApiCostRecord utcTime botId chatId (model <$> apiInfo) (apiKey <$> apiInfo) consumption
