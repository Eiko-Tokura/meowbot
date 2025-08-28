{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}
module MeowBot.BotStatistics
  ( logBotStatistics
  , StatType(..)
  , APIInfo(..)
  , TokenPrice(..)
  ) where

import Control.Monad.IO.Class
import Data.Default
import Data.PersistModel
import Data.Time
import Data.Maybe
import External.ChatAPI
import External.ChatAPI.Cost
import MeowBot
import Utils.RunDB

data StatType
  = StatTokens ChatStatus
  | StatRecv
  | StatSent

logBotStatistics :: ChatId -> StatType -> Meow ()
logBotStatistics chatId StatRecv = do
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  runDB $ do
    let apiInfo = APIInfo "STATISTICS" Nothing
    upsertBy
      (UniqueBotStatisticsPerApiKeyPerChatPerDay apiInfo.apiKey botId chatId day)
      def
        { botStatisticsPerApiKeyPerChatPerDayApiKey           = apiInfo.apiKey
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
  let apiInfo = APIInfo "STATISTICS" Nothing
  runDB $ do
    upsertBy
      (UniqueBotStatisticsPerApiKeyPerChatPerDay apiInfo.apiKey botId chatId day)
      def
        { botStatisticsPerApiKeyPerChatPerDayApiKey           = apiInfo.apiKey
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
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  let info = fromMaybe (APIInfo "NO_API_KEY" Nothing) apiInfo
  runDB $ do
    upsertBy
      (UniqueBotStatisticsPerApiKeyPerChatPerDay info.apiKey botId chatId day)
      def
        { botStatisticsPerApiKeyPerChatPerDayApiKey                    = info.apiKey
        , botStatisticsPerApiKeyPerChatPerDayBotId                     = botId
        , botStatisticsPerApiKeyPerChatPerDayChatId                    = chatId
        , botStatisticsPerApiKeyPerChatPerDayDay                       = day
        , botStatisticsPerApiKeyPerChatPerDayTotalInputEstimateTokens  = inputTokens
        , botStatisticsPerApiKeyPerChatPerDayTotalOutputEstimateTokens = outputTokens
        , botStatisticsPerApiKeyPerChatPerDayTotalApiCalls             = apiCalls
        , botStatisticsPerApiKeyPerChatPerDayTotalApiCallErrors        = apiErrors
        , botStatisticsPerApiKeyPerChatPerDayTotalApiCallSkips         = apiSkips
        , botStatisticsPerApiKeyPerChatPerDayTotalCost = do
            price <- info.price
            pure $ tokenCost price (TokenConsumption inputTokens outputTokens $ Just meowBotCacheHitRate)
        }
      [ BotStatisticsPerApiKeyPerChatPerDayTotalInputEstimateTokens  +=. inputTokens
      , BotStatisticsPerApiKeyPerChatPerDayTotalOutputEstimateTokens +=. outputTokens
      , BotStatisticsPerApiKeyPerChatPerDayTotalApiCalls +=. apiCalls
      , BotStatisticsPerApiKeyPerChatPerDayTotalApiCallErrors +=. apiErrors
      , BotStatisticsPerApiKeyPerChatPerDayTotalApiCallSkips +=. apiSkips
      , BotStatisticsPerApiKeyPerChatPerDayTotalCost +=. do
          price <- info.price
          pure $ tokenCost price (TokenConsumption inputTokens outputTokens $ Just meowBotCacheHitRate) 
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

