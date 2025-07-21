{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module MeowBot.BotStatistics
  ( logBotStatistics
  , StatType(..)
  ) where

import Control.Monad.IO.Class
import Data.Default
import Data.PersistModel
import Data.Time
import External.ChatAPI
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
    upsertBy
      (UniqueBotStatisticsPerChatPerDay botId chatId day)
      def
        { botStatisticsPerChatPerDayBotId  = botId
        , botStatisticsPerChatPerDayChatId = chatId
        , botStatisticsPerChatPerDayDay    = day
        , botStatisticsPerChatPerDayTotalMessageRecv = 1
        }
      [ BotStatisticsPerChatPerDayTotalMessageRecv +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsPerChat botId chatId)
      def
        { botStatisticsPerChatBotId  = botId
        , botStatisticsPerChatChatId = chatId
        , botStatisticsPerChatTotalMessageRecv = 1
        }
      [ BotStatisticsPerChatTotalMessageRecv +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsBotId botId)
      def
        { botStatisticsBotId  = botId
        , botStatisticsTotalMessageRecv = 1
        }
      [ BotStatisticsTotalMessageRecv +=. 1
      ]
    pure ()
logBotStatistics chatId StatSent = do
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  runDB $ do
    upsertBy
      (UniqueBotStatisticsPerChatPerDay botId chatId day)
      def
        { botStatisticsPerChatPerDayBotId  = botId
        , botStatisticsPerChatPerDayChatId = chatId
        , botStatisticsPerChatPerDayDay    = day
        , botStatisticsPerChatPerDayTotalMessageSent = 1
        }
      [ BotStatisticsPerChatPerDayTotalMessageSent +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsPerChat botId chatId)
      def
        { botStatisticsPerChatBotId  = botId
        , botStatisticsPerChatChatId = chatId
        , botStatisticsPerChatTotalMessageSent = 1
        }
      [ BotStatisticsPerChatTotalMessageSent +=. 1
      ]
    upsertBy
      (UniqueBotStatisticsBotId botId)
      def
        { botStatisticsBotId  = botId
        , botStatisticsTotalMessageSent = 1
        }
      [ BotStatisticsTotalMessageSent +=. 1
      ]
    pure ()
logBotStatistics chatId (StatTokens ChatStatus { chatEstimateTokens = EstimateTokens {..} }) = do
  day <- liftIO $ utctDay <$> getCurrentTime
  botId <- query
  runDB $ do
    upsertBy
      (UniqueBotStatisticsPerChatPerDay botId chatId day)
      def
        { botStatisticsPerChatPerDayBotId  = botId
        , botStatisticsPerChatPerDayChatId = chatId
        , botStatisticsPerChatPerDayDay    = day
        , botStatisticsPerChatPerDayTotalInputEstimateTokens = inputTokens 
        , botStatisticsPerChatPerDayTotalOutputEstimateTokens = outputTokens
        , botStatisticsPerChatPerDayTotalApiCalls = apiCalls
        , botStatisticsPerChatPerDayTotalApiCallErrors = apiErrors
        , botStatisticsPerChatPerDayTotalApiCallSkips = apiSkips
        }
      [ BotStatisticsPerChatPerDayTotalInputEstimateTokens  +=. inputTokens
      , BotStatisticsPerChatPerDayTotalOutputEstimateTokens +=. outputTokens
      , BotStatisticsPerChatPerDayTotalApiCalls +=. apiCalls
      , BotStatisticsPerChatPerDayTotalApiCallErrors +=. apiErrors
      , BotStatisticsPerChatPerDayTotalApiCallSkips +=. apiSkips
      ]
    upsertBy
      (UniqueBotStatisticsPerChat botId chatId)
      def
        { botStatisticsPerChatBotId  = botId
        , botStatisticsPerChatChatId = chatId
        , botStatisticsPerChatTotalInputEstimateTokens  = inputTokens
        , botStatisticsPerChatTotalOutputEstimateTokens = outputTokens
        , botStatisticsPerChatTotalApiCalls = apiCalls
        , botStatisticsPerChatTotalApiCallErrors = apiErrors
        , botStatisticsPerChatTotalApiCallSkips = apiSkips
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
        { botStatisticsBotId  = botId
        , botStatisticsTotalInputEstimateTokens  = inputTokens
        , botStatisticsTotalOutputEstimateTokens = outputTokens
        , botStatisticsTotalApiCalls = apiCalls
        , botStatisticsTotalApiCallErrors = apiErrors
        , botStatisticsTotalApiCallSkips = apiSkips
        }
      [ BotStatisticsTotalInputEstimateTokens  +=. inputTokens
      , BotStatisticsTotalOutputEstimateTokens +=. outputTokens
      , BotStatisticsTotalApiCalls +=. apiCalls
      , BotStatisticsTotalApiCallErrors +=. apiErrors
      , BotStatisticsTotalApiCallSkips +=. apiSkips
      ]
    pure ()

