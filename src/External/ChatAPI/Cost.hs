module External.ChatAPI.Cost 
  ( tokenCost, TokenCost
  , EstimateTokens(..)
  , APIInfo(..)
  , TokenPrice(..), TokenConsumption(..)
  , deepSeekTokenPrice
  , meowBotCacheHitRate
  , modelPrice
  ) where
import Control.Applicative
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import GHC.Generics (Generic)
import Data.Default (Default, def)
import Control.DeepSeq (NFData)
import External.ChatAPI.Models

data APIInfo = APIInfo
  { apiKey :: Text
  , price  :: Maybe TokenPrice
  } deriving (Show, Eq, Generic, NFData)

instance Semigroup APIInfo where
  APIInfo _k1 p1 <> APIInfo k2 p2 = APIInfo k2 (p2 <|> p1)

data EstimateTokens = EstimateTokens
  { inputTokens  :: !Int
  , outputTokens :: !Int
  , apiCalls     :: !Int
  , apiErrors    :: !Int
  , apiSkips     :: !Int
  , apiInfo      :: !(Maybe APIInfo)
  } deriving (Show, Eq, Generic, NFData, Default)

instance Semigroup EstimateTokens where
  EstimateTokens i1 o1 a1 b1 c1 d1 <> EstimateTokens i2 o2 a2 b2 c2 d2
    = EstimateTokens (i1 + i2) (o1 + o2) (a1 + a2) (b1 + b2) (c1 + c2) (d1 <> d2)

instance Monoid EstimateTokens where
  mempty = def

data TokenPrice = TokenPrice
  { inputTokenPrice      :: Double
  , inputTokenPriceCache :: Maybe Double
  , outputTokenPrice     :: Double
  } deriving (Show, Eq, Generic, NFData)

data TokenConsumption = TokenConsumption
  { inputTokens  :: Int
  , outputTokens :: Int
  , cacheHitRate :: Maybe Double
  } deriving (Show, Eq, Generic, NFData)

type TokenCost = Double
tokenCost :: TokenPrice -> TokenConsumption -> TokenCost
tokenCost TokenPrice {..} TokenConsumption {..} =
  let costInputTokens = case (inputTokenPriceCache, cacheHitRate) of
        (Just cachePrice, Just hitRate) ->
          fromIntegral inputTokens * (inputTokenPrice * (1 - hitRate) + cachePrice * hitRate)
        _ -> fromIntegral inputTokens * inputTokenPrice
      costOutputTokens = fromIntegral outputTokens * outputTokenPrice
  in costInputTokens + costOutputTokens

meowBotCacheHitRate :: Double
meowBotCacheHitRate = 0.1

modelPrice :: ChatModel -> UTCTime -> Maybe TokenPrice
modelPrice (DeepSeek DeepSeekChat) t = Just $ deepSeekTokenPrice t
modelPrice _ _ = Nothing

deepSeekTokenPrice :: UTCTime -> TokenPrice
deepSeekTokenPrice u -- price change on 2025-09-05 UTC 16:00
  | u < UTCTime (fromGregorian 2025 9 5) (secondsToDiffTime (16 * 3600)) = oldPrice
  | otherwise = newPrice
  where
    usdToCny = 7.16
    oldPrice = TokenPrice
      { inputTokenPrice      = 0.27e-6 * usdToCny
      , inputTokenPriceCache = Just (0.07e-6 * usdToCny)
      , outputTokenPrice     = 1.1e-6 * usdToCny
      }
    newPrice = TokenPrice
      { inputTokenPrice      = 0.56e-6 * usdToCny
      , inputTokenPriceCache = Just (0.07e-6 * usdToCny)
      , outputTokenPrice     = 1.68e-6 * usdToCny
      }
