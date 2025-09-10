module MeowBot.CostModel where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Default
import Data.PersistModel
import Data.Time (UTCTime)
import External.ChatAPI.Cost
import External.ChatAPI.Models
import MeowBot
import MeowBot.CostModel.Types
import Utils.RunDB

type Amount = Double

data PricingModel = PricingModel
  { payAsYouGoFeeRate      :: Double
  , monthlySubscriptionFee :: Double
  } deriving Show

instance Default PricingModel where
  def = PricingModel
    { payAsYouGoFeeRate      = 1/3  -- 33.3% fee on pay-as-you-go usage
    , monthlySubscriptionFee = 10.0 -- 10 per month for subscription
    }

generateNominalCost :: PricingModel -> CostModel -> Double -> Maybe Double
generateNominalCost _  Unlimited    _     = Nothing
generateNominalCost _  Subscription _     = Nothing
generateNominalCost pm PayAsYouGo   usage = Just $ usage / (1 - pm.payAsYouGoFeeRate)
generateNominalCost _  CostOnly     usage = Just usage

generateCostRecord :: Maybe ChatModel -> Maybe Text -> UTCTime -> PricingModel -> Maybe CostModel -> BotId -> ChatId -> Maybe WalletId -> TokenConsumption -> Maybe TokenCost -> ApiCostRecord
generateCostRecord chatModel apiKey time pm mcm bid cid wid consumption mActualCost =
  let mNominalCost = do
        cm <- mcm
        generateNominalCost pm cm =<< mActualCost
  in ApiCostRecord
        { apiCostRecordBotId        = bid
        , apiCostRecordChatId       = cid
        , apiCostRecordWalletId     = wid
        , apiCostRecordNominalCost  = mNominalCost
        , apiCostRecordActualCost   = mActualCost
        , apiCostRecordInputTokens  = consumption.inputTokens
        , apiCostRecordOutputTokens = consumption.outputTokens
        , apiCostRecordCacheHitRate = consumption.cacheHitRate
        , apiCostRecordChatModel    = chatModel
        , apiCostRecordTime         = time
        , apiCostRecordCoveredBySubscription = case mcm of
            Just Subscription -> Just True
            _                 -> Just False
        , apiCostRecordApiKey       = apiKey
        , apiCostRecordDescription  = Nothing
        }

-- | For a specific (bot, chat) pair, find the associated cost model and wallet
-- first finds chat-specific ownership, then global ownership, otherwise Nothing
--
-- can be wrapped in runDB as atomic if needed
findWalletAssociatedToBotChat :: BotId -> ChatId -> DB (Maybe (CostModel, Maybe WalletId))
findWalletAssociatedToBotChat bid cid = do
  mBotChatRecord <- selectFirst [BotCostModelPerChatBotId ==. bid, BotCostModelPerChatChatId ==. cid] [Desc BotCostModelPerChatInserted]
  case mBotChatRecord of
    Just (Entity _ BotCostModelPerChat { botCostModelPerChatCostModel, botCostModelPerChatWalletId }) ->
      return $ Just (botCostModelPerChatCostModel, botCostModelPerChatWalletId)
    Nothing -> do
      mBotRecord <- selectFirst [BotCostModelBotId ==. bid] [Desc BotCostModelInserted]
      case mBotRecord of
        Just (Entity _ BotCostModel { botCostModelCostModel, botCostModelWalletId }) ->
          return $ Just (botCostModelCostModel, botCostModelWalletId)
        Nothing ->
          return Nothing

data ServiceBalanceCheck
  = NoCostModelAssigned
  | HasCostModelButNoWalletAssociated
  | WalletBalanceGood    (Maybe OverdueBehavior) Amount
  | WalletBalanceLow     (Maybe OverdueBehavior) Amount
  | WalletBalanceOverdue (Maybe OverdueBehavior) Amount
  | WalletUnlimited      (Maybe Amount)
  deriving (Show, Eq)

determineOverdue :: CostModel -> Maybe OverdueBehavior -> Amount -> ServiceBalanceCheck
determineOverdue cm ob amt
  | Unlimited <- cm = WalletUnlimited (Just amt)
  | amt >= 1        = WalletBalanceGood ob amt
  | amt < 0         = WalletBalanceOverdue ob amt
  | otherwise       = WalletBalanceLow ob amt

-- | For internal service to check balance, not for end users
serviceBalanceCheck :: BotId -> ChatId -> DB ServiceBalanceCheck
serviceBalanceCheck bid cid = do
  findWalletAssociatedToBotChat bid cid >>= \case
    Just (cm, Just wid) -> do
      mWalletRecord <- get wid
      case mWalletRecord of
        Just w@(walletOverdueBehavior -> Just ob) -> return $ determineOverdue cm (Just ob) w.walletBalance
        Just w                                    -> do
          defOb <- selectFirst [] []
          return $ determineOverdue cm ((walletOverdueBehavior . entityVal =<< defOb) <|> Just def) w.walletBalance
        Nothing -> return HasCostModelButNoWalletAssociated -- impossible
    Just (Unlimited, Nothing) -> return $ WalletUnlimited Nothing
    Just (_, Nothing)         -> return HasCostModelButNoWalletAssociated
    _                         -> return NoCostModelAssigned

-- | behavior:
-- for an actual cost to be attached:
-- * apikey need to be provided, in database (ApiKeyInfo) and priced
-- * costModel need to be assigned
-- * price info need to be found for the model
-- * if the wallet exists and nominalCost > 0, wallet is deducted
--
-- consumption is always recorded
insertApiCostRecord :: UTCTime -> BotId -> ChatId -> Maybe ChatModel -> Maybe Text -> TokenConsumption -> DB (Maybe ())
insertApiCostRecord utcTime botId chatId model apiKey consumption = runMaybeT $ do
    cmPair <- lift $ findWalletAssociatedToBotChat botId chatId
    let mCostModel = fst <$> cmPair
        mWalletId  = snd =<< cmPair
    mTuple <- lift . runMaybeT $ do
      apikey     <- MaybeT (pure apiKey)
      apiKeyInfo <- MaybeT . getBy $ UniqueApiKeyInfo apikey
      MaybeT $ pure $ guard apiKeyInfo.entityVal.apiKeyInfoPriced
      modelPrice <- MaybeT $ findApiPriceInfoByKey utcTime apikey
      costModel  <- MaybeT $ pure mCostModel
      return (modelPrice, costModel, mWalletId, apikey)
    let pricingModel  = def
    case mTuple of
      Just (modelPrice, costModel, mWalletId, apikey) -> do
        let estimateCost  = tokenCost modelPrice consumption
            apiCostRecord = generateCostRecord model (Just apikey) utcTime pricingModel (Just costModel) botId chatId mWalletId consumption (Just estimateCost)
        lift $ insert_ apiCostRecord
        case (mWalletId, apiCostRecord.apiCostRecordNominalCost) of
            (Just wid, Just nominalCost) | nominalCost > 0 -> lift $ update wid [ WalletBalance -=. estimateCost ]
            _                                              -> pure ()
      Nothing -> do
        let apiCostRecord = generateCostRecord model apiKey utcTime pricingModel mCostModel botId chatId mWalletId consumption Nothing
        lift $ insert_ apiCostRecord

findApiPriceInfoByKey :: UTCTime -> Text -> DB (Maybe TokenPrice)
findApiPriceInfoByKey time key = runMaybeT $ do
  apiKeyInfo <- MaybeT $ getBy $ UniqueApiKeyInfo key
  let model = apiKeyInfo.entityVal.apiKeyInfoChatModel
  MaybeT $ findApiPriceInfo time model

findApiPriceInfo :: UTCTime -> ChatModel -> DB (Maybe TokenPrice)
findApiPriceInfo time model = do
  mInfo <- selectFirst
    ([ ApiPriceInfoChatModel ==. model ]
    <> [ ApiPriceInfoValidFrom  <=. Just time ] ||. [ ApiPriceInfoValidFrom  ==. Nothing ]
    <> [ ApiPriceInfoValidUntil >=. Just time ] ||. [ ApiPriceInfoValidUntil ==. Nothing ]
    )
    [ Desc ApiPriceInfoValidFrom ]
  return $ TokenPrice
    <$> (apiPriceInfoInputTokenPrice . entityVal =<< mInfo)
    <*> (apiPriceInfoInputTokenPriceCache . entityVal <$> mInfo)
    <*> (apiPriceInfoOutputTokenPrice . entityVal =<< mInfo)
