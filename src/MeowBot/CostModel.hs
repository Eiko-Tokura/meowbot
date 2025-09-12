{-# LANGUAGE TransformListComp #-}
module MeowBot.CostModel
  ( module MeowBot.CostModel.Types
  , module MeowBot.CostModel
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Maybe (fromMaybe)
import Data.PersistModel
import Data.Time (UTCTime)
import External.ChatAPI.Cost
import External.ChatAPI.Models
import MeowBot
import MeowBot.CostModel.Types
import Utils.RunDB
import Utils.ListComp
import qualified Data.Text as T

actualCostToAmount :: ActualCost -> PayAsYouGoFeeRate -> Amount
actualCostToAmount (ActualCost ac) (PayAsYouGoFeeRate fr) = Amount $ ac / (1 - fr)

generateNominalCost :: CostModel -> ActualCost -> Maybe Amount
generateNominalCost Unlimited               _     = Nothing
generateNominalCost Subscription{}          _     = Nothing
generateNominalCost (PayAsYouGo feeRate _)  usage = Just $ actualCostToAmount usage feeRate
generateNominalCost (CostOnly _)            usage = Just $ coerce usage

generateCostRecord :: Maybe ChatModel -> Maybe Text -> UTCTime -> Maybe CostModel -> BotId -> ChatId -> Maybe WalletId -> TokenConsumption -> Maybe ActualCost -> ApiCostRecord
generateCostRecord chatModel apiKey time mcm bid cid wid consumption mActualCost =
  let mNominalCost = do
        cm <- mcm
        generateNominalCost cm =<< mActualCost
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
            Just Subscription{} -> Just True
            _                   -> Just False
        , apiCostRecordApiKey       = apiKey
        , apiCostRecordDescription  = Nothing
        }

-- | For a specific (bot, chat) pair, find the associated cost model and wallet
-- first finds chat-specific ownership, then global ownership, otherwise Nothing
--
-- can be wrapped in runDB as atomic if needed
findWalletAssociatedToBotChat :: BotId -> ChatId -> DB (Maybe (CostModel, WalletId))
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

type WalletInfo = Entity Wallet
data ServiceBalanceCheck
  = NoCostModelAssigned
  | WalletBalanceGood    WalletInfo
  | WalletBalanceLow     WalletInfo
  | WalletBalanceOverdue WalletInfo
  | WalletUnlimited      (Maybe WalletInfo)
  deriving (Show, Eq)

determineOverdue :: CostModel -> WalletInfo -> ServiceBalanceCheck
determineOverdue cm w@(walletBalance . entityVal -> amt)
  | Unlimited <- cm = WalletUnlimited (Just w)
  | amt >= low      = WalletBalanceGood w
  | amt < 0         = WalletBalanceOverdue w
  | otherwise       = WalletBalanceLow w
  where low = fromMaybe 1 w.entityVal.walletLowThreshold

-- | Migration note:
-- After the entire system being tracked,
-- the NoCostModelAssigned and HasCostModelButNoWalletAssociated states should respond to DisableService
-- but for now, we keep the old behavior of doing nothing
overdueBehaviorNeedAction :: ServiceBalanceCheck -> Maybe (OverdueBehavior, WalletInfo)
overdueBehaviorNeedAction NoCostModelAssigned                                                      = Nothing
overdueBehaviorNeedAction (WalletUnlimited _)                                                      = Nothing
overdueBehaviorNeedAction (WalletBalanceGood _)                                                    = Nothing
overdueBehaviorNeedAction (WalletBalanceLow w@(walletOverdueBehavior . entityVal -> Just act))     = Just (toDoNothing $ onNotis inAdvanceNoti act, w)
overdueBehaviorNeedAction (WalletBalanceLow _)                                                     = Nothing
overdueBehaviorNeedAction (WalletBalanceOverdue w@(walletOverdueBehavior . entityVal -> Just act)) = Just (onNotis overdueNoti act, w)
overdueBehaviorNeedAction (WalletBalanceOverdue _)                                                 = Nothing

overdueNoti :: [OverdueNotification] -> [OverdueNotification]
overdueNoti = filter (\case
  NotifyBillOwner -> True
  NotifyChatId _  -> True
  _ -> False)

inAdvanceNoti :: [OverdueNotification] -> [OverdueNotification]
inAdvanceNoti = filter (\case
  NotifyBillOwnerInAdvance -> True
  NotifyChatIdInAdvance _  -> True
  _ -> False)

-- | For internal service to check balance, not for end users
serviceBalanceCheck :: BotId -> ChatId -> DB ServiceBalanceCheck
serviceBalanceCheck bid cid = do
  findWalletAssociatedToBotChat bid cid >>= \case
    Just (Unlimited, _) -> return $ WalletUnlimited Nothing
    Just (cm, wid) -> do
      mWalletRecord <- get wid
      case mWalletRecord of
        Just w@(walletOverdueBehavior -> Just _) -> return $ determineOverdue cm (Entity wid w)
        Just w                                   -> do
          defOb <- selectFirst [] []
          return $ determineOverdue cm (Entity wid w
            { walletOverdueBehavior = (walletOverdueDefaultOverdueBehavior <=< fmap entityVal) defOb })
        Nothing -> return NoCostModelAssigned -- impossible
    _           -> return NoCostModelAssigned

-- | For internal service to check balance, not for end users
serviceBalanceActionCheck :: BotId -> ChatId -> DB (Maybe (OverdueBehavior, WalletInfo))
serviceBalanceActionCheck bid cid = overdueBehaviorNeedAction <$> serviceBalanceCheck bid cid

getBotPerChatCostModel :: BotId -> DB [Entity BotCostModelPerChat]
getBotPerChatCostModel botId = do
  list <- selectList [BotCostModelPerChatBotId ==. botId] [Desc BotCostModelPerChatInserted]
  return  [ head' entity
          | entity@(Entity _ modelPerChat) <- list
          , then group by botCostModelPerChatChatId modelPerChat using groupWith
          ]

getBotCostModel :: BotId -> DB (Maybe (Entity BotCostModel))
getBotCostModel botId = selectFirst [BotCostModelBotId ==. botId] [Desc BotCostModelInserted]

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
        mWalletId  = snd <$> cmPair
    mTuple <- lift . runMaybeT $ do
      apikey     <- MaybeT (pure apiKey)
      apiKeyInfo <- MaybeT . getBy $ UniqueApiKeyInfo apikey
      MaybeT $ pure $ guard apiKeyInfo.entityVal.apiKeyInfoPriced
      modelPrice <- MaybeT $ findApiPriceInfoByKey utcTime apikey
      return (modelPrice, mWalletId, apikey)
    case mTuple of
      Just (modelPrice, mWalletId, apikey) -> do
        let estimateCost  = ActualCost $ tokenCost modelPrice consumption
            apiCostRecord = generateCostRecord model (Just apikey) utcTime mCostModel botId chatId mWalletId consumption (Just estimateCost)
        lift $ insert_ apiCostRecord
        case (mWalletId, apiCostRecord.apiCostRecordNominalCost) of
            (Just wid, Just nominalCost) | nominalCost > 0 -> lift $ update wid [ WalletBalance -=. nominalCost ]
            _                                              -> pure ()
      Nothing -> do
        let apiCostRecord = generateCostRecord model apiKey utcTime mCostModel botId chatId mWalletId consumption Nothing
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

-- | Check and send notification if needed, returns list of actions to perform
-- checks walletOverdueNotified, when already notified, no action is taken
checkSendNotis :: BotName -> BotId -> ChatId -> OverdueNotification -> WalletInfo -> Meow [BotAction]
checkSendNotis botname botid cid noti winfo =
  let notified = fromMaybe False winfo.entityVal.walletOverdueNotified
  in case noti of
  NotifyChatId notifyCid -> do
    let text = T.unwords
          [ "Note: The bot"
          , toText botname
          , "with id"
          , toText botid
          , "in chat"
          , toText cid
          , "has insufficient balance:"
          , toText winfo.entityVal.walletBalance
          ]
    if not notified
    then do
      runDB $ update winfo.entityKey [ WalletOverdueNotified =. Just True ]
      return [baSendToChatId notifyCid text]
    else return []
  NotifyChatIdInAdvance notifyCid -> do
    let text = T.unwords
          [ "Note: The bot"
          , toText botname
          , "with id"
          , toText botid
          , "in chat"
          , toText cid
          , "has low balance:"
          , toText winfo.entityVal.walletBalance
          ]
    if not notified
    then do
      runDB $ update winfo.entityKey [ WalletOverdueNotified =. Just True ]
      return [baSendToChatId notifyCid text]
    else return []
  NotifyBillOwner          -> do
    let text = T.unwords
          [ "Note: The bot you ("
          , toText ownerCid
          , ") own"
          , toText botname
          , "with id"
          , toText botid
          , "in chat"
          , toText cid
          , "has insufficient balance:"
          , toText winfo.entityVal.walletBalance
          ]
        ownerCid = ownerChatId winfo.entityVal.walletOwnerId
    if not notified
    then do
      runDB $ update winfo.entityKey [ WalletOverdueNotified =. Just True ]
      return [baSendToChatId ownerCid text]
    else return []
  NotifyBillOwnerInAdvance -> do
    let text = T.unwords
          [ "Note: The bot you ("
          , toText ownerCid
          , ") own"
          , toText botname
          , "with id"
          , toText botid
          , "in chat"
          , toText cid
          , "has low balance:"
          , toText winfo.entityVal.walletBalance
          ]
        ownerCid = ownerChatId winfo.entityVal.walletOwnerId
    if not notified
    then do
      runDB $ update winfo.entityKey [ WalletOverdueNotified =. Just True ]
      return [baSendToChatId ownerCid text]
    else return []
