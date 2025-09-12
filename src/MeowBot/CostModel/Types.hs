module MeowBot.CostModel.Types where

import Utils.Persist
import Data.Default
import MeowBot.Data
import Text.Printf

newtype Amount = Amount { unAmount :: Double }
  deriving newtype (Show, Eq, Ord, Num, Fractional, PersistField, PersistFieldSql)

newtype ActualCost = ActualCost { unActualCost :: Double }
  deriving newtype (Show, Eq, Ord, Num, Fractional, PersistField, PersistFieldSql)

-- | displayed as decimals instead of scientific notation
instance ToText Amount Text where
  toText (Amount a)     = pack $ printf "%.3f" a

instance ToText ActualCost Text where
  toText (ActualCost a) = pack $ printf "%.8f" a

newtype PayAsYouGoFeeRate = PayAsYouGoFeeRate { unPayAsYouGoFeeRate :: Double }
  deriving newtype (Show, Read, Eq, Ord, Num, Fractional, PersistField, PersistFieldSql)

newtype MonthlySubscriptionFee = MonthlySubscriptionFee { unMonthlySubscriptionFee :: Double }
  deriving newtype (Show, Read, Eq, Ord, Num, Fractional, PersistField, PersistFieldSql)

newtype DailyBasicCost = DailyBasicCost { unDailyBasicCost :: Double }
  deriving newtype (Show, Read, Eq, Ord, Num, Fractional, PersistField, PersistFieldSql)

-- | Used for filling default
data PricingModel = PricingModel
  { payAsYouGoFeeRate      :: PayAsYouGoFeeRate
  , monthlySubscriptionFee :: MonthlySubscriptionFee
  , dailyBasicCost         :: DailyBasicCost
  } deriving Show

-- | The default pricing model is:
instance Default PricingModel where
  def = PricingModel
    { payAsYouGoFeeRate      = 1/3  -- 33.3% fee on pay-as-you-go usage
    , monthlySubscriptionFee = 10.0 -- 10 per month for subscription
    , dailyBasicCost         = 0.33 -- 10 / 30 days
    }

instance Default MonthlySubscriptionFee where def = monthlySubscriptionFee ( def :: PricingModel )
instance Default PayAsYouGoFeeRate      where def = payAsYouGoFeeRate      ( def :: PricingModel )
instance Default DailyBasicCost         where def = dailyBasicCost         ( def :: PricingModel )

data CostModel
  = Unlimited                                                  -- ^ no cost attached at all
  | Subscription MonthlySubscriptionFee                        -- ^ monthly subscription, no per-use cost
  | PayAsYouGo   PayAsYouGoFeeRate      (Maybe DailyBasicCost) -- ^ pay per use, with possible fee rate
  | CostOnly     (Maybe DailyBasicCost)                        -- ^ 0 fee rate, pay per use
  deriving (Show, Read, Eq)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CostModel)

instance ToText CostModel Text where
  toText Unlimited             = "Unlimited"
  toText (Subscription fee)    = "Subscription (" <> toText fee <> " per month)"
  toText (PayAsYouGo _ mDaily) = "PayAsYouGo"     <> maybe "" (\d -> " +" <> toText d <> "/day") mDaily
  toText (CostOnly mDaily)     = "CostOnly"       <> maybe "no daily cost" (\d -> toText d <> " daily cost") mDaily

chinoCostModel :: CostModel
chinoCostModel = PayAsYouGo (payAsYouGoFeeRate def) (Just $ dailyBasicCost def)

instance Default CostModel where
  def = PayAsYouGo (payAsYouGoFeeRate def) Nothing

data OverdueNotification
  = NotifyBillOwner              -- ^ notify the bill owner when the balance is < 0
  | NotifyBillOwnerInAdvance     -- ^ notify the bill owner when the balance is low
  | NotifyChatId ChatId          -- ^ send notification to another chat instead
  | NotifyChatIdInAdvance ChatId -- ^ send notification to another chat when the balance is low
  deriving (Show, Read, Eq)

data OverdueBehavior
  = DoNothing      [OverdueNotification] -- ^ just notify, do nothing
  | DisableService [OverdueNotification] -- ^ disable the service until the balance is topped up
  deriving (Show, Read, Eq)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow OverdueBehavior)

onNotis :: ([OverdueNotification] -> [OverdueNotification]) -> OverdueBehavior -> OverdueBehavior
onNotis f (DoNothing ns)      = DoNothing (f ns)
onNotis f (DisableService ns) = DisableService (f ns)

toDoNothing :: OverdueBehavior -> OverdueBehavior
toDoNothing (DoNothing ns)      = DoNothing ns
toDoNothing (DisableService ns) = DoNothing ns

instance Default OverdueBehavior where
  def = DoNothing []

hasDailyCost :: CostModel -> Bool
hasDailyCost cm = case getDailyCost cm of
  Just _  -> True
  Nothing -> False

getDailyCost :: CostModel -> Maybe DailyBasicCost
getDailyCost (CostOnly (Just d))     = Just d
getDailyCost (PayAsYouGo _ (Just d)) = Just d
getDailyCost (Unlimited; Subscription _; PayAsYouGo _ _; CostOnly _) = Nothing
