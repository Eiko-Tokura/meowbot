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

data CostModel
  = Unlimited    -- ^ no cost attached at all
  | Subscription -- ^ monthly subscription, no per-use cost
  | PayAsYouGo   -- ^ pay per use, with possible fee rate
  | CostOnly     -- ^ 0 fee rate, pay per use, can be used for internal billing
  deriving (Show, Read, Eq)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CostModel)

instance Default CostModel where
  def = PayAsYouGo

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
