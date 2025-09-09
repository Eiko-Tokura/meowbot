module MeowBot.CostModel.Types where

import Utils.Persist
import Data.Default
import MeowBot.Data

data CostModel
  = Unlimited    -- ^ no cost attached at all
  | Subscription -- ^ monthly subscription, no per-use cost
  | PayAsYouGo   -- ^ pay per use, with possible fee rate
  | CostOnly     -- ^ 0 fee rate, pay per use, can be used for internal billing
  deriving (Show, Read, Eq, Ord)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CostModel)

instance Default CostModel where
  def = PayAsYouGo

data OverdueBehavior
  = DoNothing
  | DisableService
  | NotifyBillOwner          -- ^ notify the bill owner when the balance is < 0
  | NotifyBillOwnerInAdvance -- ^ notify the bill owner when the balance is low
  | NotifyChatId ChatId      -- ^ send notification to another chat instead
  deriving (Show, Read, Eq, Ord)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow OverdueBehavior)

instance Default OverdueBehavior where
  def = DoNothing
