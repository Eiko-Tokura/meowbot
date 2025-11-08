module External.ChatAPI.ModelPricing where

import Data.Time
import External.ChatAPI.Cost
import External.ChatAPI.Models
import Module.RS.QQ

[makeRModule__|
ModelPricing
  modelPricing :: !(ChatModel -> UTCTime -> Maybe TokenPrice)
|]
