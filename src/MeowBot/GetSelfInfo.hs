module MeowBot.GetSelfInfo where

import MeowBot
import MeowBot
import MeowBot.CQCode
import Control.Monad.Trans.Maybe
import Data.Maybe
import MeowBot.Parser
-- import External.ChatAPI
import Data.List.NonEmpty (NonEmpty(..), prependList)

getSelfInfo :: Meow (Maybe SelfInfo)
getSelfInfo = queries selfInfo
