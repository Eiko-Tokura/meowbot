module MeowBot.Data.Parser
  ( module MeowBot.Data.Parser
  , module MeowBot.Parser
  ) where

import MeowBot.Data
import MeowBot.Parser
import Data.Text (Text)
import Data.PersistModel
import Utils.Persist

chatIdP :: Parser Text Char ChatId
chatIdP = asum
  [ string "user"  >> spaces >> PrivateChat . UserId  <$> int
  , string "group" >> spaces >> GroupChat   . GroupId <$> int
  ]

botIdP :: Parser Text Char BotId
botIdP = BotId <$> int

walletIdP :: Parser Text Char WalletId
walletIdP = intToKey <$> int
