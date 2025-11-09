{-# OPTIONS_GHC -Wno-orphans #-}
module Module.BotGlobal where

import Control.Concurrent.STM
import MeowBot.BotStructure
import Module.RS.QQ
import Control.System
import Control.Monad.Effect

-- | Used to send emergency messages using all bots available. This channel is global, in scope of all bots
-- Can also add a 'broadcast channel' maybe later.
[makeRModule|
BotGlobal
  globalMessageChannel :: TVar [(ChatId, Text)]
|]

instance EventLoop FData BotGlobal mods es

data GlobalMessage
  = GlobalMessage
      { recipientChatId :: !ChatId
      , messageContent  :: !Text
      }
