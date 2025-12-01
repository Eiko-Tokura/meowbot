{-# OPTIONS_GHC -Wno-orphans #-}
module Module.BotGlobal where

import Control.Concurrent.STM
import MeowBot.BotStructure
import Module.RS.QQ
import Control.System
import Control.Monad.Effect
import Data.Set (Set)
import qualified Data.Set as Set

-- | Used to send emergency messages using all bots available. This channel is global, in scope of all bots
-- Can also add a 'broadcast channel' maybe later.
[makeRModule|
BotGlobal
  globalMessageChannel :: TVar [(ChatId, Text)]
  globalBotAccountIds  :: TVar (Set UserId)
|]

userIdInGlobalBots :: (MonadIO m, In BotGlobal mods) => UserId -> EffT mods es m Bool
userIdInGlobalBots uid = do
  botIdsVar <- asksModule globalBotAccountIds
  botIds <- liftIO $ readTVarIO botIdsVar
  return $ uid `Set.member` botIds

instance EventLoop FData BotGlobal mods es

data GlobalMessage
  = GlobalMessage
      { recipientChatId :: !ChatId
      , messageContent  :: !Text
      }
