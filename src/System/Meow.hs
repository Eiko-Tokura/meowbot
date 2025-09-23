{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, UndecidableInstances #-}
module System.Meow where

import Control.Monad.Effect
import Module.RS.QQ

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader

import MeowBot.BotAction
import MeowBot.BotStructure
import MeowBot.CommandRule
import Network.WebSockets hiding (Response)
import qualified Data.ByteString.Lazy as BL

import Module.LogDatabase
import Module.Command
import Module.Async
import Module.ProxyWS
import Module.ConnectionManager
import Module.StatusMonitor
import Module.CronTabTick

import Data.UpdateMaybe
import Data.HList
import Data.Kind

-- | The monad the bot instance runs in
type Cat  a = CatT  MeowData Mods IO a

-- | The modules loaded into the bot
type Mods   = '[LogDatabase] -- '[CronTabTickModule, StatusMonitorModule, AsyncModule, CommandModule, LogDatabase, ProxyWS, ConnectionManagerModule]

-- | The monads the commands run in
type MeowT mods = EffT ('[SModule WholeChat, SModule BotConfig, SModule OtherData] ++ mods) '[SystemError]

type CatT mods = MeowT mods

type Meow a = MeowT MeowData Mods IO a

------------------------------------------------------------------------
data BotAction
  = BASendPrivate
      UserId     -- ^ the user to send to
      Text       -- ^ Text, the message to send
  | BASendGroup
      GroupId    -- ^ the group chat to send to
      Text       -- ^ Text, the message to send
  | BARetractMsg
      CQMessageId  -- ^ CQMessageId, the message to delete (retract)
  | BAActionAPI
      ActionAPI  -- ^ General actionAPI, the action to perform
  | BAQueryAPI
      [BL.ByteString -> Maybe (Meow [BotAction])] -- ^ run queries
  | BAAsync
      (Async (Meow [BotAction])) -- ^ the action to run asynchronously, which allows much powerful even continuously staged actions.
  | BAPureAsync
      (Async [BotAction])        -- ^ the action to run asynchronously, which is pure and will not further read or modify the data.
  | BASimpleAction (Meow ()) -- ^ a simple action that can be run immediately, do not put anything that blocks the main thread.
  | BADelayedAction      Int  (Meow [BotAction]) -- ^ a delayed action that will be executed after the specified milliseconds.
  | BADelayedPureAction  Int  [BotAction]
  | BADelayedPureAction1 Int  BotAction

---

-- | Because of the reference to Meow, have to put it here
[makeRModule__|
MeowData
  meowReadsConnection    :: Connection
  meowReadsAction        :: TVar [Meow [BotAction]]
  meowReadsQueries       :: TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) )]
|]

overrideMeow :: OverrideSettings -> Meow' a -> Meow' a
overrideMeow override = local
  ( \((wc, bc), other) -> ((wc, bc { overrideSettings = Just override }), other)
  )

instance Show (Async (Meow [BotAction])) where
  show a = "Async (Meow BotAction) " ++ show (asyncThreadId a)

data BotCommand = BotCommand
  { identifier :: CommandId
  , command    :: Meow [BotAction]
  }
