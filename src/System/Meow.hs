{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, UndecidableInstances #-}
module System.Meow where

import Control.Monad.Effect
import Module.RS.QQ
import Module.RS

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.System

import MeowBot.BotStructure
import MeowBot.CommandRule
import qualified Data.ByteString.Lazy as BL

import Module.Logging
import Module.LogDatabase
import Module.Async
import Module.ProxyWS
import Module.ConnectionManager
import Module.StatusMonitor
import Module.CronTabTick
import Module.MeowTypes
import Module.RecvSentCQ
import Module.MeowConnection
import Module.Prometheus

import Data.UpdateMaybe
import Data.Aeson

-- | The modules loaded into the bot
type Mods =
  [ LogDatabase
  , AsyncModule
  , ConnectionManagerModule
  , ProxyWS
  , CronTabTickModule
  , StatusMonitorModule
  , RecvSentCQ
  , MeowActionQueue
  , MeowConnection
  , SModule BotConfig
  , SModule WholeChat
  , SModule OtherData
  , MeowDatabase
  , Prometheus
  , LoggingModule
  ]
  -- '[CronTabTickModule, StatusMonitorModule, AsyncModule, CommandModule, LogDatabase, ProxyWS, ConnectionManagerModule]

type MeowErrs = '[SystemError]

-- | The monads the commands run in
type MeowT mods m = EffT mods MeowErrs m

type Meow = MeowT Mods IO

data SomeQueryAPI
  = forall queryType. (FromJSON (WithEcho (QueryAPIResponse queryType)), ToJSON (ActionForm (QueryAPI queryType)))
  => SomeQueryAPI (QueryAPI queryType) (QueryAPIResponse queryType -> Meow [BotAction])

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
      SomeQueryAPI
  | BARawQueryCallBack
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
MeowActionQueue
  meowReadsAction  :: TVar [Meow [BotAction]]
  meowReadsQueries :: TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) )]
|]

instance SystemModule MeowActionQueue where
  data ModuleInitData MeowActionQueue = MeowActionQueueInitData
  data ModuleEvent    MeowActionQueue = MeowActionQueueEvent

instance EventLoop c MeowActionQueue mods es where

withMeowActionQueue
  :: (ConsFDataList FData (MeowActionQueue : mods), MonadIO m)
  => EffT (MeowActionQueue : mods) es m a -> EffT mods es m a
withMeowActionQueue act = do
  varActions <- liftIO $ newTVarIO []
  varQueries <- liftIO $ newTVarIO []
  runEffTOuter_ (MeowActionQueueRead varActions varQueries) MeowActionQueueState act

overrideMeow :: OverrideSettings -> Meow a -> Meow a
overrideMeow override = localByState $ \bc -> bc { overrideSettings = Just override }
{-# INLINE overrideMeow #-}

instance Monad m => Show (Async (m [BotAction])) where
  show a = "Async (Meow BotAction) " ++ show (asyncThreadId a)

data BotCommand = BotCommand
  { identifier :: CommandId
  , command    :: Meow [BotAction]
  }
