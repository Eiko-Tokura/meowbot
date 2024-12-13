{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, TemplateHaskell, UndecidableInstances #-}
module System.Meow where

import Control.Concurrent.Async
import Control.Concurrent.STM
import System
import System.General
import MeowBot.BotStructure
import MeowBot.CommandRule
import Network.WebSockets hiding (Response)
import qualified Data.ByteString.Lazy as BL

import Module.LogDatabase
import Module.Command
import Module.Async
import Module.ProxyWS

-- the hierarchy of types:
--
--           SystemT r s mods m a
--           /                  \   (set s = AllData)
-- ModuleT r s l m a         CatT r mods m a
--                               |  (split AllData into (WholeChat, BotConfig) and OtherData)
--                           MeowT r mods m a
--
-- their raw definitions are
--
-- SystemT r s mods m a = ReaderStateT (AllModuleGlobalStates mods, r)  (AllModuleLocalStates mods, s) (LoggingT m) a
-- ModuleT r s l m a    = ReaderStateT (ModuleGlobalState l, r)         (ModuleLocalState l, s) (LoggingT m) a
-- CatT r mods m a      = ReaderStateT (AllModuleGlobalStates mods, r)  (AllModuleLocalStates mods, AllData) (LoggingT m) a
-- MeowT r mods m a     = ReaderStateT ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r)) (AllModuleLocalStates mods, OtherData) (LoggingT m) a
--
-- since ModuleT is not stronger than MeowT, we cannot run MeowT in ModuleT.
-- this might not be good for our AsyncModule, since it wants to run Meow [BotAction] in the module.
--
-- there are three ways to solve this:
-- 1. Deprecate ModuleT and use SystemT for all modules.
-- 2. Restrict and add a weaker type for MeowT, so that it can be run in ModuleT.
-- 3. Set a global state for the AsyncModule
-- 4. Use ModuleT (r, AllModuleGlobalStates mods) (s, AllModuleLocalStates mods) l m a instead of ModuleT r s l m a
-- 5. Define a MonadMeow class where you can run Meow in the monad, and derive a funcition that supports
--    ModuleT r s l (MeowT r mods m) a -> SystemT r s mods m a

data MeowData = MeowData
  { meowConnection :: Connection
  , meowActions    :: !(TVar [Meow [BotAction]])
  , meowCQMessage  :: !(TVar (Maybe CQMessage))
  , meowRawMessage :: !(TVar (Maybe BL.ByteString))
  }
-- | The modules loaded into the bot
type Mods   = '[CommandModule, AsyncModule, LogDatabase, ProxyWS]

-- | The monads the commands run in
type Meow a = MeowT MeowData Mods IO a

-- | The monad the bot instance runs in
type Cat  a = CatT  MeowData Mods IO a

instance HasSystemRead (TVar [Meow [BotAction]]) (MeowData) where
  readSystem = meowActions
  {-# INLINE readSystem #-}

instance HasSystemRead Connection (MeowData) where
  readSystem = meowConnection
  {-# INLINE readSystem #-}

instance HasSystemRead (TVar (Maybe CQMessage)) (MeowData) where
  readSystem = meowCQMessage
  {-# INLINE readSystem #-}

instance HasSystemRead (TVar (Maybe BL.ByteString)) (MeowData) where
  readSystem = meowRawMessage
  {-# INLINE readSystem #-}

------------------------------------------------------------------------
data BotAction 
  = BASendPrivate
      UserId     -- ^ the user to send to
      Text       -- ^ Text, the message to send
  | BASendGroup
      GroupId    -- ^ the group chat to send to
      Text       -- ^ Text, the message to send
  | BARetractMsg
      MessageId  -- ^ MessageId, the message to delete (retract)
  | BAAsync
      (Async (Meow [BotAction])) -- ^ the action to run asynchronously, which allows much powerful even continuously staged actions.
  | BAPureAsync
      (Async [BotAction])        -- ^ the action to run asynchronously, which is pure and will not further read or modify the data.

instance Show (Async (Meow [BotAction])) where
  show a = "Async (Meow BotAction) " ++ show (asyncThreadId a)

data BotCommand = BotCommand
  { identifier :: CommandId
  , command    :: Meow [BotAction]
  }
