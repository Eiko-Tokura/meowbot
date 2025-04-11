{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, TemplateHaskell, UndecidableInstances #-}
module System.Meow where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
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
import Module.ConnectionManager
import Module.StatusMonitor
import Module.CronTabTick

import Data.HList
import Data.Kind

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

newtype MeowData = MeowData (CList MeowDataClass
  [ Connection
  , TVar [Meow [BotAction]]
  , TVar (Maybe SentCQMessage)
  , TVar (Maybe ReceCQMessage)
  , TVar (Maybe BL.ByteString)
  ])

instance HasSystemRead c (CList MeowDataClass
  [ Connection
  , TVar [Meow [BotAction]]
  , TVar (Maybe SentCQMessage)
  , TVar (Maybe ReceCQMessage)
  , TVar (Maybe BL.ByteString)
  ]) => HasSystemRead c MeowData where
  readSystem (MeowData clist) = readSystem clist
  {-# INLINE readSystem #-}

instance HasSystemRead t (CList MeowDataClass (t ': ts)) where
  readSystem (CCons x _) = x
  {-# INLINE readSystem #-}

instance {-# OVERLAPPABLE #-} HasSystemRead t (CList MeowDataClass ts) => HasSystemRead t (CList MeowDataClass (t' ': ts)) where
  readSystem (CCons _ xs) = readSystem xs
  {-# INLINE readSystem #-}

-------------MeowDataClass-------------
class MeowDataClass a where
  type MeowDataInit a :: Type
  initOneMeowData :: MeowDataInit a -> IO a

instance MeowDataClass (TVar (Maybe a)) where
  type MeowDataInit (TVar (Maybe a)) = ()
  initOneMeowData _ = newTVarIO Nothing
  {-# INLINE initOneMeowData #-}

instance MeowDataClass (TVar [a]) where
  type MeowDataInit (TVar [a]) = ()
  initOneMeowData _ = newTVarIO []
  {-# INLINE initOneMeowData #-}

instance MeowDataClass Connection where
  type MeowDataInit Connection = Connection
  initOneMeowData = return
  {-# INLINE initOneMeowData #-}

class InitMeowDataClass a where
  type MeowDataAllInit a :: [Type]
  initAllMeowData :: HList (MeowDataAllInit a) -> IO a

instance InitMeowDataClass (CList MeowDataClass '[]) where
  type MeowDataAllInit (CList MeowDataClass '[]) = '[]
  initAllMeowData _ = return CNil
  {-# INLINE initAllMeowData #-}

instance (MeowDataClass p, InitMeowDataClass (CList MeowDataClass ps)) => InitMeowDataClass (CList MeowDataClass (p : ps)) where
  type MeowDataAllInit (CList MeowDataClass (p : ps)) = MeowDataInit p : MeowDataAllInit (CList MeowDataClass ps)
  initAllMeowData (x :* xs) = CCons <$> initOneMeowData x <*> initAllMeowData xs
  {-# INLINE initAllMeowData #-}

-- -- need a
-- class HelperClass as bs where
--   function :: Monad m => (HList as -> m (CList MeowDataClass as)) -> (HList bs -> m (CList MeowDataClass bs)) -> HList (as ++ bs) -> m (CList MeowDataClass (as ++ bs))
--
-- -- perform double induction on as and bs
-- instance HelperClass '[] bs where
--   function _ f xs = f xs()
--   {-# INLINE function #-}
--
-- instance HelperClass as '[] where
--   function f _ xs = f xs
--   {-# INLINE function #-}
--
-- instance (MeowDataClass a, HelperClass as bs) => HelperClass (a : as) bs where
--   function f g (x :* xs) = CCons <$> _ <*> _

-- | The modules loaded into the bot
type Mods   = '[CronTabTickModule, StatusMonitorModule, AsyncModule, CommandModule, LogDatabase, ProxyWS, ConnectionManagerModule]

-- | The monads the commands run in
type Meow a = MeowT MeowData Mods IO a

-- | The monad the bot instance runs in
type Cat  a = CatT  MeowData Mods IO a


overrideMeow :: OverrideSettings -> Meow a -> Meow a
overrideMeow override = local
  ( \((wc, bc), other) -> ((wc, bc { overrideSettings = Just override }), other)
  )

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
  | BAActionAPI
      ActionAPI  -- ^ General actionAPI, the action to perform
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
