{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, TemplateHaskell, UndecidableInstances #-}
module System.General where

import Control.Monad.Effect
import Module.Logging
import Module.RS.QQ

import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import System
import Data.Kind
import Data.HList (In, getF)
import Data.Bifunctor
import MeowBot.BotStructure
import Data.Time.Clock

newtype CatT r mods m a = CatT { runCatT :: SystemT r AllData mods m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)
  deriving
    ( MonadReader (AllModuleGlobalStates mods, r)
    , MonadState (AllModuleLocalStates mods, AllData)
    ) via ReaderStateT (AllModuleGlobalStates mods, r) (AllModuleLocalStates mods, AllData) (LoggingT m)
-- ^ the monad that the bot runs in
-- running in this monad it is necessary to block other threads from modifying the data.
-- so avoid running long blocking operations in this monad, use async and staged actions instead.
-- type System mods = SystemT () AllData mods (LoggingT IO) ()

-- | The monad transformer that the bot runs in.
newtype MeowT (r :: Type) (mods :: [Type]) (m :: Type -> Type) a = MeowT
  { runMeowT ::
      ReaderStateT
        ( (WholeChat, BotConfig)
        , (AllModuleGlobalStates mods, r)
        )
        ( AllModuleLocalStates mods
        , OtherData
        )
        (LoggingT m) -- ^ the monad to run in
        a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)
  deriving
    ( MonadReader ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r))
    , MonadState (AllModuleLocalStates mods, OtherData)
    ) via ReaderStateT ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r)) (AllModuleLocalStates mods, OtherData) (LoggingT m)

deriving newtype instance MonadIO m => MonadLoggerIO (MeowT r mods m)

-----------------------------------------------------------------------
instance MonadTrans (MeowT r mods) where
  lift = MeowT . lift . lift
  {-# INLINE lift #-}

instance Monad m => MonadReadable BotConfig (MeowT r mods m) where
  query = asks (snd . fst)
  {-# INLINE query #-}

instance Monad m => MonadReadable BotModules (MeowT r mods m) where
  query = queries botModules
  {-# INLINE query #-}

instance Monad m => MonadReadable RunningMode (MeowT r mods m) where
  query = queries runningMode
  {-# INLINE query #-}

instance Monad m => MonadReadable WholeChat (MeowT r mods m) where
  query = asks (fst . fst)
  {-# INLINE query #-}

instance Monad m => MonadReadable BotName (MeowT r mods m) where
  query = queries nameOfBot
  {-# INLINE query #-}

instance Monad m => MonadReadable BotId (MeowT r mods m) where
  query = queries botId
  {-# INLINE query #-}

-- instance Monad m => MonadReadable CQMessage (MeowT r mods m) where
--   query = asks (getNewMsg . fst . fst)
--   {-# INLINE query #-}

instance MonadIO m => MonadReadable UTCTime (MeowT r mods m) where
  query = liftIO getCurrentTime
  {-# INLINE query #-}

instance Monad m => MonadReadable (AllModuleGlobalStates mods) (MeowT r mods m) where
  query = asks (fst . snd)
  {-# INLINE query #-}

instance (Monad m, mod `In` mods) => MonadReadable (ModuleGlobalState mod) (MeowT r mods m) where
  query = queries (getF @mod @mods)
  {-# INLINE query #-}

instance Monad m => MonadReadable AllData (MeowT r mods m) where
  query = do
    (wc, bc) <- asks fst
    (_, od) <- get
    return $ AllData wc bc od
  {-# INLINE query #-}

instance Monad m => MonadReadable OtherData (MeowT r mods m) where
  query = gets snd
  {-# INLINE query #-}

instance Monad m => MonadModifiable OtherData (MeowT r mods m) where
  change f = modify $ second f
  {-# INLINE change #-}

------------------------------------------------------------------------
instance MonadTrans (CatT r mods) where
  lift = CatT . lift . lift
  {-# INLINE lift #-}

instance Monad m => MonadReadable AllData (CatT r mods m) where
  query = gets snd
  {-# INLINE query #-}

instance Monad m => MonadModifiable AllData (CatT r mods m) where
  change f = modify $ second f
  {-# INLINE change #-}

