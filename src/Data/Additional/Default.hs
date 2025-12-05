{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.Additional.Default
  ( getTypeWithDef
  , putType
  , module Data.Additional
  , Typeable
  ) where

import Data.Additional
import Data.Typeable
import Data.Maybe
import Control.Monad.IO.Class
import MeowBot.BotStructure
import System.Meow
import Control.Monad.Logger
import Module.RS

getTypeWithDef :: forall t mods m. (MeowAllData' m mods, MonadIO m, Show t, Eq t, Typeable t, IsAdditionalData t) => t -> MeowT mods m t
getTypeWithDef defT = getTypeWithDefM (pure defT)

getTypeWithDefM :: forall t mods m. (MeowAllData' m mods, MonadIO m, Show t, Eq t, Typeable t, IsAdditionalData t) => MeowT mods m t -> MeowT mods m t
getTypeWithDefM defM = do
  mt <- listToMaybe . getAdditionalDataType @_ @t <$> getS @OtherData
  case mt of
    Just pm -> return pm
    Nothing -> do
      defT <- defM
      modifyS @OtherData . modifyAdditionalData $ (:) $ AdditionalData defT
      $(logInfo) $ pack (show (typeRep (Proxy @t))) <> " initialized!"
      return defT

putType :: forall t mods m. (MeowAllData' m mods, MonadIO m, Show t, Eq t, Typeable t, IsAdditionalData t) => t -> MeowT mods m ()
putType t = modifyS @OtherData . modifyAdditionalDataType $ const $ Just t
