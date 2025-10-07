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

getTypeWithDef :: forall t mods m. (MeowAllData mods, MonadIO m, Show t, Eq t, Typeable t, IsAdditionalData t) => t -> MeowT mods m t
getTypeWithDef defT = do
  mt <- listToMaybe . getAdditionalDataType @_ @t <$> getS @OtherData
  case mt of
    Just pm -> return pm
    Nothing -> do
      let emptyMap = defT
      modifyS @OtherData . modifyAdditionalData $ (:) $ AdditionalData emptyMap
      $(logInfo) $ pack (show (typeRep (Proxy @t))) <> " initialized!"
      return emptyMap

putType :: forall t mods m. (MeowAllData mods, MonadIO m, Show t, Eq t, Typeable t, IsAdditionalData t) => t -> MeowT mods m ()
putType t = modifyS @OtherData . modifyAdditionalDataType $ const $ Just t
