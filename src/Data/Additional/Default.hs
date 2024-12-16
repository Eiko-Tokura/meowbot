{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.Additional.Default 
  ( getTypeWithDef
  , module Data.Additional
  , Typeable
  ) where

import Data.Additional
import Data.Typeable
import Data.Maybe
import Control.Monad.IO.Class
import MeowBot.BotStructure
import System.General
import Control.Monad.Logger

getTypeWithDef :: forall t r mods m. (MonadIO m, Show t, Eq t, Read t, Typeable t, IsAdditionalData t) => t -> MeowT r mods m t
getTypeWithDef defT = do
  mt <- listToMaybe . getAdditionalDataType @_ @t <$> query @OtherData
  case mt of
    Just pm -> return pm
    Nothing -> do
      let emptyMap = defT
      change @OtherData . modifyAdditionalData $ (:) $ AdditionalData emptyMap
      $(logInfo) $ pack (show (typeRep (Proxy @t))) <> " initialized!"
      return emptyMap

