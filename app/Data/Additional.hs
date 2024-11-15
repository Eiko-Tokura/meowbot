{-# LANGUAGE GADTs #-}
module Data.Additional where

import Data.Typeable
import Data.Maybe

class Typeable a => IsAdditionalData a

data AdditionalData = forall a. (Typeable a, Show a, Eq a, IsAdditionalData a) => AdditionalData a 

instance Show AdditionalData where
  show (AdditionalData a) = show a

instance Eq AdditionalData where
  (AdditionalData a) == (AdditionalData b) = cast a == Just b

class HasAdditionalData a where
  {-# MINIMAL getAdditionalData, modifyAdditionalData #-}

  getAdditionalData :: a -> [AdditionalData]

  modifyAdditionalData :: ([AdditionalData] -> [AdditionalData]) -> a -> a

  getAdditionalDataType :: Typeable b => a -> [b]
  getAdditionalDataType = mapMaybe (\(AdditionalData a) -> cast a) . getAdditionalData
  {-# INLINE getAdditionalDataType #-}

  -- | Modify all element of the list that matches the type
  -- return Nothing means deletion
  modifyAdditionalDataType :: (IsAdditionalData b, Eq b, Show b, Typeable b) => (b -> Maybe b) -> a -> a
  modifyAdditionalDataType mf = modifyAdditionalData $ go mf
    where go _ [] = []
          go mf (AdditionalData a : xs) = case cast a of
            Just b -> case mf b of
              Just b' -> AdditionalData b' : go mf xs -- modification
              Nothing -> go mf xs                     -- deletion
            Nothing -> AdditionalData a : go mf xs                   -- pass
  {-# INLINE modifyAdditionalDataType #-}

