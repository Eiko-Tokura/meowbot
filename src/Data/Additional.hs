{-# LANGUAGE GADTs, TemplateHaskell #-}
module Data.Additional where

import Data.Typeable
import Data.Maybe
import Control.DeepSeq

class Typeable a => IsAdditionalData a

data AdditionalData
  = forall a. (Typeable a, Show a, Eq a, IsAdditionalData a)         => AdditionalData a
  | forall a. (Typeable a, Show a, Eq a, IsAdditionalData a, Read a) => AdditionalDataSaved a

instance NFData AdditionalData where
  rnf (AdditionalData a) = let !_ = a in ()
  rnf (AdditionalDataSaved a) = let !_ = a in ()

instance Show AdditionalData where
  show (AdditionalData a) = show a
  show (AdditionalDataSaved a) = show a

instance Eq AdditionalData where
  (AdditionalData a) == (AdditionalData b) = cast a == Just b
  (AdditionalDataSaved a) == (AdditionalDataSaved b) = cast a == Just b
  _ == _ = False

-- | Filter out all saved data
filterSavedAdditional :: [AdditionalData] -> [AdditionalData]
filterSavedAdditional = filter
  (\case
    AdditionalDataSaved _ -> True
    _ -> False
  )

class HasAdditionalData a where
  {-# MINIMAL getAdditionalData, modifyAdditionalData #-}

  getAdditionalData :: a -> [AdditionalData]

  modifyAdditionalData :: ([AdditionalData] -> [AdditionalData]) -> a -> a

  getAdditionalDataType :: Typeable b => a -> [b]
  getAdditionalDataType = mapMaybe (\case
    AdditionalData a -> cast a
    _ -> Nothing
    ) . getAdditionalData
  {-# INLINE getAdditionalDataType #-}

  getAdditionalDataSavedType :: Typeable b => a -> [b]
  getAdditionalDataSavedType = mapMaybe (\case
    AdditionalDataSaved a -> cast a
    _ -> Nothing
    ) . getAdditionalData
  {-# INLINE getAdditionalDataSavedType #-}

  getAdditionalDataAllType :: Typeable b => a -> [b]
  getAdditionalDataAllType = mapMaybe (\case
    AdditionalData a -> cast a
    AdditionalDataSaved a -> cast a
    ) . getAdditionalData
  {-# INLINE getAdditionalDataAllType #-}

  -- | Modify all element of the list that matches the type
  -- return Nothing means deletion
  modifyAdditionalDataType :: (IsAdditionalData b, Eq b, Show b, Typeable b) => (b -> Maybe b) -> a -> a
  modifyAdditionalDataType mf = modifyAdditionalData $ go mf
    where go _ [] = []
          go mf (AdditionalData a : xs) = case cast a of
            Just b -> case mf b of
              Just b' -> AdditionalData b' : go mf xs -- modification
              Nothing -> go mf xs                     -- deletion
            Nothing -> AdditionalData a : go mf xs    -- pass
          go mf (anythingElse : xs) = anythingElse : go mf xs
  modifyAdditionalDataSavedType :: (IsAdditionalData b, Eq b, Show b, Typeable b, Read b) => (b -> Maybe b) -> a -> a
  modifyAdditionalDataSavedType mf = modifyAdditionalData $ go mf
    where go _ [] = []
          go mf (AdditionalDataSaved a : xs) = case cast a of
            Just b -> case mf b of
              Just b' -> AdditionalDataSaved b' : go mf xs -- modification
              Nothing -> go mf xs                          -- deletion
            Nothing -> AdditionalDataSaved a : go mf xs    -- pass
          go mf (anythingElse : xs) = anythingElse : go mf xs
  {-# INLINE modifyAdditionalDataSavedType #-}
  {-# INLINE modifyAdditionalDataType #-}

