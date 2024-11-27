{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs, LambdaCase, TemplateHaskell, DerivingVia, OverloadedStrings #-}
module Data.Additional.Saved where

import Data.Additional
import Data.Template
import Data.Typeable
import Command.Poll.PollData (PollData(..), PollId(..))

newtype Saved a = Saved a deriving Eq via a

instance Show (Saved AdditionalData) where
  show (Saved (AdditionalDataSaved a)) = show $ Just (show $ typeOf a, a)
  show (Saved (AdditionalData _)) = "Nothing"

instance Read (Saved AdditionalData) where
  readsPrec _ s = case reads @(Maybe (String, _)) s of
    [(Just ($(patShowQ $ typeRep (Proxy @PollData)), a), s')] -> [(Saved $ AdditionalDataSaved @PollData a, s')]
    _ -> []

testSavedAdditional :: IO ()
testSavedAdditional = do
  let adt = AdditionalDataSaved @PollData (PollData (PollId 1) Nothing "test" mempty mempty)
  let saved = Saved adt :: Saved AdditionalData
  putStrLn "Testing Data:" >> print saved
  if read (show saved) == saved
    then putStrLn "Saved AdditionalData test passed"
    else error "Saved AdditionalData test FAILED"
