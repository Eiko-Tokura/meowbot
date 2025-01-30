{-# LANGUAGE GADTs, TemplateHaskell, DerivingVia, OverloadedStrings #-}
module Data.Additional.Saved where

import Data.Additional
import Data.Template
import Data.Typeable
import Command.Poll.PollData (PollData(..), PollId(..))
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe, listToMaybe)
import Parser.Template (asumE)

newtype Saved a = Saved a deriving Eq via a

type Saved_AdditionalData = Saved AdditionalData

instance Show (Saved AdditionalData) where
  show (Saved (AdditionalDataSaved a)) = show $ Just (show $ typeOf a, a)
  show (Saved (AdditionalData _)) = "Nothing"

instance Read (Saved AdditionalData) where -- ^ all types that need to be saved should be added here!
  readsPrec _ s = fromMaybe [] $ asumE
    [ do
        (Just ($(patShowTypeRepQ @PollData), a), s') <- listToMaybe $ reads @(Maybe (String, PollData)) s
        return $ pure (Saved $ AdditionalDataSaved a, s')
    , do
        (Just ($(patShowTypeRepQ @(Map PollId PollData)), a), s') <- listToMaybe $ reads @(Maybe (String, Map PollId PollData)) s
        return $ pure (Saved $ AdditionalDataSaved a, s')
    ]

testSavedAdditional :: IO ()
testSavedAdditional = do
  let adt    = AdditionalDataSaved @PollData (PollData (PollId 1) Nothing "test" mempty mempty)
  let adt'   = AdditionalDataSaved @(Map PollId PollData) (fromList [(PollId 1, PollData (PollId 1) Nothing "test" mempty mempty)])
  let saved  = Saved adt :: Saved AdditionalData
  let saved' = Saved adt' :: Saved AdditionalData
  putStrLn "Testing Data:" >> print saved
  if read (show saved) == saved
    then putStrLn "Saved AdditionalData PollData test passed"
    else error    "Saved AdditionalData PollData test FAILED"
  putStrLn "Testing Data:" >> print saved'
  if read (show saved') == saved'
    then putStrLn "Saved AdditionalData Map PollId PollData test passed"
    else error    "Saved AdditionalData Map PollId PollData test FAILED"
