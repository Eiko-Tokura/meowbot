module Main where

import Data.Additional.Saved
import Command.Poll

import Test.Tasty
import Test.Tasty.HUnit
import Test.ChatAPI

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Legacy Tests"
    [ testCase "Saved Additional" testSavedAdditional
    , testCase "Poll Parser" pollParserTest
    ]
  , testTools
  , testChatAPI
  ]
  -- testSavedAdditional
  -- pollParserTest
