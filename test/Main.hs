module Main where

import Data.Additional.Saved
import Command.Poll

import Test.Tasty
import Test.Tasty.HUnit
import Test.ChatAPI
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main = do
  man <- newManager tlsManagerSettings
  defaultMain (tests man)

tests :: Manager -> TestTree
tests man = testGroup "Tests"
  [ testGroup "Legacy Tests"
    [ testCase "Saved Additional" testSavedAdditional
    , testCase "Poll Parser" pollParserTest
    ]
  , testTools
  , testChatAPI man
  ]
  -- testSavedAdditional
  -- pollParserTest
