module Main where

import Data.Additional.Saved
import Command.Poll

import Test.Tasty
import Test.Tasty.HUnit
import Test.ChatAPI
import Test.SearchScrape
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Module.Logging
import Module.Logging.Logger

main :: IO ()
main = do
  man <- newManager tlsManagerSettings
  base <- createStdoutBaseLogger
  defaultMain (tests (simpleLogger True (baseLogFunc base)) man)

tests :: Logger IO LogData -> Manager -> TestTree
tests logger man = testGroup "Tests"
  [ testGroup "Legacy Tests"
    [ testCase "Saved Additional" testSavedAdditional
    , testCase "Poll Parser" pollParserTest
    ]
  , testTools
  , testSearchScrape
  , testChatAPI logger man
  ]
  -- testSavedAdditional
  -- pollParserTest
