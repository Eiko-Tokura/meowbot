module Test.SearchScrape where

import Search
import Test.Tasty
import Test.Tasty.HUnit
import External.ChatAPI.Tool.Search

testSearchScrape :: TestTree
testSearchScrape = testGroup "Search"
  [ testCase "search" $ do
      Just api <- readGoogleApiKey
      res <- search api 5 "haskell"
      case res of
        Left err -> assertFailure $ "search failed: " ++ show err
        Right r  -> print r
  ]
