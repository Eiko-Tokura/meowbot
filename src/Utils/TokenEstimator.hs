{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- | This module provides a function that estimates the number of tokens in a given text.
module Utils.TokenEstimator
  ( estimateTokens
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

-- | The rule is:
-- a English character including space and punctuation ~ 0.25 token
-- any non-ASCII character ~ 1 token
estimateTokens :: Text -> Int
estimateTokens text = T.foldl' estimate 0 text `div` 4
  where
    estimate !acc c
      |    isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c `elem` (" .,;:!?" :: String)
          = acc + 1 -- Printable ASCII characters (including space and punctuation)
      | otherwise
          = acc + 4 -- Non-ASCII characters

testTokenEstimatorRule :: IO ()
testTokenEstimatorRule = do
  let testCases = [ "Hello, world!"            -- 3 tokens
                  , "你好，世界！"             -- 6 tokens
                  , "This is a test."          -- 3 tokens
                  , "12345"                    -- 1 tokens
                  , "Special chars: @#$%^&*()" -- 12 tokens
                  ]
  mapM_ (print . estimateTokens) testCases
