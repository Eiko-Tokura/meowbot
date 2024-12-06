module Utils.ByteString 
  ( bsToString
  ) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL

bsToString :: BL.ByteString -> String
bsToString = TL.unpack . TLE.decodeUtf8
{-# INLINE bsToString #-}

