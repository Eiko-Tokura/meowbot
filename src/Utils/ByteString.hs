module Utils.ByteString
  ( bsToString
  , bsToLazyText
  , bsToText
  , LazyByteString
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL

type LazyByteString = BL.ByteString

bsToString :: BL.ByteString -> String
bsToString = TL.unpack . TLE.decodeUtf8
{-# INLINE bsToString #-}

bsToLazyText :: BL.ByteString -> TL.Text
bsToLazyText = TLE.decodeUtf8
{-# INLINE bsToLazyText #-}

bsToText :: BL.ByteString -> T.Text
bsToText = TL.toStrict . bsToLazyText
{-# INLINE bsToText #-}


