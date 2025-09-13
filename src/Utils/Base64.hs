-- | Utilities for encoding images to base64.
module Utils.Base64 where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base64.Lazy (encode, decode)
import Control.DeepSeq
import GHC.Generics
import Control.Exception

newtype Base64 = Base64 { runBase64 :: ByteString }
  deriving newtype (Eq, Ord, Show, Read, Generic, NFData)

rawByteStringToBase64 :: ByteString -> Base64
rawByteStringToBase64 = Base64 . encode
{-# INLINE rawByteStringToBase64 #-}

base64ToRawByteString :: Base64 -> Either String ByteString
base64ToRawByteString = decode . runBase64
{-# INLINE base64ToRawByteString #-}

readFileBase64 :: FilePath -> IO (Either String Base64)
readFileBase64 path = do
  content <- try $ BL.readFile path
  return $ case content of
    Left (e :: IOException) -> Left $ show e
    Right str -> Right $ rawByteStringToBase64 str
{-# INLINABLE readFileBase64 #-}
