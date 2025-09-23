{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Utils.Text
  ( T.Text, LazyText
  , lazyPack, lazyUnpack, tlshow
  , TextUtils(..)
  , putTextLn, putLazyTextLn
  , readFileText, readFileLazyText
  , ToText(..)
  ) where

import Data.Text (Text)
import Data.String(IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Text.Pretty.Simple as PP

type LazyText = TL.Text

class TextUtils t where
  type CorrespondingByteString t
  ------------ conversion functions ----------------
  pack :: String -> t

  unpack :: t -> String

  textToByteString :: t -> CorrespondingByteString t

  textToLazyByteString :: t -> BL.ByteString

  textUnlines :: [t] -> t

  textLines :: t -> [t]
  ----------------- IO functions -------------------
  restrictLength :: Int -> t -> t

  tshow :: Show a => a -> t
  tshow = pack . show
  {-# INLINE tshow #-}

putTextLn :: Text -> IO ()
putTextLn = TIO.putStrLn

putLazyTextLn :: LazyText -> IO ()
putLazyTextLn = TLIO.putStrLn

readFileText :: FilePath -> IO Text
readFileText = TIO.readFile

readFileLazyText :: FilePath -> IO LazyText
readFileLazyText = TLIO.readFile

instance TextUtils T.Text where
  type CorrespondingByteString T.Text = B.ByteString
  pack                 = T.pack
  unpack               = T.unpack
  textToByteString     = TE.encodeUtf8
  textToLazyByteString = TLE.encodeUtf8 . TL.fromStrict
  textUnlines          = T.unlines
  textLines            = T.lines
  restrictLength n     = T.take n
  {-# INLINE pack #-}
  {-# INLINE unpack #-}
  {-# INLINE textToByteString #-}
  {-# INLINE textToLazyByteString #-}
  {-# INLINE restrictLength #-}

instance TextUtils LazyText where
  type CorrespondingByteString LazyText = BL.ByteString
  pack                 = TL.pack
  unpack               = TL.unpack
  textToByteString     = TLE.encodeUtf8
  textToLazyByteString = TLE.encodeUtf8
  textUnlines          = TL.unlines
  textLines            = TL.lines
  restrictLength n     = TL.take (fromIntegral n)
  {-# INLINE pack #-}
  {-# INLINE unpack #-}
  {-# INLINE textToByteString #-}
  {-# INLINE textToLazyByteString #-}
  {-# INLINE restrictLength #-}

lazyPack :: String -> LazyText
lazyPack = TL.pack
{-# INLINE lazyPack #-}

lazyUnpack :: LazyText -> String
lazyUnpack = TL.unpack
{-# INLINE lazyUnpack #-}

tlshow :: Show a => a -> LazyText
tlshow = TL.pack . show
{-# INLINE tlshow #-}

-- | A class for converting values to 'Text'.
-- it avoids repetedly using 'T.pack . show' in the code, and automatically avoids using show to a Text value
class (Semigroup t, IsString t) => ToText a t where
  toText :: a -> t

instance ToText Text Text where
  toText = id
  {-# INLINE toText #-}

instance ToText LazyText LazyText where
  toText = id
  {-# INLINE toText #-}

instance ToText Char Text where
  toText = T.singleton
  {-# INLINE toText #-}

instance ToText Char LazyText where
  toText = TL.singleton
  {-# INLINE toText #-}

instance ToText String Text where
  toText = T.pack
  {-# INLINE toText #-}

instance ToText String LazyText where
  toText = TL.pack
  {-# INLINE toText #-}

instance ToText a Text => ToText (Maybe a) Text where
  toText Nothing  = "Nothing"
  toText (Just a) = "Just (" <> toText a <> ")"
  {-# INLINE toText #-}

instance ToText a LazyText => ToText (Maybe a) LazyText where
  toText Nothing  = "Nothing"
  toText (Just a) = "Just (" <> toText a <> ")"
  {-# INLINE toText #-}

instance {-# OVERLAPPABLE #-} Show a => ToText a Text where
  toText = TL.toStrict . PP.pShow
  {-# INLINE toText #-}

instance {-# OVERLAPPABLE #-} Show a => ToText a LazyText where
  toText = PP.pShow
  {-# INLINE toText #-}
