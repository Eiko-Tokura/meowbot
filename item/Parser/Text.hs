{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Parser.Text where

import Parser.Definition
import Parser.Char
import Control.Monad.Item
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

type Chars sb = IsStream sb Char
-- | Parse a non-empty word, which is a continuous string of characters that are not spaces, or a string inside a pair of quotes.
word :: Chars sb => Parser sb Char String
word =   insideBrackets ('\'', '\'')
     <|> insideBrackets ('"', '"')
     <|> some (itemNot ' ')

word' :: Chars sb => Parser sb Char Text
word' = T.pack <$> word

-- | Parse a word that is not a flag, i.e. a word that does not start with a dash.
nonFlagWord :: Chars sb => Parser sb Char String
nonFlagWord = insideBrackets ('\'', '\'')
      <|> insideBrackets ('"', '"')
      <|> some (itemNotIn [' ', '-'])

nonFlagWord' :: Chars sb => Parser sb Char Text
nonFlagWord' = T.pack <$> nonFlagWord

class (IsStream sb b) => Packable sb b where
  packable :: [b] -> sb

instance Packable String Char where
  packable = id
  {-# INLINE packable #-}

instance Packable Text Char where
  packable = T.pack
  {-# INLINE packable #-}

string' :: (Packable sb i, MonadZero m, MonadItem i m, Eq i) => sb -> m sb
string' s = string (head $ flatten s) >> return s
{-# INLINE string' #-}

some' :: (Packable sb i, Alternative m, MonadItem i m) => m i -> m sb
some' p = packable <$> some p
{-# INLINE some' #-}

many' :: (Packable sb i, Alternative m, MonadItem i m) => m i -> m sb
many' p = packable <$> many p
{-# INLINE many' #-}

someTill' :: (Packable sb i, MonadZero m, MonadTry m, MonadItem i m, Eq i) => m t -> m i -> m sb
someTill' p end = packable <$> someTill p end
{-# INLINE someTill' #-}

manyTill' :: (Packable sb i, MonadTry m, MonadItem i m) => m t -> m i -> m sb
manyTill' p end = packable <$> manyTill p end
{-# INLINE manyTill' #-}

digits' :: (Packable sb Char, MonadZero m, Alternative m, MonadItem Char m) => m sb
digits' = packable <$> digits
{-# INLINE digits' #-}
