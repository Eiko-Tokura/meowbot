{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE TypeOperators, TypeFamilies, ConstraintKinds #-}
module Parser.Text where

import Parser.Definition
import Control.Monad.Item
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse a non-empty word, which is a continuous string of characters that are not spaces, or a string inside a pair of quotes.
word :: Parser Char String
word =   insideBrackets ('\'', '\'') 
     <|> insideBrackets ('"', '"') 
     <|> some (itemNot ' ')

word' :: Parser Char Text
word' = T.pack <$> word

-- | Parse a word that is not a flag, i.e. a word that does not start with a dash.
nonFlagWord :: Parser Char String
nonFlagWord = insideBrackets ('\'', '\'')
      <|> insideBrackets ('"', '"') 
      <|> some (itemNotIn [' ', '-'])

nonFlagWord' :: Parser Char Text
nonFlagWord' = T.pack <$> nonFlagWord

class (Stream sb b) => Packable sb b where
  pack :: [b] -> sb 

instance Packable String Char where
  pack = id
  {-# INLINE pack #-}

instance Packable Text Char where
  pack = T.pack
  {-# INLINE pack #-}

string' :: (Packable sb i, MonadZero m, MonadItem i m, Eq i) => sb -> m sb
string' s = string (head $ flatten s) >> return s
{-# INLINE string' #-}

some' :: (Packable sb i, Alternative m, MonadItem i m) => m i -> m sb
some' p = pack <$> some p 
{-# INLINE some' #-}

many' :: (Packable sb i, Alternative m, MonadItem i m) => m i -> m sb
many' p = pack <$> many p
{-# INLINE many' #-}

someTill' :: (Packable sb i, MonadZero m, MonadTry m, MonadItem i m, Eq i) => m t -> m i -> m sb
someTill' p end = pack <$> someTill p end
{-# INLINE someTill' #-}

manyTill' :: (Packable sb i, MonadTry m, MonadItem i m) => m t -> m i -> m sb
manyTill' p end = pack <$> manyTill p end
{-# INLINE manyTill' #-}

digits' :: (Packable sb Char, MonadZero m, Alternative m, MonadItem Char m) => m sb
digits' = pack <$> digits
{-# INLINE digits' #-}