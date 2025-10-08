{-# LANGUAGE TemplateHaskell #-}
-- | Author: Eiko chan >w<
--
-- This module provides common parsers related to character level.
module Parser.Char where

import Control.Monad.Item
import Control.Applicative
import Parser.Template

-- | Parse a positive floating point number
positiveFloat :: forall a m. (MonadZero m, Alternative m, MonadTry m, MonadItem Char m, Floating a, Ord a, Read a) => m a
positiveFloat = require (>0) float

-- | Parse a non-negative floating point number
nFloat :: forall a m. (MonadZero m, Alternative m, MonadTry m, MonadItem Char m, Floating a, Ord a, Read a) => m a
nFloat = require (>=0) float

-- | Parse a single digit
digit :: (MonadZero m, MonadItem Char m) => m Char
digit = $(itemInQ ['0'..'9'])
{-# INLINE digit #-}

-- | Parse a string of digits
digits :: (MonadZero m, Alternative m, MonadItem Char m) => m String
digits = some digit
{-# INLINE digits #-}

-- | Parse a space or a tab
space :: (MonadZero m, MonadItem Char m) => m Char
space = $(itemInQ [' ', '\t'])
{-# INLINE space #-}

-- | Parse one or more spaces or tabs
spaces :: (MonadZero m, Alternative m, MonadItem Char m) => m String
spaces = some space
{-# INLINE spaces #-}

-- | Parse zero or more spaces or tabs
spaces0 :: (MonadZero m, Alternative m, MonadItem Char m) => m String
spaces0 = many space
{-# INLINE spaces0 #-}

bool :: (MonadZero m, Alternative m, MonadItem Char m) => m Bool
bool = ((string "True" <|> string "true") >> return True) <|> ((string "False" <|> string "false") >> return False)
{-# INLINE bool #-}

-- | Non-negative integer
nint :: forall i m. (MonadZero m, Alternative m, MonadItem Char m, Integral i, Read i) => m i
nint = read <$> some digit
{-# INLINE nint #-}

-- | Integer, possibly negative
int :: forall i m. (MonadZero m, Alternative m, MonadItem Char m, Integral i, Read i) => m i
int = read <$> (digits <|> just '-' <:> digits)
{-# INLINE int #-}

-- | Integer in the range [a, b]
intRange :: forall i m. (MonadZero m, Alternative m, MonadItem Char m, Integral i, Read i) => i -> i -> m i
intRange a b = require (\i -> a <= i && i <= b) int
{-# INLINE intRange #-}

-- | Parse a floating point number
float :: forall a m. (MonadZero m, Alternative m, MonadTry m, MonadItem Char m, Floating a, Read a) => m a
float = do
  part1 <- (just '-' <:> digits) <|> digits
  mpart2 <- tryMaybe (just '.' <:> digits)
  case mpart2 of
    Nothing    -> return $ read part1
    Just part2 -> return $ read $ part1 ++ part2

-- | Parse a space, a tab, a newline, or a carriage return
spaceOrEnter :: (MonadZero m, MonadItem Char m, Alternative m) => m Char
spaceOrEnter = $(itemInQ [' ', '\t', '\n', '\r'])
{-# INLINE spaceOrEnter #-}
