{-# LANGUAGE TemplateHaskell #-}
module Parser.Char where

import Control.Monad.Item
import Control.Applicative
import Parser.Template

positiveFloat :: forall a m. (MonadZero m, Alternative m, MonadTry m, MonadItem Char m, Floating a, Ord a, Read a) => m a
positiveFloat = require (>0) float

digit :: (MonadZero m, MonadItem Char m) => m Char
digit = $(itemInQ ['0'..'9'])
{-# INLINE digit #-}

digits :: (MonadZero m, Alternative m, MonadItem Char m) => m String
digits = some digit
{-# INLINE digits #-}

space :: (MonadZero m, MonadItem Char m) => m Char
space = $(itemInQ [' ', '\t'])
{-# INLINE space #-}

spaces :: (MonadZero m, Alternative m, MonadItem Char m) => m String
spaces = some space
{-# INLINE spaces #-}

spaces0 :: (MonadZero m, Alternative m, MonadItem Char m) => m String
spaces0 = many space
{-# INLINE spaces0 #-}

-- | Non-negative integer
nint :: forall i m. (MonadZero m, Alternative m, MonadItem Char m, Integral i, Read i) => m i
nint = read <$> some digit
{-# INLINE nint #-}

-- | Integer, possibly negative
int :: forall i m. (MonadZero m, Alternative m, MonadItem Char m, Integral i, Read i) => m i
int = read <$> (some digit <|> just '-' <:> some digit)
{-# INLINE int #-}

-- | Integer in the range [a, b]
intRange :: forall i m. (MonadZero m, Alternative m, MonadItem Char m, Integral i, Read i) => i -> i -> m i
intRange a b = require (\i -> a <= i && i <= b) int
{-# INLINE intRange #-}

float :: forall a m. (MonadZero m, Alternative m, MonadTry m, MonadItem Char m, Floating a, Read a) => m a
float = do
  part1 <- (just '-' <:> digits) <|> digits
  mpart2 <- tryMaybe (just '.' <:> digits)
  case mpart2 of
    Nothing    -> return $ read part1
    Just part2 -> return $ read $ part1 ++ part2

spaceOrEnter :: (MonadZero m, MonadItem Char m, Alternative m) => m Char
spaceOrEnter = $(itemInQ [' ', '\t', '\n', '\r'])
{-# INLINE spaceOrEnter #-}
