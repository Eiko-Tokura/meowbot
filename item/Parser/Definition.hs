{-# LANGUAGE DerivingVia, TypeFamilies, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
module Parser.Definition where

import Control.Monad.State
import Control.Monad.Trans.State.Lazy()
import Control.Monad.Item
import Control.Monad
import Control.Applicative
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T

class Stream sb b | sb -> b where
  uncons :: sb -> [(b, sb)] -- ^ ways to take out the first singleton, allowing brances
  {-# MINIMAL uncons #-}

  flatten :: sb -> [[b]]
  flatten x | null (uncons x) = [[]]
  flatten x = uncons x >>= \(b, sb') -> map (b:) (flatten sb')
  {-# INLINE flatten #-}

instance Stream Text Char where
  uncons = maybe [] pure . T.uncons
  {-# INLINE uncons #-}
  flatten = pure . T.unpack
  {-# INLINE flatten #-}

instance Stream [a] a where
  uncons [] = []
  uncons (x:xs) = pure (x, xs)
  {-# INLINE uncons #-}
  flatten = pure
  {-# INLINE flatten #-}

-- instance (MonadZero m) => MonadItem b (ParserT b m) where
--   {-# SPECIALIZE instance MonadItem Char (ParserT Char Maybe) #-}
--   getItem = ParserT $ do
--     sb <- get
--     case uncons sb of
--       [] -> zero
--       ((c, sb'):_) -> put sb' >> return c
--   {-# INLINE getItem #-}

-- | The Parser transformer type, here
--
-- * `sb` is the input type, for example `Text`
--
-- * `b` is the type of the singleton, for example `Char`
--
-- * `m` the monad, typically `[]` or `Maybe`. But it is possible to add other monad transformers into here. For example adding WriterT you can have logging or output capabilities. Adding ExceptT you can have error messages.
--
-- * `a` the return type
newtype ParserT sb b m a = ParserT { runParserT :: StateT sb m a }
  -- deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadState sb, MonadZero, MonadTry) via (StateT sb m)
  -- deriving (Semigroup, Monoid) via Ap (StateT sb m) a -- Ap m a is a newtype whose monoid structure is defined applicatively
  -- deriving MonadTrans via (StateT sb)
instance (Monad m) => Functor (ParserT sb b m) where
  fmap f (ParserT p) = ParserT $ fmap f p
  {-# INLINE fmap #-}

instance (Monad m) => Applicative (ParserT sb b m) where
  pure a = ParserT $ pure a
  {-# INLINE pure #-}
  ParserT f <*> ParserT a = ParserT $ f <*> a
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (ParserT sb b m) where
  ParserT p >>= f = ParserT $ do
    a <- p
    runParserT $ f a
  {-# INLINE (>>=) #-}

instance (MonadZero m) => MonadZero (ParserT sb b m) where
  zero = ParserT zero
  {-# INLINE zero #-}

instance (MonadPlus m) => MonadPlus (ParserT sb b m) where
  mzero = ParserT mzero
  mplus (ParserT a) (ParserT b) = ParserT $ mplus a b
  {-# INLINE mzero #-}
  {-# INLINE mplus #-}

-- | The definition used in GHC on (Functor m, MonadPlus m) => Alternative (StateT s m)
-- is requiring redundant constraints. It suffices to use the Alternative instance of m.
instance (Monad m, Alternative m) => Alternative (ParserT sb b m) where
  empty = ParserT $ StateT $ const empty
  (<|>) (ParserT a) (ParserT b) = ParserT $ StateT $ \s -> runStateT a s <|> runStateT b s
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

instance (MonadZero m, Stream sb b) => MonadItem b (ParserT sb b m) where
  getItem = ParserT $ do
    sb <- get
    case uncons sb of
      [] -> zero
      ((c, sb'):_) -> put sb' >> return c
  {-# INLINE getItem #-}

instance (MonadIsZero m) => MonadTry (ParserT sb b m) where
  tryMaybe ma = ParserT $ tryMaybe $ runParserT ma
  {-# INLINE tryMaybe #-}

type Parser sb b a = ParserT sb b [] a

runParser :: (Stream sb b) => Parser sb b a -> sb -> Maybe a
runParser p = listToMaybe . evalStateT (runParserT p)
{-# INLINE runParser #-}

runParserFull :: (Stream sb b) => Parser sb b a -> sb -> [a]
runParserFull p = evalStateT (runParserT p)
{-# INLINE runParserFull #-}

{-# SPECIALIZE runParser     :: Parser Text Char a -> Text -> Maybe a #-}
{-# SPECIALIZE runParser     :: Parser [Char] Char a -> [Char] -> Maybe a #-}
{-# SPECIALIZE runParserFull :: Parser Text Char a -> Text -> [a] #-}
{-# SPECIALIZE runParserFull :: Parser [Char] Char a -> [Char] -> [a] #-}
