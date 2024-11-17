{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE FlexibleContexts, DefaultSignatures, FunctionalDependencies, DerivingVia, StandaloneDeriving #-}
-- | A module for items from a stream.
-- abstracting the concept of an item from a stream, most likely used in parsers.
-- But it does not mention anything about parser, nor does it mention characters or text
-- so it potentially can be used by all parsers.
module Control.Monad.Item where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe

class Monad m => MonadItem i m | m -> i where
  getItem :: m i

class Monad m => MonadTry m where
  tryMaybe :: m a -> m (Maybe a)
  {-# MINIMAL tryMaybe #-}

  tryBool :: m a -> m Bool
  tryBool = fmap isJust . tryMaybe
  {-# INLINE tryBool #-}

-- | A class of monad that has a zero element. A weaker version of MonadPlus
class Monad m => MonadZero m where
  {-# MINIMAL zero #-}
  zero   :: m a
  default zero :: (MonadPlus m) => m a
  zero = mzero
  {-# INLINE zero #-}

newtype AdditiveStructure m a = AdditiveStructure (m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus) via m

instance MonadPlus m => MonadZero (AdditiveStructure m) where
  zero = AdditiveStructure mzero
  {-# INLINE zero #-}

instance MonadZero m => MonadZero (StateT s m) where
  zero = lift zero
  {-# INLINE zero #-}

-- deriving via (Add (StateT s m)) instance MonadPlus m => MonadZero (StateT s m)
deriving via (AdditiveStructure Maybe) instance MonadZero Maybe
deriving via (AdditiveStructure []) instance MonadZero []

-- | A class of monad that can determine whether a value is zero
class MonadZero m => MonadIsZero m where
  isZero :: m a -> m Bool
  {-# MINIMAL isZero #-}

  nonZero :: m a -> m Bool
  nonZero = fmap not . isZero
  {-# INLINE nonZero #-}

instance MonadIsZero Maybe where
  isZero = pure . isNothing
  {-# INLINE isZero #-}

instance MonadIsZero [] where
  isZero = pure . null
  {-# INLINE isZero #-}

instance MonadIsZero m => MonadTry (StateT s m) where
  -- | Just try a parser, never eat regardless of whether successful
  -- tryMaybe :: (MonadZero m, MonadPlus m) => ParserT sb m a -> ParserT sb m (Maybe a)
  tryMaybe p = do
    s0 <- get
    let res = evalStateT p s0
    success <- lift $ nonZero res
    if success then Just <$> lift res else return Nothing
  {-# INLINE tryMaybe #-}

require :: MonadZero m => (i -> Bool) -> m i -> m i
require cond = (>>= \i -> if cond i then return i else zero)
{-# INLINE require #-}

satisfy :: (MonadZero m, MonadItem i m) => (i -> Bool) -> m i
satisfy cond = require cond getItem
{-# INLINE satisfy #-}

itemIn :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m i
itemIn is = satisfy (`elem` is)
{-# INLINE itemIn #-}

itemsIn :: (Alternative m, MonadZero m, MonadItem i m, Eq i) => [i] -> m [i]
itemsIn = some . itemIn
{-# INLINE itemsIn #-}

itemNotIn :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m i
itemNotIn is = satisfy (`notElem` is)
{-# INLINE itemNotIn #-}

itemsNotIn :: (Alternative m, MonadZero m, MonadItem i m, Eq i) => [i] -> m [i]
itemsNotIn = some . itemNotIn
{-# INLINE itemsNotIn #-}

noneOf :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m i
noneOf is = satisfy (`notElem` is)
{-# INLINE noneOf #-}

just :: (MonadZero m, MonadItem i m, Eq i) => i -> m i
just i = satisfy (== i)
{-# INLINE just #-}

itemNot :: (MonadZero m, MonadItem i m, Eq i) => i -> m i
itemNot i = satisfy (/= i)
{-# INLINE itemNot #-}

string :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m [i]
string = mapM just
{-# INLINE string #-}

-- Applicative combinators
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>
{-# INLINE (<:>) #-}

(|+|) :: Alternative m => m a -> m b -> m (Either a b)
(|+|) p q = fmap Left p <|> fmap Right q
infixr 3 |+|
{-# INLINE (|+|) #-}

-- | Identical to `optional`
optMaybe :: Alternative m => m a -> m (Maybe a)
optMaybe p = fmap Just p <|> pure Nothing
{-# INLINE optMaybe #-}

optBool :: Alternative m => m a -> m Bool
optBool = fmap isJust . optional
{-# INLINE optBool #-}

opt_ :: Alternative m => m a -> m ()
opt_ = void . optMaybe
{-# INLINE opt_ #-}

-- MonadTry and MonadZero combinators
someTill :: (MonadTry m, MonadZero m) => m t -> m a -> m [a]
someTill pt p = do
  end <- tryBool pt
  if end then zero else p <:> manyTill pt p
{-# INLINE someTill #-}

manyTill :: (MonadTry m) => m t -> m a -> m [a]
manyTill pt p = do
  end <- tryBool pt
  if end then return [] else p <:> manyTill pt p
{-# INLINE manyTill #-}

digit :: (MonadZero m, MonadItem Char m) => m Char
digit = itemIn ['0'..'9']
{-# INLINE digit #-}

digits :: (MonadZero m, Alternative m, MonadItem Char m) => m String
digits = some digit
{-# INLINE digits #-}

space :: (MonadZero m, MonadItem Char m) => m Char
space = itemIn [' ', '\t']
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

spaceOrEnter :: (MonadZero m, MonadItem Char m) => m Char
spaceOrEnter = itemIn [' ', '\t', '\n', '\r']
{-# INLINE spaceOrEnter #-}

positiveFloat :: forall a m. (MonadZero m, Alternative m, MonadTry m, MonadItem Char m, Floating a, Ord a, Read a) => m a
positiveFloat = require (>0) float

intercalateBy :: (Alternative m) => m sep -> m a -> m [a]
intercalateBy sep p = p <:> many (sep *> p)
{-# INLINE intercalateBy #-}

intercalateBy0 :: (Alternative m) => m sep -> m a -> m [a]
intercalateBy0 sep p = intercalateBy sep p <|> pure []
{-# INLINE intercalateBy0 #-}

insideBrackets :: (MonadZero m, Alternative m, MonadItem i m, Eq i) => (i, i) -> m [i]
insideBrackets (l, r) = just l *> some getItem <* just r
{-# INLINE insideBrackets #-}
