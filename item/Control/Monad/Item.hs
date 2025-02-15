{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE FlexibleContexts, TypeOperators, DefaultSignatures, FunctionalDependencies, DerivingVia, StandaloneDeriving #-}
-- | Author : Eiko chan >w<
--
-- A module for items from a stream.
-- abstracting the concept of an item from a stream, most likely used in parsers.
-- But it does not mention anything about parser, nor does it mention characters or text
-- so the interface potentially can be used by **all parsers** (or even something not parsers).
module Control.Monad.Item where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe

-- | A class of monad that can provide items. (possibly from a stream)
class Monad m => MonadItem i m | m -> i where
  getItem :: m i

-- | The idea of MonadTry instead of optional is that, we expect try action to not modify the state in parsers, i.e. they do not eat.
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

-- | A class of monad that can determine whether a value is zero (failure) inside the monad.
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

-- | Parses the end of the input, only succeeds if getItem fails in a try
end :: forall i m. (MonadZero m, MonadTry m, MonadItem i m) => m ()
end = do
  hasItem <- tryBool (getItem @i)
  when hasItem zero
{-# INLINE end #-}

-- | Require the return value to satisfy the condition, otherwise return zero
require :: MonadZero m => (i -> Bool) -> m i -> m i
require cond = (>>= \i -> if cond i then return i else zero)
{-# INLINE require #-}

-- | Parse an item that satisfies the condition
satisfy :: (MonadZero m, MonadItem i m) => (i -> Bool) -> m i
satisfy cond = require cond getItem
{-# INLINE satisfy #-}

-- | Parse an item that belonds to the given list, uses `elemE`
itemIn :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m i
itemIn is = satisfy (`elemE` is)
{-# INLINE itemIn #-}

-- | an `elem` that expands to a union of `==` checks at compile time. Use it only on constant and literal lists.
elemE :: Eq a => a -> [a] -> Bool
elemE = elem
{-# INLINE[1] elemE #-}
{-# RULES "expand elemE" forall a x. elemE a [x] = a == x #-}
{-# RULES "expand elemE" forall a x xs. elemE a (x:xs) = a == x || elemE a xs #-}

-- | an `notElem` that expands to a union of `==` checks at compile time. Use it on constant and literal lists.
notElemE :: Eq a => a -> [a] -> Bool
notElemE = (not .) . elemE
{-# INLINE notElemE #-}

-- | one or more `itemIn`
itemsIn :: (Alternative m, MonadZero m, MonadItem i m, Eq i) => [i] -> m [i]
itemsIn = some . itemIn
{-# INLINE itemsIn #-}

-- | one or more items not in the list
itemNotIn :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m i
itemNotIn is = satisfy (`notElemE` is)
{-# INLINE itemNotIn #-}

-- | one or more `itemNotIn`
itemsNotIn :: (Alternative m, MonadZero m, MonadItem i m, Eq i) => [i] -> m [i]
itemsNotIn = some . itemNotIn
{-# INLINE itemsNotIn #-}

-- | parse any item that is not in the list
noneOf :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m i
noneOf is = satisfy (`notElemE` is)
{-# INLINE noneOf #-}

-- | parse exactly the item given
just :: (MonadZero m, MonadItem i m, Eq i) => i -> m i
just i = satisfy (== i)
{-# INLINE just #-}

-- | parse item not equal to the given item
itemNot :: (MonadZero m, MonadItem i m, Eq i) => i -> m i
itemNot i = satisfy (/= i)
{-# INLINE itemNot #-}

-- | parse exactly a list of given items
string :: (MonadZero m, MonadItem i m, Eq i) => [i] -> m [i]
string = mapM just
{-# INLINE string #-}

-- ## Applicative combinators
-- | Applicative version of `(:)`
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>
{-# INLINE (<:>) #-}

-- | Alternative options packed into Either,
(|+|) :: Alternative m => m a -> m b -> m (Either a b)
(|+|) p q = fmap Left p <|> fmap Right q
infixr 3 |+|
{-# INLINE (|+|) #-}

-- | A type synonym for Either, right associative
type (|+|) = Either

-- | type sum, but prefer the **right side** as default and return Right r
(+|) :: Alternative m => m e -> m a -> m (Either e a)
(+|) e a = fmap Right a <|> fmap Left e
infixr 3 +|
{-# INLINE (+|) #-}

-- | identical to `optional`
optMaybe :: Alternative m => m a -> m (Maybe a)
optMaybe p = fmap Just p <|> pure Nothing
{-# INLINE optMaybe #-}

-- | optionally parse, return True if successful
optBool :: Alternative m => m a -> m Bool
optBool = fmap isJust . optMaybe
{-# INLINE optBool #-}

-- | optionally parse, ignoring return value
opt_ :: Alternative m => m a -> m ()
opt_ = void . optMaybe
{-# INLINE opt_ #-}

-- ## MonadTry and MonadZero combinators
-- | parse one or more items until the till-parser succeeds
someTill :: (MonadTry m, MonadZero m) => m t -> m a -> m [a]
someTill pt p = do
  end <- tryBool pt
  if end then zero else p <:> manyTill pt p
{-# INLINE someTill #-}

-- | parse zero or more items until the till-parser succeeds
manyTill :: (MonadTry m) => m t -> m a -> m [a]
manyTill pt p = do
  end <- tryBool pt
  if end then return [] else p <:> manyTill pt p
{-# INLINE manyTill #-}

-- | parse one or more items separated by the sep-parser
intercalateBy :: (Alternative m) => m sep -> m a -> m [a]
intercalateBy sep p = p <:> many (sep *> p)
{-# INLINE intercalateBy #-}

-- | parse zero or more items separated by the sep-parser
intercalateBy0 :: (Alternative m) => m sep -> m a -> m [a]
intercalateBy0 sep p = intercalateBy sep p <|> pure []
{-# INLINE intercalateBy0 #-}

-- | parse zero or more items inside two brackets-parsers, with the default item parser
insideBrackets :: (MonadZero m, Alternative m, MonadTry m, MonadItem i m, Eq i) => (i, i) -> m [i]
insideBrackets (l, r) = just l *> manyTill (just r) getItem <* just r
{-# INLINE insideBrackets #-}

-- | parse zero or more items inside two brackets-parsers, with the given item parser
insideBracketsWith :: (MonadZero m, Alternative m, MonadTry m, MonadItem i m, Eq i) => (i, i) -> m i -> m [i]
insideBracketsWith (l, r) item = just l *> manyTill (just r) item <* just r
{-# INLINE insideBracketsWith #-}

-- | A type constraint on m that looks like a parser, with reasonable constraints
-- reasonably assumes MonadZero, Alternative, MonadTry, MonadItem, Eq
type MonadIZT i m = (MonadZero m, Alternative m, MonadTry m, MonadItem i m, Eq i)
-- | A type constraint on m that looks like a parser, with reasonable constraints
-- reasonably assumes MonadZero, Alternative, MonadItem, Eq
type MonadIZ i m = (MonadZero m, Alternative m, MonadItem i m, Eq i)
--------------------------------------------------------------------------------

{-
htmlCodes :: (Chars sb) => Parser sb Char Char
htmlCodes = asumE
  [ $(stringQ_ "&amp;") >> pure '&'
  , $(stringQ_ "&#44;") >> pure ','
  , $(stringQ_ "&#91;") >> pure '['
  , $(stringQ_ "&#93;") >> pure ']'
  ]

Let's think about what will this thing expand to:

htmlCodes
  = asumE
    [ just '&' >> just 'a' >> just 'm' >> just 'p' >> just ';' >> pure '&'
    , just '&' >> just '#' >> just '4' >> just '4' >> just ';' >> pure ','
    , just '&' >> just '#' >> just '9' >> just '1' >> just ';' >> pure '['
    , just '&' >> just '#' >> just '9' >> just '3' >> just ';' >> pure ']'
    ]
  =     (just '&' >> just 'a' >> just 'm' >> just 'p' >> just ';' >> pure '&')
    <|> (just '&' >> just '#' >> just '4' >> just '4' >> just ';' >> pure ',')
    <|> (just '&' >> just '#' >> just '9' >> just '1' >> just ';' >> pure '[')
    <|> (just '&' >> just '#' >> just '9' >> just '3' >> just ';' >> pure ']')
-}

-- -- | A protected version of `>>` that is used to optimize with <|>
-- (%>>) :: Monad m => m a -> m b -> m b
-- (%>>) = (>>)
-- infixr 1 %>>
-- {-# INLINE[1] (%>>) #-}

-- eta :: Applicative f => a -> f a
-- eta = pure
-- {-# INLINE[1] eta #-}
--
-- {-# RULES "Factor common path/%>>" forall x y z. (x %>> y) <|> (x %>> z) = x %>> (y <|> z) #-}
-- {-# RULES "Factor common path/%>>" forall x y z. (just x %>> y) <|> (just x %>> z) = just x %>> (y <|> z) #-}
-- {-# RULES "Factor common path/%>>" forall x y z. (y %>> just x) <|> (z %>> just x) = (y <|> z) %>> just x #-}
--
-- {-# RULES "Factor common path/<:>" forall f x y z. (f x <:> y) <|> (f x <:> z) = f x <:> (y <|> z) #-}
-- {-# RULES "Factor common path/<:>" forall x y z. (just x <:> y) <|> (just x <:> z) = just x <:> (y <|> z) #-}
-- {-# RULES "Factor common path/<:>" forall x y z. (eta x <:> y) <|> (eta x <:> z) = eta x <:> (y <|> z) #-}
-- {-# RULES "Factor common path/<:>" forall x y z. (y <:> just x) <|> (z <:> just x) = (y <|> z) <:> just x #-}
-- {-# RULES "Factor common path/<:>" forall x y z. (y <:> eta x) <|> (z <:> eta x) = (y <|> z) <:> eta x #-}
--
-- ruleTest :: (Alternative m, MonadZero m, MonadItem Char m) => m Char
-- ruleTest = (just 'a' %>> just 'b' %>> just 'c') <|> (just 'a' %>> just 'c' %>> just 'c')
--
