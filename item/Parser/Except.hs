{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
-- | This module provides functionalities for error messages and error handling in parsers
-- use in conjunction with Parser.Run
module Parser.Except 
  ( ParserExceptT, ParserE
  , runParserExceptT
  , packParserExceptT
  , liftR, liftR1, liftR2
  , lift0, lift1, lift2
  , withE, readE, addE
  , runParserE
  , mergeEither
  , module Control.Monad.Trans.Except
  ) where

import Control.Applicative
import Parser.Definition
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Bifunctor

-- | A class of monad that can throw an error
-- this will use a stack of monad transformers, we stack ExceptT inside ParserT
-- ~ StateT sb (ExceptT e m) a
-- ~ sb -> m (Either e (a, sb))
type ParserExceptT sb b e m a = ParserT sb b (ExceptT e m) a
type ParserE sb b e a         = ParserExceptT sb b e [] a

-- instance {-# OVERLAPPING #-} (Monad m, Alternative m) => Alternative (ExceptT e (ParserT sb b m)) where
--   empty = ExceptT $ Right <$> empty
--   {-# INLINE empty #-}
--   ExceptT a <|> ExceptT b = ExceptT $ a <|> b
--   {-# INLINE (<|>) #-}

-- | Unwraps the ParserExceptT monad transformer stack into a function that takes a state and returns a monadic value
runParserExceptT :: ParserExceptT sb b e m a -> sb -> m (Either e (a, sb))
runParserExceptT = (runExceptT .) . runStateT . runParserT
{-# INLINE runParserExceptT #-}

-- | Wraps a function that takes a state and returns a monadic value into a ParserExceptT monad transformer stack
packParserExceptT :: (sb -> m (Either e (a, sb))) -> ParserExceptT sb b e m a
packParserExceptT = ParserT . StateT . (ExceptT .)
{-# INLINE packParserExceptT #-}

-- | Lifts a monad transformer inside the ParserT, R means "on the right"
liftR :: (Monad m, MonadTrans t') => ParserT sb b m a -> ParserT sb b (t' m) a
liftR = ParserT . StateT . (lift .) . runStateT . runParserT
{-# INLINE liftR #-}

liftR1 :: (Monad m, MonadTrans t') => (a1 -> ParserT sb b m a) -> a1 -> ParserT sb b (t' m) a
liftR1 = (liftR .)
{-# INLINE liftR1 #-}

liftR2 :: (Monad m, MonadTrans t') => (a1 -> a2 -> ParserT sb b m a) -> a1 -> a2 -> ParserT sb b (t' m) a
liftR2 = ((liftR .) .)
{-# INLINE liftR2 #-}

-- | reads a string value and throws an error if it fails
readE :: (Monad m, Read a) => e -> ParserT sb b m String -> ParserExceptT sb b e m a
readE e p = do
  s <- liftR p
  case reads s of
    [(a, "")] -> return a
    _         -> lift (throwE e)
{-# INLINE readE #-}

-- | identical to `lift`
lift0 :: (Monad m, MonadTrans t) => m a -> t m a
lift0 = lift
{-# INLINE lift0 #-}

lift1 :: (Monad m, MonadTrans t) => (a -> m b) -> a -> t m b
lift1 = (lift .)
{-# INLINE lift1 #-}

lift2 :: (Monad m, MonadTrans t) => (a -> b -> m c) -> a -> b -> t m c
lift2 = ((lift .) .)
{-# INLINE lift2 #-}

-- | Modify an ordinary parser to a parser that can throw an error when it fails
withE :: (Monad m, Alternative m) => e -> ParserT sb b m a -> ParserExceptT sb b e m a
withE e p = liftR p <|> lift (throwE e)
{-# INLINE withE #-}

-- | Add an error message to a ParserExceptT
addE :: (Monad m, Alternative m) => e -> ParserExceptT sb b e m a -> ParserExceptT sb b e m a
addE e p = p <|> lift (throwE e)
{-# INLINE addE #-}

-- | Run a ParserE and return the result or the error message
runParserE :: e -> ParserE sb b e a -> sb -> Either e a
runParserE e0 p = ((mergeEither e0 . map (second fst) . runExceptT) .) . runStateT $ runParserT p
{-# INLINE runParserE #-}

-- | Merge a list of Either into a single Either, if there is any Right value, return the first one, otherwise return the first Left value
mergeEither :: e -> [Either e a] -> Either e a
mergeEither e0 [] = Left e0
mergeEither e0 (x:xs) = case x of
  Left e -> case mergeEither e0 xs of
    Left _ -> Left e
    Right a -> Right a
  Right a -> Right a
{-# INLINE mergeEither #-}
