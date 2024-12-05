{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
module Parser.Except where

import Control.Monad.Item
import Control.Applicative
import Parser.Definition
import Control.Monad.Trans.Except
import Control.Monad.State

-- | A class of monad that can throw an error
-- this will use a stack of monad transformers, we stack ExceptT inside ParserT
-- ~ StateT sb (ExceptT e m) a
-- ~ sb -> m (Either e (a, sb))
-- maybe we actually want something like
-- sb -> m (Either e a, sb) ~ ExceptT e (StateT sb m) a
type ParserExceptT sb b e m a = ExceptT e (ParserT sb b m) a
type ParserE sb b e a         = ParserExceptT sb b e [] a

-- A lesson is learned here about choosing the order of monad transformers
-- if you want something to behave like T1, then use T1 (T2 m) a
-- here I think since we want something behave more like a Parser, not an ExceptT, 
-- we should use ParserT (ExceptT e m) a instead of ExceptT e (ParserT sb b m) a
--
-- because the Alternative for ExceptT is different from the Alternative for ParserT and is not what we want
--
-- There are three ways to fix this:
--
-- 1. switch the order of ExceptT and ParserT
--
-- 2. use a newtype to wrap the ExceptT
--
-- 3. write an orphan instance for Alternative (ExceptT e (ParserT sb b m)) (quick and dirty for now)
instance {-# OVERLAPPING #-} (Monad m, Alternative m) => Alternative (ExceptT e (ParserT sb b m)) where
  empty = ExceptT $ Right <$> empty
  {-# INLINE empty #-}
  ExceptT a <|> ExceptT b = ExceptT $ a <|> b
  {-# INLINE (<|>) #-}

runParserExceptT :: ParserExceptT sb b e m a -> sb -> m (Either e a, sb)
runParserExceptT = runStateT . runParserT . runExceptT
{-# INLINE runParserExceptT #-}

packParserExceptT :: (sb -> m (Either e a, sb)) -> ParserExceptT sb b e m a
packParserExceptT = ExceptT . ParserT . StateT
{-# INLINE packParserExceptT #-}

readE :: (Monad m, Read a) => e -> ParserT sb b m String -> ParserExceptT sb b e m a
readE e p = do
  s <- lift p
  case reads s of
    [(a, "")] -> return a
    _         -> throwE e

lift0 :: (Monad m, MonadTrans t) => m a -> t m a
lift0 = lift
{-# INLINE lift0 #-}

lift1 :: (Monad m, MonadTrans t) => (a -> m b) -> a -> t m b
lift1 = (lift .)
{-# INLINE lift1 #-}

lift2 :: (Monad m, MonadTrans t) => (a -> b -> m c) -> a -> b -> t m c
lift2 = ((lift .) .)
{-# INLINE lift2 #-}

withE :: (Monad m, Alternative m) => e -> ParserT sb b m a -> ParserExceptT sb b e m a
withE e p = ExceptT $ return e +| p
{-# INLINE withE #-}

runParserE :: e -> ParserE sb b e a -> sb -> Either e a
runParserE e0 p = mergeEither e0 . evalStateT (runParserT $ runExceptT p)
{-# INLINE runParserE #-}

mergeEither :: e -> [Either e a] -> Either e a
mergeEither e0 [] = Left e0
mergeEither e0 (x:xs) = case x of
  Left e -> case mergeEither e0 xs of
    Left _ -> Left e
    Right a -> Right a
  Right a -> Right a
{-# INLINE mergeEither #-}
