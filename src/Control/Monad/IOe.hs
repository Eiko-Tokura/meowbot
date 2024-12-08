module Control.Monad.IOe
  (
    ioMToIOe, mToE, mIf, mT
  ) where

import Control.Monad.Trans.Except

type IOe e a = ExceptT e IO a

ioMToIOe :: IO (Maybe a) -> IOe String a
ioMToIOe = ExceptT . fmap (mToE "")
{-# INLINE ioMToIOe #-}

mToE :: e -> Maybe a -> Either e a
mToE _ (Just value) = Right value
mToE alternative Nothing = Left alternative
{-# INLINE mToE #-}

mIf :: Bool -> a -> Maybe a
mIf True x = Just x
mIf False _ = Nothing
{-# INLINE mIf #-}

mT :: (Applicative t) => Maybe (t a) -> t (Maybe a)
mT Nothing = pure Nothing
mT (Just ioa) = Just <$> ioa

