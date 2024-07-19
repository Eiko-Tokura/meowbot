module Control.Monad.IOe 
  (
    ioMToIOe, mToE, mIf
  ) where

import Control.Monad.Trans.Except

type IOe e a = ExceptT e IO a

ioMToIOe :: IO (Maybe a) -> IOe String a
ioMToIOe = ExceptT . fmap (mToE "")

mToE :: e -> Maybe a -> Either e a
mToE _ (Just value) = Right value
mToE alternative Nothing = Left alternative

mIf :: Bool -> a -> Maybe a
mIf True x = Just x
mIf False _ = Nothing
