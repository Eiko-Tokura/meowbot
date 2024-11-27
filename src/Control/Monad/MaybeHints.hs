module Control.Monad.MaybeHints where

-- | this allows error messages to be attached to the Nothing case
type Maybe' h a = Either (Either () h) a

silently :: Maybe a -> Maybe' h a
silently Nothing = Left $ Left ()
silently (Just a) = Right a
{-# INLINE silently #-}

forgetH :: Maybe' h a -> Maybe a
forgetH (Left _) = Nothing
forgetH (Right a) = Just a
{-# INLINE forgetH #-}

inCase :: h -> Maybe a -> Maybe' h a
inCase h Nothing = Left $ Right h
inCase _ (Just a) = Right a
{-# INLINE inCase #-}

exceptToHint :: (e -> h) -> Either e a -> Maybe' h a
exceptToHint f (Left e) = Left $ Right $ f e
exceptToHint _ (Right a) = Right a
{-# INLINE exceptToHint #-}

