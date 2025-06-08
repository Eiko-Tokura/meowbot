module Data.BSeq where

import Data.Sequence (Seq)
import GHC.TypeLits
import Data.Sequence qualified as Seq
import Data.Proxy

newtype BSeq (n :: Nat) a = BSeq
  { toSequence :: Seq a
  }
  deriving (Show, Eq)
  deriving newtype (Functor, Foldable)

-- | Add an element to the end of the bounded sequence.
-- if the sequence is full, the first element is dropped.
bSeqSnoc :: forall n a. (KnownNat n) => BSeq n a -> a -> BSeq n a
bSeqSnoc (BSeq xs) x
  | length xs < n = BSeq (xs Seq.|> x)
  | otherwise     = BSeq (Seq.drop 1 xs Seq.|> x)
  where n = fromIntegral (natVal (Proxy :: Proxy n))
{-# INLINE bSeqSnoc #-}

-- | Add an element to the front of the bounded sequence.
-- if the sequence is full, the last element is dropped.
bSeqCons :: forall n a. (KnownNat n) => a -> BSeq n a -> BSeq n a
bSeqCons x (BSeq xs)
  | length xs < n = BSeq (x Seq.<| xs)
  | otherwise     = BSeq (x Seq.<| Seq.deleteAt (n - 1) xs)
  where n = fromIntegral (natVal (Proxy :: Proxy n))
{-# INLINE bSeqCons #-}

-- | Take the first n elements from the bounded sequence.
take :: Int -> BSeq n a -> BSeq n a
take n = BSeq . Seq.take n . toSequence
{-# INLINE take #-}

-- | Drop the first n elements from the bounded sequence.
drop :: Int -> BSeq n a -> BSeq n a
drop n = BSeq . Seq.drop n . toSequence
{-# INLINE drop #-}
