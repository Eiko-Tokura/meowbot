module Utils.List where

import Control.Parallel.Strategies

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Strict take n elements from a list, whenever it gets evaluated
-- will evaluate the entire list, dropping unused elements.
-- This is helpful for avoiding lazy stateful thunk leak when the rest of the list is not needed.
strictTake :: Int -> [a] -> [a]
strictTake n = (`using` evalList rseq) . take n
{-# INLINE strictTake #-}

-- | Strict take n elements from the tail of a list, whenever it gets evaluated
strictTakeTail :: Int -> [a] -> [a]
strictTakeTail n = (`using` evalList rseq) . reverse . take n . reverse
{-# INLINE strictTakeTail #-}

