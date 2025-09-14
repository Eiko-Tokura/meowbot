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

-- | Some really clever magic owo I came up with
optimalMeowTakeTail :: Int -> [a] -> [a]
optimalMeowTakeTail n xs = let len = length xs in
  if len <= meowCap
  then xs
  else strictTakeTail n xs
  where meowCap = n + optimalCap n

-- | Tries to keep the average length of the list
optimalMeowTakeTailKeepAvg :: Int -> [a] -> [a]
optimalMeowTakeTailKeepAvg n = optimalMeowTakeTail (max 1 $ n - optimalCap n `div` 2)

-- | Can you guess what it means? XD
optimalCap :: Int -> Int
optimalCap n = max 0 $ round @Double $ (2*) $ sqrt (fromIntegral n' * 2 * (1 - alpha)) - 1
  where alpha = 0.125
        n' = n `div` 2
