module Utils.ListComp
  ( groupWith, sortWith, head'
  ) where

import GHC.Exts

-- | This function should only be used when you are using `group by`
head' []    = error "head': impossible happened, a group should not be empty"
head' (x:_) = x
{-# INLINE head' #-}
