module Control.Monad.Effect where

import Data.HList
import Control.Monad.Trans.ReaderState

newtype Eff rs ss es m a = Eff { runEff :: HList rs -> HList ss -> m (Either (UList es) a, HList ss) }

-- the effect should include the following
-- * read
-- * state
-- * throw error without losing state
-- * logging (replacing LoggingT IO)

-- class Effect eff where
--   type Reads eff  :: Type
--   type States eff :: Type
