module Parser.String where

import Control.Monad.Item
import Data.List (isPrefixOf)

nonFlagString :: (MonadZero m, MonadItem String m) => m String
nonFlagString = require (not . ("--" `isPrefixOf`)) getItem
