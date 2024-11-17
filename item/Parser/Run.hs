-- | Make running the parser easier by exporting the necessary functions.
module Parser.Run 
  ( module Control.Monad.Item
  , module Control.Applicative
  , module Parser.Text
  , module Parser.Template
  , runParser
  , Parser
  ) where

import Control.Monad.Item
import Control.Applicative
import Parser.Text
import Parser.Definition
import Parser.Template
