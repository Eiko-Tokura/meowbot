module Module.RawMessage where

import Control.Concurrent.STM (TVar)
import Control.Monad.Effect
import Data.ByteString.Lazy (ByteString)
import Module.RS.QQ

[makeRModule|
RawMessage
  rawMessage :: !(TVar (Maybe ByteString))
|]
