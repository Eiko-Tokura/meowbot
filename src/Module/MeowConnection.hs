module Module.MeowConnection where

import Control.Monad.Effect
import Module.RS.QQ
import Network.WebSockets (Connection)

[makeRModule|
MeowConnection
  meowConnection :: !Connection
|]
