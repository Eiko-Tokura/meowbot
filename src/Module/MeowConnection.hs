module Module.MeowConnection where

import Control.System
import Module.RS.QQ
import Network.WebSockets (Connection)

[makeRModule|
MeowConnection
  meowConnection :: !Connection
|]

instance EventLoop c MeowConnection mods es
