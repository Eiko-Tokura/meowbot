module Utils.Diagrams.CJK where

import Diagrams.Prelude

cjk :: HasStyle a => a -> a
cjk = font "Noto Sans CJK SC"
