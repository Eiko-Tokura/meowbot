module Utils.Diagrams where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Plots

data SimpleNamedBarPlot = SimpleNamedBarPlot
  { snbpTitle :: String
  , snbpData  :: [(String, Double)]
  }

simpleNamedBarPlot :: SimpleNamedBarPlot -> Axis Rasterific V2 Double
simpleNamedBarPlot p = r2Axis &~ do
  yMin ?= 0
  xLabel .= p.snbpTitle
  axisStyle .= vividColours
  hide minorTicks
  namedBarPlot p.snbpData $ return ()
