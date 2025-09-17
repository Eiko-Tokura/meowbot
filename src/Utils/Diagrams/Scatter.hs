module Utils.Diagrams.Scatter where

import Utils.Diagrams.CJK (cjk)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Text.Printf
import Plots
 
-- | some basic patterns like circle, square, triangle, cross, star
-- without color
patterns :: [QDiagram Rasterific V2 Double Any]
patterns = [ circle 0.7
           , square 1.2
           , eqTriangle 1.4
           , eqTriangle 1.4 # rotateBy (1/2)
           , pentagon 0.9
           , hexagon 0.9
           , diamond 1.2
           ]

-- | some colors, to be used cyclely with patterns
-- numbers relatively prime with patterns
colors :: [Colour Double]
colors =
      [ red, green, blue, orange, purple
      , brown, pink, gray, cyan, magenta
      , yellow, black
      ]

cyclePatternColor :: [QDiagram Rasterific V2 Double Any]
cyclePatternColor = zipWith (#) (cycle patterns) (fc <$> cycle colors)

-- | put points on to the diagram
diagramDifferentPoints :: [(String, P2 Double)] -> QDiagram Rasterific V2 Double Any
diagramDifferentPoints pts = 
  let n       = length pts
      marks   = take n cyclePatternColor # map (scale 0.01 . lw none)
      placed  = position (zip (map snd pts) marks)
      labels  = mconcat
        [ moveTo (p .+^ r2 (0,0.015))
            ( text s
            # cjk
            # fontSizeL 0.01
            # fcA (black `withOpacity` 0.7)
            # lcA (opaque white)
            )
        | (s,p) <- pts
        ]
  in placed <> labels

testDiagram :: QDiagram Rasterific V2 Double Any
testDiagram = diagramDifferentPoints
  [ (name, p2 (x, y))
  | i <- [1 .. 31 :: Int]
  , let name = printf "点%02d" i
        x    = sin $ fromIntegral i / 5
        y    = cos $ fromIntegral i / 5
  ] # centerXY # pad 1.1

scatter :: [(String, (Double, Double))] -> QDiagram Rasterific V2 Double Any
scatter pts = diagramDifferentPoints
  [ (name, p2 (x, y))
  | (name, (x, y)) <- pts
  ] # centerXY # pad 1.1
