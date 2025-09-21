module Utils.Diagrams.Scatter where

import Utils.Diagrams.CJK (cjk)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Plots
 
-- | some basic patterns like circle, square, triangle, cross, star
-- without color
patterns :: [QDiagram Rasterific V2 Double Any]
patterns = [ circle 0.64
           , square 1.2
           , eqTriangle 1.44
           , eqTriangle 1.44 # rotateBy (1/2)
           , pentagon 0.9
           , hexagon 0.72
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

cyclePatternColor' :: [QDiagram Rasterific V2 Double Any]
cyclePatternColor' = zipWith (#) (cycle patterns) (fcA . (`withOpacity` 0.6) <$> cycle colors)

-- | put points on to the diagram
diagramDifferentPoints :: [(String, P2 Double)] -> QDiagram Rasterific V2 Double Any
diagramDifferentPoints pts = 
  let n       = length pts
      marks   = take n cyclePatternColor # map (scale 0.006 . lw none)
      placed  = position (zip (map snd pts) marks)
      labels  = mconcat
        [ moveTo (p .+^ r2 (0,0.008))
            ( text s
            # cjk
            # fontSizeL 0.005
            # fcA (black `withOpacity` 0.7)
            # lcA (opaque white)
            )
        | (s,p) <- pts
        ]
  in placed <> labels

diagramSizedDifferentPoints :: [(String, Double, P2 Double)] -> QDiagram Rasterific V2 Double Any
diagramSizedDifferentPoints ptsw =
  let n       = length ptsw
      marks   = take n cyclePatternColor' # zipWith (\w p -> scale (0.0066 * (w ^. _2)) $ lw none p) ptsw
      placed  = position (zip ((^. _3) <$> ptsw) marks)
      labels  = mconcat
        [ moveTo (p .+^ r2 (0, 0.009 * w))
            ( text s
            # cjk
            # fontSizeL (0.006 * max 0.3 w)
            # fcA (black `withOpacity` 0.6)
            # lcA (opaque white)
            )
        | (s, w, p) <- ptsw
        ]
  in placed <> labels

scatter :: [(String, (Double, Double))] -> QDiagram Rasterific V2 Double Any
scatter pts = diagramDifferentPoints
  [ (name, p2 (x, y))
  | (name, (x, y)) <- pts
  ] # centerXY # pad 1.1

scatterSized :: [(String, Double, (Double, Double))] -> QDiagram Rasterific V2 Double Any
scatterSized pts = diagramSizedDifferentPoints
  [ (name, size, p2 (x, y))
  | (name, size, (x, y)) <- pts
  ] # centerXY # pad 1.1
