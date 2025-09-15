-- | Making rendering diagrams to ByteString easy
module Utils.Diagrams.Render
  ( module Utils.Diagrams.Render
  , dims, V2(..)
  ) where

import Codec.Picture
import Diagrams.Backend.Rasterific
import Diagrams.Prelude
import Plots
import qualified Data.ByteString.Lazy as BL

renderPngLbs :: Monoid m => SizeSpec V2 Double -> QDiagram Rasterific V2 Double m -> BL.ByteString
renderPngLbs ss dia = encodePng $ renderDia Rasterific (RasterificOptions ss) dia

axisToPngLbs :: SizeSpec V2 Double -> Axis Rasterific V2 Double -> BL.ByteString
axisToPngLbs ss = renderPngLbs ss . renderAxis
