-- | Making rendering diagrams to ByteString easy
module Utils.Diagrams.Render
  ( module Utils.Diagrams.Render
  , dims, V2(..)
  , mkSizeSpec2D
  ) where

import Codec.Picture
import Diagrams.Backend.Rasterific
import Diagrams.Prelude
import Utils.Diagrams.CJK (cjk)
import Plots
import qualified Data.ByteString.Lazy as BL

renderPngLbs :: Monoid m => SizeSpec V2 Double -> QDiagram Rasterific V2 Double m -> BL.ByteString
renderPngLbs ss dia = encodePng $ renderDia Rasterific (RasterificOptions ss) dia

renderBmpLbs :: Monoid m => SizeSpec V2 Double -> QDiagram Rasterific V2 Double m -> BL.ByteString
renderBmpLbs ss dia = encodeBitmap $ renderDia Rasterific (RasterificOptions ss) dia

axisToPngLbs :: SizeSpec V2 Double -> Axis Rasterific V2 Double -> BL.ByteString
axisToPngLbs ss = renderPngLbs ss . cjk . withWhiteBackground . renderAxis

axisToBmpLbs :: SizeSpec V2 Double -> Axis Rasterific V2 Double -> BL.ByteString
axisToBmpLbs ss = renderBmpLbs ss . cjk . withWhiteBackground . renderAxis

defaultToPngLbs :: Monoid m => SizeSpec V2 Double -> QDiagram Rasterific V2 Double m -> BL.ByteString
defaultToPngLbs ss = renderPngLbs ss . withWhiteBackground . cjk

withWhiteBackground :: Monoid m => QDiagram Rasterific V2 Double m -> QDiagram Rasterific V2 Double m
withWhiteBackground = bgFrame 0.02 white
