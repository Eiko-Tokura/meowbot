{-# LANGUAGE OverloadedStrings #-}
module Scrape
  ( scrapeTextWithManager
  , scrapeTextWithManagerE
  , simpleScraper
  , textScraper
  ) where

import Data.Char (isSpace)
import Data.Default
import Data.Maybe
import Network.HTTP.Client
import Text.HTML.Scalpel
import qualified Data.Text.Lazy as L
import Data.Text (Text, pack)
import Control.Exception

textScraper :: Scraper L.Text L.Text
textScraper = L.concat . map clearSpace <$> texts textSelector
  where clearSpace t = if L.all isSpace t then collapseSpace t else t
        collapseSpace t = case L.unsnoc t of
          Just (_, e) -> L.singleton e
          _           -> " "

scrapeTextWithManager :: Manager -> URL -> IO L.Text
scrapeTextWithManager manager url = do
  scrapeURLWithConfig (def { manager = Just manager }) url textScraper >>= return . fromMaybe ""

scrapeTextWithManagerE :: Manager -> URL -> IO (Either Text L.Text)
scrapeTextWithManagerE manager url = do
  try @SomeException (scrapeTextWithManager manager url) >>= return . either (Left . pack . show) Right

simpleScraper :: URL -> IO (Maybe L.Text)
simpleScraper url = scrapeURL url textScraper

