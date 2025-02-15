{-# LANGUAGE OverloadedStrings #-}
module Scrape
  ( scrapeTextWithManager
  , scrapeTextWithManagerE
  , scrapeTextRemoveScriptsAndStylesE
  , simpleScraper
  , textScraper
  , removeScriptsAndStyles
  ) where

import Data.Char (isSpace)
import Data.Default
import Data.Maybe
import Data.Bifunctor
import Network.HTTP.Client
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Text.StringLike
import qualified Data.Text.Lazy as L
import Data.Text (Text, pack)
import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Control.Monad

textScraper :: Scraper L.Text L.Text
textScraper = L.concat . map clearSpace <$> texts textSelector
  where clearSpace t = if L.all isSpace t then collapseSpace t else t
        collapseSpace t = case L.unsnoc t of
          Just (_, e) -> L.singleton e
          _           -> " "

tagM :: StringLike str => Tag str -> State Int (Maybe (Tag str))
tagM t@(TagOpen str _) = do
  when (str `elem` ["script", "style"]) $ modify (+1)
  return0 t
tagM t@(TagClose str) = do
  when (str `elem` ["script", "style"]) $ modify (max 0 . subtract 1)
  if (str `elem` ["script", "style"]) then return Nothing else return0 t
tagM t = return0 t

return0 :: a -> State Int (Maybe a)
return0 a = get >>= \case
    0 -> return $ Just a
    _ -> return Nothing

removeScriptsAndStyles :: StringLike str => [Tag str] -> [Tag str]
removeScriptsAndStyles = catMaybes . (`evalState` 0) . mapM tagM

scrapeTextWithManager :: Manager -> URL -> IO L.Text
scrapeTextWithManager manager url = do
  scrapeURLWithConfig (def { manager = Just manager }) url textScraper >>= return . fromMaybe ""

scrapeTextWithManagerE :: Manager -> URL -> IO (Either Text L.Text)
scrapeTextWithManagerE manager url = do
  try @SomeException (scrapeTextWithManager manager url) >>= return . either (Left . pack . show) Right

scrapeTextRemoveScriptsAndStylesE :: Manager -> URL -> IO (Either Text L.Text)
scrapeTextRemoveScriptsAndStylesE man url = runExceptT $ do
  tags <- ExceptT $ fmap (first (pack . show)) $ try @SomeException $ do
    fetchTagsWithConfig (def { manager = Just man }) url
  let cleaned = removeScriptsAndStyles tags
  return $ fromMaybe "" $ scrape textScraper cleaned

simpleScraper :: URL -> IO (Maybe L.Text)
simpleScraper url = scrapeURL url textScraper

