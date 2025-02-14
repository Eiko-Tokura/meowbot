{-# LANGUAGE UndecidableInstances #-}
module External.ChatAPI.Tool.Scrape where

import External.ChatAPI.Tool
import External.ChatAPI.MeowToolEnv
import Scrape
import Utils.Text as T
import Control.Monad.Except
import Control.Monad.IO.Class
import Module.ConnectionManager
import Data.Bifunctor
import Data.HList
import Control.Monad.Reader
import qualified Data.Text.Lazy as TL

data ScrapeTool

instance ConnectionManagerModule `In` mods => ToolClass (MeowToolEnv r mods) ScrapeTool where
  type ToolInput  ScrapeTool = ParamToData (ObjectP0 '[StringP "url" "the url to scrape"])
  type ToolOutput ScrapeTool = ParamToData (ObjectP0 '[StringP "result" "the text content scraped"])
  data ToolError  ScrapeTool = ScrapeError Text deriving Show
  toolName _ _ = "scrape"
  toolDescription _ _ = "Scrape a url and extract text content"
  toolHandler _ _ ((StringT url) :%* ObjT0Nil) = do
    man <- asks (manager . getF @ConnectionManagerModule . fst . snd . fst)
    res <- ExceptT . liftIO $ first ScrapeError <$> scrapeTextWithManagerE man (unpack url)
    return $ StringT (TL.toStrict res) :%* ObjT0Nil
