{-# LANGUAGE UndecidableInstances #-}
module External.ChatAPI.Tool.Scrape where

import Control.Monad.Effect
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.HList hiding (In)
import External.ChatAPI.MeowToolEnv
import External.ChatAPI.Tool
import Module.ConnectionManager
import Scrape
import Utils.Text as T
import qualified Data.Text.Lazy as TL

data ScrapeTool

-- ConnectionManagerModule `In` mods => 
instance ConnectionManagerModule `In` mods
  => ToolClass (MeowToolEnv mods) ScrapeTool where
  type ToolInput  ScrapeTool = ParamToData (ObjectP0 '[StringP "url" "the url to scrape"])
  type ToolOutput ScrapeTool = ParamToData (ObjectP0 '[StringP "result" "the text content scraped"])
  data ToolError  ScrapeTool = ScrapeError Text deriving Show
  toolName _ _ = "scrape"
  toolDescription _ _ = "Scrape a url and extract text content"
  toolHandler _ _ ((StringT url) :%* ObjT0Nil) = do
    man <- lift $ asksModule manager
    --asks (manager . getF @ConnectionManagerModule . fst . snd . fst)
    res <- ExceptT . liftIO $ first ScrapeError <$> scrapeTextRemoveScriptsAndStylesE man (unpack url)
    return $ StringT (TL.toStrict res) :%* ObjT0Nil
