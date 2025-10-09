{-# LANGUAGE UndecidableInstances #-}
module External.ChatAPI.Tool.Search where

import External.ChatAPI.Tool
import Search
import Utils.Text as T
import Control.Monad.Effect

readGoogleApiKey :: IO (Maybe GoogleSearchApi)
readGoogleApiKey = do
  apiKey <- textLines <$> readFileText "googleApiKey"
  case apiKey of
    (api:cx:_) -> return $ Just $ GoogleSearchApi api cx
    _          -> return Nothing

data SearchTool

instance MonadIO m => ToolClass m SearchTool where
  type ToolInput  SearchTool = ParamToData (ObjectP0 '[StringP "query" "the search query"])
  type ToolOutput SearchTool = ParamToData (ObjectP0 '[StringP "results" "the search results"])
  data ToolError  SearchTool = SearchError Text deriving Show
  toolName _ _ = "search"
  toolDescription _ _ = "Search the web using Google Search API"
  toolHandler _ _ ((StringT query) :%* ObjT0Nil) = do
    api  <- effMaybeInWith (SearchError "no google api key found") $ liftIO readGoogleApiKey
    res  <- liftIO $ search api 7 query
    case res of
      Left err -> effThrow $ SearchError $ "search failed: " <> toText err
      Right r  -> return $ StringT r :%* ObjT0Nil
