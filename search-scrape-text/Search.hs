{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveAnyClass #-}
module Search where

import Control.Exception
import Data.Bifunctor
import Data.Text (Text, unpack, pack)
import GHC.Generics
import System.Process
import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

data GoogleSearchApi = GoogleSearchApi
  { googleApiKey :: Text
  , googleCseId  :: Text
  } deriving (Show)

-- data GoogleSearchResult = GoogleSearchResult
--   { resultTitle   :: Text
--   , resultLink    :: Text
--   , resultSnippet :: Text
--   } deriving (Show)

data SearchQuery = SearchQuery
  { searchQuery    :: Text
  , searchApiKey   :: Text
  , searchCseId    :: Text
  , searchNum      :: Int
  } deriving (Show, Generic, ToJSON)

-- | Using direct python call to google-search.py and receive from stdout
-- need to pass a json object as string, base64 encode it, and pass it to the python script as command line argument
search :: GoogleSearchApi -> Int -> Text -> IO (Either Text Text)
search api number query = do
  let searchQuery = SearchQuery query (googleApiKey api) (googleCseId api) number
      base64Query = TL.unpack $ TLE.decodeASCII $ B64.encode $ A.encode searchQuery
  estdout <- try $ readProcess "/usr/bin/python" ["google-search.py", base64Query] ""
  case estdout of
    Left (SomeException e) -> return $ Left $ pack $ show e
    Right stdout -> return $ Right $ pack stdout


-- | If Inline Python is available, use this function to search
-- currently not working because the package cannot embed string into python
-- search :: GoogleSearchApi -> Int -> Text -> IO (Either Text [GoogleSearchResult])
-- search api number query = do
--   fmap (first $ pack . show) . try @PyError $ withPython $ do
--     let api_key   = pure $ unpack $ googleApiKey api :: IO String
--         cse_id    = pure $ unpack $ googleCseId api :: IO String
--         query_str = pure $ unpack query :: IO String
--         --num       = pure number :: Py Int
--     runPy $ do
--       [pymain|
--       from googleapiclient.discovery import build
-- 
--       def google_search(query, api_key, cse_id, num_results=5):
--           """
--           Query Google Custom Search API and return a list of search results.
--           
--           Parameters:
--             query (str): The search query.
--             api_key (str): Your Google API key.
--             cse_id (str): Your Custom Search Engine ID.
--             num_results (int): Number of results to return.
--           
--           Returns:
--             list: A list of dictionaries containing search result data.
--           """
--           service = build("customsearch", "v1", developerKey=api_key)
--           res = service.cse().list(q=query, cx=cse_id, num=num_results).execute()
--           return res.get('items', [])
-- 
--       if __name__ == "__main__":
--         # Replace these with your actual API key and Custom Search Engine ID
--         API_KEY = str(api_key_hs)
--         CSE_ID = str(cse_id_hs)
-- 
--         print(f"API key: {API_KEY}")
--         
--         results = google_search(str(query_str_hs), API_KEY, CSE_ID)
--         
--         print(f"Search results for: {query}\n")
--         for item in results:
--             print("Title:", item.get("title"))
--             print("Link: ", item.get("link"))
--             print("Snippet:", item.get("snippet"))
--             print("-" * 40)
--       |]
--       titles   <- fmap (map pack) $ fromPy' =<< [pye| [item.get("title") for item in results] |]
--       links    <- fmap (map pack) $ fromPy' =<< [pye| [item.get("link") for item in results] |]
--       snippets <- fmap (map pack) $ fromPy' =<< [pye| [item.get("snippet") for item in results] |]
--       return $ zipWith3 GoogleSearchResult titles links snippets
