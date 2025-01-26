{-# LANGUAGE OverloadedStrings #-}
module External.ChatAPI.Tool.Search where

import External.ChatAPI.Tool
-- Add to dependencies: http-conduit, aeson
import Network.HTTP.Client
import Servant.Client
import Servant.API
import Data.Aeson.Key
import Control.Monad.ExceptionReturn
import Control.Exception

-- green_check_circle
-- Your new search engine has been created
-- Copy the following code and paste it into your site's <body> section, where you want the search box and the search results to render.
-- 
-- <script async src="https://cse.google.com/cse.js?cx=90796ddb933b7428d">
-- </script>
-- <div class="gcse-search"></div>
-- GET https://customsearch.googleapis.com/customsearch/v1


