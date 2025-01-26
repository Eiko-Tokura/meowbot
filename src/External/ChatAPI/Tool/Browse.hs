{-# LANGUAGE DataKinds #-}
module External.ChatAPI.Tool.Browse where

import External.ChatAPI.Tool

-- | Web browsing tool parameters definition
data WebBrowseTool

-- | Browsing result structure
data BrowseResult = BrowseResult
  { content    :: Text       -- ^ Clean text content
  , status     :: Text       -- ^ HTTP status
  , links      :: [Text]     -- ^ Found hyperlinks
  , screenshot :: Maybe Text -- ^ Base64 thumbnail (optional)
  } deriving (Show, Generic, ToJSON)

instance ToolClass WebBrowseTool where
  type ToolInput  WebBrowseTool = ParamToData 
    (ObjectP0 '[ StringP "url" "Website URL to visit"
               , StringP "action" "Action: fetch|extract|links"
               ])
  
  type ToolOutput WebBrowseTool = ParamToData 
    (ObjectP0 '[ StringP "result" "Formatted results"
               , ObjectP "metadata" "Technical details"
                  '[ StringP "status" "HTTP status"
                   , IntP "length" "Content length"
                   , IntP "links" "Links found"
                   ]
               ])

  data ToolError  WebBrowseTool = InvalidURL Text
                                | Timeout
                                | SecurityBlock
                                | UnsupportedContent
                                | RateLimited
                                deriving (Show)

  toolName _ = "web_browse"
  toolDescription _ = "Browse websites and extract information (≧ω≦)"

  toolHandler _ (url :@* (action :@* (:%*))) = do
    -- Validate URL format
    unless (isValidUrl url) $ throwE $ InvalidURL url
    
    -- Rate limit checks
    checkRateLimit
    
    -- Perform browsing
    result <- case action of
      "fetch"   -> fetchFullPage url
      "extract" -> extractMainContent url
      "links"   -> extractLinks url
      _         -> throwE $ InvalidURL "Unknown action"
    
    return $ packageResults result

  jsonToInput _ = first prettifyError . parseEither parseJSON
    where
      prettifyError err = case T.splitOn "key " err of
        ["Error in $", missing] -> "Missing parameter: " <> T.drop 1 (T.init missing) <> " 😿"
        _ -> "Invalid format: " <> toText err

-- | Fetch webpage with safety checks
fetchFullPage :: Text -> ExceptT (ToolError WebBrowseTool) IO BrowseResult
fetchFullPage url = do
  liftIO $ putStrLn $ "🌐 Browsing: " ++ T.unpack url
  
  manager <- liftIO $ newManager tlsManagerSettings
  req <- parseRequest (T.unpack url)
  
  response <- tryHttp $ do
    let req' = req 
          { requestHeaders = [ ("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
                            , ("Accept", "text/html,application/xhtml+xml")
                            ]
          , checkResponse = \_ _ -> pure ()
          }
    httpLbs req' manager
  
  let contentType = lookup "Content-Type" $ responseHeaders response
  unless ("text/html" `T.isInfixOf` cs contentType) $ throwE UnsupportedContent
  
  let body = responseBody response
  (cleanText, links) <- liftIO $ extractContent body
  
  return BrowseResult
    { content = decodeUtf8With lenientDecode $ BL.toStrict body
    , status = tshow $ statusCode $ responseStatus response
    , links = links
    , screenshot = Nothing
    }

-- | Content extraction using scalpel
extractMainContent :: Text -> ExceptT (ToolError WebBrowseTool) IO BrowseResult
extractMainContent url = do
  result <- fetchFullPage url
  let mainContent = scrape (BL.fromStrict $ encodeUtf8 $ content result) $ 
         chroot "main" (text anySelector) <|> 
         chroot ("div" @: [hasClass "main-content"]) (text anySelector) <|>
         text "body"
  
  case mainContent of
    Just txt -> return result { content = txt }
    Nothing -> return result

-- | Link extraction
extractLinks :: Text -> ExceptT (ToolError WebBrowseTool) IO BrowseResult 
extractLinks url = do
  result <- fetchFullPage url
  let links = scrape (BL.fromStrict $ encodeUtf8 $ content result) $
         attrs "href" "a"
  
  return result { links = fromMaybe [] links }

-- | Safety checks
isValidUrl :: Text -> Bool
isValidUrl url = 
  let parsed = parseURI (T.unpack url)
  in case parsed of
       Just u -> uriScheme u `elem` ["http:","https:"]
       Nothing -> False

checkRateLimit :: ExceptT (ToolError WebBrowseTool) IO ()
checkRateLimit = do
  lastReq <- liftIO $ readIORef lastRequestTime
  current <- liftIO getCurrentTime
  let diff = diffUTCTime current lastReq
  when (diff < 5) $ throwE RateLimited
  liftIO $ writeIORef lastRequestTime current

-- | Sanity checks
sanityCheckWebBrowseTool :: IO ()
sanityCheckWebBrowseTool = do
  let testInput = "{\"url\":\"https://example.com\",\"action\":\"extract\"}"
  
  putStrLn "🧪 Testing URL Validation..."
  case jsonToInput (Proxy @WebBrowseTool) =<< eitherDecode testInput of
    Right (url :@* (action :@* (:%*))) -> do
      unless (url == "https://example.com") $ error "URL parsing failed"
      unless (action == "extract") $ error "Action parsing failed"
      putStrLn "✅ Input parsing OK"
    Left e -> error $ "Input parsing failed: " ++ T.unpack e
  
  putStrLn "🌐 Testing Page Fetch..."
  res <- runExceptT $ toolHandlerTextError (Proxy @WebBrowseTool) =<< eitherDecode testInput
  case res of
    Right (result :%* (:%*)) -> do
      putStrLn "✅ Received valid response:"
      TIO.putStrLn $ TL.toStrict $ TLE.decodeUtf8 $ encode result
    Left err -> error $ "Browse failed: " ++ T.unpack err

-- Helper instances
instance FromJSON BrowseResult where
  parseJSON = withObject "BrowseResult" $ \v -> BrowseResult
    <$> v .: "content"
    <*> v .: "status"
    <*> v .: "links"
    <*> v .:? "screenshot"

instance ToJSON BrowseResult where
  toJSON br = object
    [ "content" .= content br
    , "status" .= status br
    , "links" .= links br
    , "screenshot" .= screenshot br
    ]
