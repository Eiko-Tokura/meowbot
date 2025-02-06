{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, TypeFamilies #-}

module External.ChatAPI
  ( simpleChat, Message(..), statusChat, messageChat, messagesChat, statusChatReadAPIKey
  , ChatModel(..)
  , OpenAIModel(..), DeepSeekModel(..), LocalModel(..)
  , ChatParams(..), ChatSetting(..), ChatStatus(..), chatSettingAlternative, chatSettingMaybeWrapper
  , ChatAPI(..), APIKey(..)
  ) where

import Control.Exception (try, SomeException)
import Control.Applicative
import Control.Monad.ExceptionReturn
import Control.Monad.Trans.ReaderState
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson as A
import Data.Bifunctor
import Data.Either
import Data.Text (Text, pack)
import Data.HList
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Control.DeepSeq
import External.ChatAPI.Tool

import Parser.Run

data ChatModel
  = OpenAI OpenAIModel
  | DeepSeek DeepSeekModel
  | Local LocalModel
  deriving (Show, Read, Eq)

data OpenAIModel   = GPT4oMini | GPT4o deriving (Show, Read, Eq)
data DeepSeekModel = DeepSeekChat | DeepSeekReasoner deriving (Show, Read, Eq)
data LocalModel    = DeepSeekR1_14B | DeepSeekR1_32B deriving (Show, Read, Eq)

modelEndpoint :: ChatModel -> String
modelEndpoint OpenAI {}   = "https://api.openai.com/v1/chat/completions"
modelEndpoint DeepSeek {} = "https://api.deepseek.com/chat/completions"
modelEndpoint Local {}    = "http://10.52.1.55:11434/api/chat" -- ^ my local network, won't work for anyone else

instance ToJSON ChatModel where
  toJSON (OpenAI GPT4oMini)          = "gpt-4o-mini"
  toJSON (OpenAI GPT4o)              = "gpt-4o"
  toJSON (DeepSeek DeepSeekChat)     = "deepseek-chat"
  toJSON (DeepSeek DeepSeekReasoner) = "deepseek-reasoner"
  toJSON (Local DeepSeekR1_14B)      = "deepseek-r1:14b"
  toJSON (Local DeepSeekR1_32B)      = "deepseek-r1:32b"

-- | Modified ChatParams with tool support
data ChatParams (md :: ChatModel) (ts :: k) = ChatParams
  { chatMarkDown  :: Bool         -- ^ Enable markdown formatting
  , chatSetting   :: ChatSetting  -- ^ Temperature and system message
  --, chatTools     :: CList ToolClass ts  -- ^ List of tools
  }

data ChatSetting = ChatSetting
  { systemMessage      :: Maybe Message
  , systemTemp         :: Maybe Double
  , systemMaxToolDepth :: Maybe Int       -- ^ Maximum tool call attempts
  , systemApiKeys      :: Maybe APIKey       -- ^ API keys for chat models
  } deriving (Show, Eq, Read, Generic, NFData)

defaultSystemMaxToolDepth :: Int
defaultSystemMaxToolDepth = 5

chatSettingMaybeWrapper :: Maybe ChatSetting -> ChatSetting
chatSettingMaybeWrapper = fromMaybe (ChatSetting Nothing Nothing Nothing Nothing)

chatSettingAlternative :: Maybe ChatSetting -> ChatSetting -> ChatSetting
chatSettingAlternative mnew def = ChatSetting
  { systemMessage      = (systemMessage =<< mnew)      <|> systemMessage def
  , systemTemp         = (systemTemp =<< mnew)         <|> systemTemp    def
  , systemMaxToolDepth = (systemMaxToolDepth =<< mnew) <|> systemMaxToolDepth def
  , systemApiKeys      = (systemApiKeys =<< mnew)      <|> systemApiKeys def
  }

class GetMessage a where
  getMessage :: a -> Either Text Message

class 
  ( ToJSON (ModelDependent model ChatRequest)
  , FromJSON (ChatCompletionResponse model)
  , GetMessage (ChatCompletionResponse model)
  ) => ChatAPI model where
  type ChatCompletionResponse model
  chatModel :: ChatModel

instance ChatAPI (OpenAI GPT4oMini) where
  chatModel  = OpenAI GPT4oMini
  type ChatCompletionResponse (OpenAI GPT4oMini) = ChatCompletionResponseOpenAI

instance ChatAPI (OpenAI GPT4o) where
  chatModel  = OpenAI GPT4o
  type ChatCompletionResponse (OpenAI GPT4o) = ChatCompletionResponseOpenAI

instance ChatAPI (DeepSeek DeepSeekChat) where
  chatModel  = DeepSeek DeepSeekChat
  type ChatCompletionResponse (DeepSeek DeepSeekChat) = ChatCompletionResponseOpenAI

instance ChatAPI (DeepSeek DeepSeekReasoner) where
  chatModel  = DeepSeek DeepSeekReasoner
  type ChatCompletionResponse (DeepSeek DeepSeekReasoner) = ChatCompletionResponseOpenAI

instance ChatAPI (Local DeepSeekR1_14B) where
  chatModel  = Local DeepSeekR1_14B
  type ChatCompletionResponse (Local DeepSeekR1_14B) = ChatCompletionResponseOllama

instance ChatAPI (Local DeepSeekR1_32B) where
  chatModel  = Local DeepSeekR1_32B
  type ChatCompletionResponse (Local DeepSeekR1_32B) = ChatCompletionResponseOllama

data ChatCompletionResponseOpenAI = ChatCompletionResponseOpenAI
  { responseId :: Text
  , object     :: Text
  , created    :: Int
  , choices    :: [Choice]
  , usage      :: Object
  } deriving (Show)

data Choice = Choice
  { index         :: Int
  , message       :: Message
  , finish_reason :: Text
  } deriving (Show, Generic)

data ChatCompletionResponseOllama = ChatCompletionResponseOllama 
  { ollamaResponse :: Message 
  --, ollamaDoneReason :: Text
  }
  deriving (Show)

instance FromJSON ChatCompletionResponseOllama where
  parseJSON = withObject "ChatCompletionResponse" $ \v -> ChatCompletionResponseOllama
    <$> v .: "message"

data Message
  = SystemMessage    { content :: Text }
  | UserMessage      { content :: Text }
  | AssistantMessage { content :: Text, thinking :: Maybe Text }
  | ToolMessage      { content :: Text, toolMeta :: Maybe ToolMeta }
  deriving (Generic, Eq, Ord, Read, NFData)

-- | Wrapper for model dependent content (toJSON and FromJSON)
newtype ModelDependent m a = ModelDependent { runModelDependent :: a }
  deriving (Show, Eq, Generic)
  deriving newtype (NFData)

deriving instance Show Message

instance FromJSON ChatCompletionResponseOpenAI where
  parseJSON = withObject "ChatCompletionResponse" $ \v -> ChatCompletionResponseOpenAI
    <$> v .: "id"       -- due to the name 'id' is a keyword in Haskell, we use 'responseId' instead
    <*> v .: "object"
    <*> v .: "created"
    <*> v .: "choices"
    <*> v .: "usage"

instance FromJSON Choice

-- | Handle malformed JSON responses
instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> do
    role    <- v .: "role"
    case role :: Text of
      "system"    -> SystemMessage    <$> v .: "content"
      "user"      -> UserMessage      <$> v .: "content"
      "assistant" -> do
          ct <- v .: "content"
          let (thinking, ct') = parseThinking ct
          return $ AssistantMessage ct' thinking
      "tool"      -> do
          ToolMessage <$> (v .: "content") <*> v .:? "toolMeta"
      _           -> error "Invalid role in message"

parseThinking :: Text -> (Maybe Text, Text)
parseThinking ct = maybe (Nothing, ct) 
  (\(think, content) -> if T.null (T.strip think) then (Nothing, content) else (Just think, content))
  $ runParser
    ( string "<think>" 
    *> ( (,)
          <$> manyTill' (string "</think>") getItem 
          <* (string "</think>" >> many spaceOrEnter) <*> many' getItem
       )
    ) ct

-- | Warning: due to the deepseek model unable to recognize the role "tool", we use "system" instead
instance ToJSON (ModelDependent (DeepSeek DeepSeekReasoner) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c Nothing)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c (Just think)))
    = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>" <> c <> "</think>\n\n" <> think)]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (DeepSeek a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (OpenAI a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("tool" :: Text)      , "content" .= c]

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (Local a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)   , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)     , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _)) = A.object ["role" .= ("assistant" :: Text), "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("assistant" :: Text), "content" .= c]

instance ToJSON (ModelDependent (Local DeepSeekR1_32B) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c Nothing)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c (Just think)))
    = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>" <> c <> "</think>\n\n" <> think)]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]

instance ToJSON (ModelDependent (Local DeepSeekR1_14B) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c Nothing)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c (Just think)))
    = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>" <> c <> "</think>\n\n" <> think)]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]

apiKeyFile :: FilePath
apiKeyFile = "apiKey"

data ChatRequest = ChatRequest
  { chatReqModel :: ChatModel
  , messages     :: [Message]
  , temperature  :: Double
  , stream       :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON (ModelDependent (OpenAI a) ChatRequest) where
  toJSON (ModelDependent chatReq) = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= map (ModelDependent @(OpenAI a)) (messages chatReq)
    , "temperature" .= temperature chatReq
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ] 

instance ToJSON (ModelDependent (DeepSeek a) ChatRequest) where
  toJSON (ModelDependent chatReq) = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= map (ModelDependent @(DeepSeek a)) (messages chatReq)
    , "temperature" .= temperature chatReq
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ] 
    -- <> [ "tools" .= tls | Just tls <- [tools chatReq] ]

-- | Ollama compatible format
instance ToJSON (ModelDependent (Local a) ChatRequest) where
  toJSON (ModelDependent chatReq) = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= map (ModelDependent @(Local a)) (messages chatReq)
    , "options" .= A.object (
        ["temperature" .= temperature chatReq]
      )
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ]

promptMessage :: String -> Message
promptMessage prompt = UserMessage (pack prompt)

generateRequestBody :: forall md ts. (ChatAPI md, ConstraintList ToolClass ts) => ChatParams md ts -> [Message] -> BL.ByteString
generateRequestBody param mes = encode $ ModelDependent @md $
  ChatRequest
    { chatReqModel = chatModel @md
    , messages     = sysMessage : mes
    , temperature  = fromMaybe 0.5 (systemTemp $ chatSetting param)
    , stream       = case chatModel @md of
        DeepSeek _ -> Just False
        OpenAI _   -> Nothing
        Local _    -> Just False
    -- , tools       = Nothing
    }
  where sysMessage = generateSystemPrompt param

generateSystemPrompt :: forall md ts. ConstraintList ToolClass ts => ChatParams md ts -> Message
generateSystemPrompt params
  | chatMarkDown params = SystemMessage $
      "You are a endearing catgirl assistant named '喵喵'. \
      \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T'. \
      \ Always answer in markdown format, put your formulas in latex form enclosed by $ or $$. \
      \ Do not use \\( \\) or \\[ \\] for formulas."
      <> appendToolPrompts (Proxy @ts) "\n---\n"
  | otherwise = 
      fromMaybe 
        (SystemMessage $ 
          "You are the helpful endearing catgirl assistant named '喵喵'. You adores using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]' (no space after ',')."
          <> appendToolPrompts (Proxy @ts) "\n---\n"
        )
        (systemMessage $ chatSetting params)

data APIKey = APIKey
  { apiKeyOpenAI :: Maybe Text
  , apiKeyDeepSeek :: Maybe Text
  } deriving (Show, Read, Eq, Generic, NFData)

data GetAPIKey
  = NoAPIKeyRequired
  | APIKeyRequired (Maybe Text)

getApiKeyByModel :: ChatModel -> APIKey -> GetAPIKey
getApiKeyByModel (OpenAI _)   apiKey = APIKeyRequired $ apiKeyOpenAI apiKey
getApiKeyByModel (DeepSeek _) apiKey = APIKeyRequired $ apiKeyDeepSeek apiKey
getApiKeyByModel (Local _)    _      = NoAPIKeyRequired

fetchChatCompletionResponse :: forall md ts. (ChatAPI md, ConstraintList ToolClass ts) => APIKey -> ChatParams md ts -> [Message] -> IO (Either Text (ChatCompletionResponse md))
fetchChatCompletionResponse apiKey model msg = do
  let customTimeout = 120 * 1000000 -- 120 seconds in microseconds
  let customManagerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro customTimeout }
  manager <- newManager customManagerSettings
  request <- parseRequest (modelEndpoint $ chatModel @md)
  let requestBody = generateRequestBody model msg --promptMessage prompt
  case getApiKeyByModel (chatModel @md) apiKey of
    APIKeyRequired Nothing       -> return . Left $ "No API key found for the model " <> T.pack (show $ chatModel @md)
    APIKeyRequired (Just apikey) -> do
      let request' = request
            { method = "POST"
            , requestBody = RequestBodyBS (BL.toStrict requestBody)
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Authorization", "Bearer " <> encodeUtf8 apikey)
                ]
            }
      result <- try (httpLbs request' manager) :: IO (Either SomeException (Response BL.ByteString))
      return $ bimap (T.pack . show) responseBody result >>= first T.pack . eitherDecode
    NoAPIKeyRequired -> do
      let request' = request
            { method = "POST"
            , requestBody = RequestBodyBS (BL.toStrict requestBody)
            , requestHeaders =
                [ ("Content-Type", "application/json")
                ]
            }
      result <- try (httpLbs request' manager) :: IO (Either SomeException (Response BL.ByteString))
      return $ bimap (T.pack . show) responseBody result >>= first T.pack . eitherDecode

instance GetMessage (ChatCompletionResponseOpenAI) where
  getMessage = fmap message . maybe (Left "No choices in response") Right . listToMaybe . choices

instance GetMessage (ChatCompletionResponseOllama) where
  getMessage = return . ollamaResponse

-- displayResponse :: ChatCompletionResponse -> Text
-- displayResponse inp = let chos = choices inp in
--   case chos of
--     []         -> ""
--     headChos:_ -> (content . message) headChos

readApiKeyFile :: ExceptT Text IO APIKey
readApiKeyFile = ExceptT
  . fmap
    (bimap
      ((T.concat ["Expect api key file \"", T.pack apiKeyFile, "\", while trying to read this file, the following error occured: "] <>) . T.pack . show)
      id
    )
  . try @SomeException
  $ read <$> readFile apiKeyFile

simpleChat :: (ChatAPI md, ConstraintList ToolClass ts) => ChatParams md ts -> String -> ExceptT Text IO Text
simpleChat model prompt = do
  apiKey <- readApiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse apiKey model [promptMessage prompt]
  return $ fromRight "" $ content <$> getMessage result

data ChatStatus = ChatStatus
  { chatStatusToolDepth      :: Int
  , chatStatusTotalToolCalls :: Int
  , chatStatusMessages       :: [Message]
  } deriving (Show, Eq, Generic, NFData)

-- | Remember that ExceptT is right monad transformer, it composes inside out
-- to preserve state, it should be inner than StateT
newtype ChatT md tools m a = ChatT { runChatT :: ExceptT Text (ReaderStateT (ChatParams md tools) ChatStatus m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadState ChatStatus, MonadReader (ChatParams md tools)) via ExceptT Text (ReaderStateT (ChatParams md tools) ChatStatus m)

instance MonadTrans (ChatT md tools) where
  lift = ChatT . lift . lift

liftE :: Monad m => ExceptT Text m a -> ChatT md tools m a
liftE = ChatT . ExceptT . lift . runExceptT

-- | Enhanced message chat with tool handling
agent :: (ChatAPI md, ConstraintList ToolClass ts) => ChatT md ts IO [Message]
agent = do
  params    <- ask
  prevMsgs  <- gets chatStatusMessages
  toolDepth <- gets chatStatusToolDepth
  mapiKeys  <- asks (systemApiKeys . chatSetting)
  apiKeys   <- liftE $ pureEMsg "No API key found!" mapiKeys
  if fromMaybe defaultSystemMaxToolDepth (systemMaxToolDepth (chatSetting params)) <= toolDepth
  then liftE $ throwE "Maximum tool depth exceeded"
  else do
      response <- liftE . ExceptT $ fetchChatCompletionResponse apiKeys params prevMsgs
      amsg <- liftE . pureE $ getMessage response
      liftIO $ printMessage amsg
      case parseToolCall (content amsg) of
        Nothing -> return $ [amsg]
        Just (ToolCallPair toolName args) -> do
          toolmsg <- handleToolCall toolName args (ToolMeta "tool_call_id" toolName 0) -- replace with actual tool call id
          liftIO $ printMessage toolmsg
          modify $ \st -> st
            { chatStatusToolDepth      = chatStatusToolDepth st + 1
            , chatStatusTotalToolCalls = chatStatusTotalToolCalls st + 1
            , chatStatusMessages       = chatStatusMessages st <> [amsg, toolmsg]
            }
          ([amsg, toolmsg] <>) <$> agent

printMessage :: Message -> IO ()
printMessage (SystemMessage c)             = putStrLn $ "SystemMessage: "     <> T.unpack c
printMessage (UserMessage c)               = putStrLn $ "UserMessage: "       <> T.unpack c
printMessage (AssistantMessage c Nothing)  = putStrLn $ "AssistantMessage: "  <> T.unpack c
printMessage (AssistantMessage c (Just t)) = putStrLn $ "AssistantThinking: " <> T.unpack t <> "\nAssistantMessage: " <> T.unpack c
printMessage (ToolMessage c tm)            = putStrLn $ "ToolMessage: "       <> T.unpack c <> "\nToolMeta: "         <> show tm

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will only read apiKey from chat setting
statusChat :: (ChatAPI md, ConstraintList ToolClass ts) => ChatParams md ts -> ChatStatus -> IO (Either Text [Message], ChatStatus)
statusChat = runReaderStateT . runExceptT . runChatT $ agent

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will read apiKey from file if no api key is found in the chat setting
statusChatReadAPIKey :: forall md ts. (ChatAPI md, ConstraintList ToolClass ts) => ChatParams md ts -> ChatStatus -> IO (Either Text [Message], ChatStatus)
statusChatReadAPIKey params st = do
  apiKey <- either (const Nothing) Just <$> runExceptT readApiKeyFile
  let params' = params { chatSetting = (chatSetting params) { systemApiKeys = systemApiKeys (chatSetting params) <|> apiKey } }
  liftIO $ statusChat @md @ts params' st

-- | Tool calls are discarded in the output
-- system message will be appended so don't add it to the input
-- will try to read apiKey file if no api key is found in the chat setting
messageChat :: forall md ts. (ChatAPI md, ConstraintList ToolClass ts) => ChatParams md ts -> [Message] -> ExceptT Text IO Message
messageChat params prevMsg
  | Just _ <- systemApiKeys (chatSetting params) = ExceptT $ fmap (fmap last . fst) $ statusChat @md params (ChatStatus 0 0 prevMsg)
  | otherwise = do
      apiKey <- readApiKeyFile
      let params' = params { chatSetting = (chatSetting params) { systemApiKeys = Just apiKey } }
      ExceptT $ fmap (fmap last . fst) $ statusChat @md @ts params' (ChatStatus 0 0 prevMsg)

-- | Tool calls are also included in the output
-- system message will be appended so don't add it to the input
-- will try to read apiKey file if no api key is found in the chat setting
-- returns the list of new messages: assistant tool calls followed by the final response
messagesChat :: forall md ts. (ChatAPI md, ConstraintList ToolClass ts) => ChatParams md ts -> [Message] -> ExceptT Text IO [Message]
messagesChat params prevMsg
  | Just _ <- systemApiKeys (chatSetting params) = ExceptT $ fmap fst $ statusChat @md params (ChatStatus 0 0 prevMsg)
  | otherwise = do
      apiKey <- readApiKeyFile
      let params' = params { chatSetting = (chatSetting params) { systemApiKeys = Just apiKey } }
      ExceptT $ fmap fst $ statusChat @md @ts params' (ChatStatus 0 0 prevMsg)

-- | Handle tool execution, no recursion
handleToolCall :: forall md ts. ConstraintList ToolClass ts => Text -> Value -> ToolMeta -> ChatT md ts IO Message
handleToolCall toolCallName args md = do
  -- Find matching tool
  output <- 
    case pickConstraint (Proxy @ToolClass) (Proxy @ts) ((== toolCallName) . toolName) of
      Just toolCont -> toolCont $ \tool -> do
        input  <-  liftE . ExceptT $ pure (jsonToInput tool args)
        lift $ wrapToolOutput <$> runExceptT (toolHandlerTextError tool input) 
        -- Execute tool, tool error is caught and wrapped for agent to handle
      Nothing       -> liftE . throwE $ "Unknown tool: " <> toolCallName
  
  -- Build updated message history
  let toolMsg = ToolMessage
        { content = case output of
            String t -> t
            _ -> decodeUtf8 (BL.toStrict $ encode output)
        , toolMeta = Just md
            { toolCallId = "dummy_toolcall_id" -- T.pack (show (hash prevMsgs))  -- Simple ID generation
            , toolAttempt = toolAttempt md + 1
            }
        }
  return toolMsg
  where wrapToolOutput (Right t) = A.object ["tool_output" .= t]
        wrapToolOutput (Left e)  = A.object ["tool_error"  .= e]
