{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module External.ChatAPI
  ( simpleChat, Message(..), statusChat, messageChat, statusChatReadAPIKey
  , ChatModel(..), OpenAIModel(..), DeepSeekModel(..)
  , ChatParams(..), ChatSetting(..), ChatStatus(..), chatSettingAlternative, chatSettingMaybeWrapper
  ) where

import Control.Exception (try, SomeException)
import Control.Applicative
import Control.Monad.ExceptionReturn
import Control.Monad.Trans.ReaderState
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson as A
import Data.Bifunctor
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

data ChatModel
  = OpenAI OpenAIModel 
  | DeepSeek DeepSeekModel 
  | DeepSeekR1_14B
  deriving (Show, Eq)

data OpenAIModel = GPT4oMini | GPT4o deriving (Show, Eq)
data DeepSeekModel = DeepSeekChat | DeepSeekReasoner deriving (Show, Eq)

modelEndpoint :: ChatModel -> String
modelEndpoint OpenAI {}      = "https://api.openai.com/v1/chat/completions"
modelEndpoint DeepSeek {}    = "https://api.deepseek.com/chat/completions"
modelEndpoint DeepSeekR1_14B = "http://10.52.1.55:11434/api/chat" -- ^ my local network, won't work for anyone else

instance ToJSON ChatModel where
  toJSON (OpenAI GPT4oMini)          = "gpt-4o-mini"
  toJSON (OpenAI GPT4o)              = "gpt-4o"
  toJSON (DeepSeek DeepSeekChat)     = "deepseek-chat"
  toJSON (DeepSeek DeepSeekReasoner) = "deepseek-reasoner"
  toJSON DeepSeekR1_14B              = "deepseek-r1:14b"

-- | Modified ChatParams with tool support
data ChatParams (ts :: k) = ChatParams
  { chatModel     :: ChatModel    -- ^ LLM model to use
  , chatMarkDown  :: Bool         -- ^ Enable markdown formatting
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

-- class ChatAPI (model :: ChatModel) where
--   modelEndpoint :: Proxy model -> String

data ChatCompletionResponse = ChatCompletionResponse
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

data Message
  = SystemMessage    { content :: Text }
  | UserMessage      { content :: Text }
  | AssistantMessage { content :: Text }
  | ToolMessage      { content :: Text, toolMeta :: Maybe ToolMeta }
  deriving (Generic, Eq, Ord, Read, NFData)

deriving instance Show Message

instance FromJSON ChatCompletionResponse where
  parseJSON = withObject "ChatCompletionResponse" $ \v -> ChatCompletionResponse
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
      "assistant" -> AssistantMessage <$> v .: "content"
      "tool"      -> do
          ToolMessage <$> (v .: "content") <*> v .:? "toolMeta"
      _           -> error "Invalid role in message"

-- | Warning: due to the deepseek model unable to recognize the role "tool", we use "system" instead
instance ToJSON Message where
  toJSON (SystemMessage    c)   = A.object ["role" .= ("system" :: Text), "content" .= c]
  toJSON (UserMessage      c)   = A.object ["role" .= ("user" :: Text), "content" .= c]
  toJSON (AssistantMessage c)   = A.object ["role" .= ("assistant" :: Text), "content" .= c]
  toJSON (ToolMessage      c _) = A.object ["role" .= ("assistant" :: Text), "content" .= c]
  --("tool" :: Text), "content" .= c]

apiKeyFile :: FilePath
apiKeyFile = "apiKey"

data ChatRequest = ChatRequest
  { chatReqModel :: ChatModel
  , messages     :: [Message]
  , temperature  :: Double
  , stream       :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON ChatRequest where
  toJSON chatReq = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= messages chatReq
    , "temperature" .= temperature chatReq
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ] 
    -- <> [ "tools" .= tls | Just tls <- [tools chatReq] ]

promptMessage :: String -> Message
promptMessage prompt = UserMessage (pack prompt)

generateRequestBody :: ConstraintList ToolClass ts => ChatParams ts -> [Message] -> BL.ByteString
generateRequestBody param mes = encode $
  ChatRequest
    { chatReqModel = chatModel param
    , messages     = sysMessage : mes
    , temperature  = fromMaybe 0.5 (systemTemp $ chatSetting param)
    , stream       = case chatModel param of
        DeepSeek _     -> Just False
        OpenAI _       -> Nothing
        DeepSeekR1_14B -> Just False
    -- , tools       = Nothing
    }
  where sysMessage = generateSystemPrompt param

generateSystemPrompt :: forall ts. ConstraintList ToolClass ts => ChatParams ts -> Message
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
          "You are the endearing catgirl assistant named '喵喵'. \
          \ You adore using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]'."
          <> appendToolPrompts (Proxy @ts) "\n---\n"
        )
        (systemMessage $ chatSetting params)

data APIKey = APIKey
  { apiKeyOpenAI :: Maybe Text
  , apiKeyDeepSeek :: Maybe Text
  } deriving (Show, Read, Eq, Generic, NFData)

getApiKeyByModel :: ChatModel -> APIKey -> Maybe Text
getApiKeyByModel (OpenAI _)   apiKey = apiKeyOpenAI apiKey
getApiKeyByModel (DeepSeek _) apiKey = apiKeyDeepSeek apiKey
getApiKeyByModel DeepSeekR1_14B _ = Nothing

fetchChatCompletionResponse :: ConstraintList ToolClass ts => APIKey -> ChatParams ts -> [Message] -> IO (Either Text ChatCompletionResponse)
fetchChatCompletionResponse apiKey model msg = do
  let customTimeout = 60 * 1000000 -- 60 seconds in microseconds
  let customManagerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro customTimeout }
  manager <- newManager customManagerSettings
  request <- parseRequest (modelEndpoint $ chatModel model)
  case getApiKeyByModel (chatModel model) apiKey of
    Nothing -> return $ Left $ "No API key found for the model " <> T.pack (show $ chatModel model)
    Just apikey -> do
      let requestBody = generateRequestBody model msg --promptMessage prompt
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

displayResponse :: ChatCompletionResponse -> Text
displayResponse inp = let chos = choices inp in
  case chos of
    []         -> ""
    headChos:_ -> (content . message) headChos

readApiKeyFile :: ExceptT Text IO APIKey
readApiKeyFile = ExceptT
  . fmap
    (bimap
      ((T.concat ["Expect api key file \"", T.pack apiKeyFile, "\", while trying to read this file, the following error occured: "] <>) . T.pack . show)
      id
    )
  . try @SomeException
  $ read <$> readFile apiKeyFile

simpleChat :: ConstraintList ToolClass ts => ChatParams ts -> String -> ExceptT Text IO Text
simpleChat model prompt = do
  apiKey <- readApiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse apiKey model [promptMessage prompt]
  return $ displayResponse result

data ChatStatus = ChatStatus
  { chatStatusToolDepth      :: Int
  , chatStatusTotalToolCalls :: Int
  , chatStatusMessages       :: [Message]
  } deriving (Show, Eq)

-- | Remember that ExceptT is right monad transformer, it composes inside out
-- to preserve state, it should be inner than StateT
newtype ChatT tools m a = ChatT { runChatT :: ExceptT Text (ReaderStateT (ChatParams tools) ChatStatus m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadState ChatStatus, MonadReader (ChatParams tools)) via ExceptT Text (ReaderStateT (ChatParams tools) ChatStatus m)

instance MonadTrans (ChatT tools) where
  lift = ChatT . lift . lift

liftE :: Monad m => ExceptT Text m a -> ChatT tools m a
liftE = ChatT . ExceptT . lift . runExceptT

-- | Enhanced message chat with tool handling
agent :: ConstraintList ToolClass ts => ChatT ts IO Text
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
      firstChoice <- liftE . pureEMsg "No response in choices" $ listToMaybe (choices response)
      let amsg = message firstChoice
      liftIO $ print amsg
      case parseToolCall (content amsg) of
        Nothing -> return $ content $ amsg
        Just (ToolCallPair toolName args) -> do
          toolmsg <- handleToolCall toolName args (ToolMeta "tool_call_id" toolName 0) -- replace with actual tool call id
          liftIO $ print toolmsg
          modify $ \st -> st
            { chatStatusToolDepth      = chatStatusToolDepth st + 1
            , chatStatusTotalToolCalls = chatStatusTotalToolCalls st + 1
            , chatStatusMessages       = chatStatusMessages st <> [amsg, toolmsg]
            }
          agent

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will only read apiKey from chat setting
statusChat :: ConstraintList ToolClass ts => ChatParams ts -> ChatStatus -> IO (Either Text Text, ChatStatus)
statusChat = runReaderStateT . runExceptT . runChatT $ agent

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will read apiKey from file if no api key is found in the chat setting
statusChatReadAPIKey :: forall ts. ConstraintList ToolClass ts => ChatParams ts -> ChatStatus -> IO (Either Text Text, ChatStatus)
statusChatReadAPIKey params st = do
  apiKey <- either (const Nothing) Just <$> runExceptT readApiKeyFile
  let params' = params { chatSetting = (chatSetting params) { systemApiKeys = systemApiKeys (chatSetting params) <|> apiKey } }
  liftIO $ statusChat @ts params' st

-- | Tool calls are discarded in the output
-- system message will be appended so don't add it to the input
-- will try to read apiKey file if no api key is found in the chat setting
messageChat :: forall ts. ConstraintList ToolClass ts => ChatParams ts -> [Message] -> ExceptT Text IO Text
messageChat params prevMsg
  | Just _ <- systemApiKeys (chatSetting params) = ExceptT $ fmap fst $ statusChat params (ChatStatus 0 0 prevMsg)
  | otherwise = do
      apiKey <- readApiKeyFile
      let params' = params { chatSetting = (chatSetting params) { systemApiKeys = Just apiKey } }
      ExceptT $ fmap fst $ statusChat @ts params' (ChatStatus 0 0 prevMsg)

-- | Handle tool execution, no recursion
handleToolCall :: forall ts. ConstraintList ToolClass ts => Text -> Value -> ToolMeta -> ChatT ts IO Message
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
