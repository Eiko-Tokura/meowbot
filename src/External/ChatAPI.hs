{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia, DerivingStrategies #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, TypeFamilies #-}

module External.ChatAPI
  ( simpleChat, Message(..), mapMessage, statusChat, messageChat, messagesChat, statusChatReadAPIKey
  , ChatModel(..)
  , OpenAIModel(..), DeepSeekModel(..), LocalModel(..)
  , OpenRouterModel(..), SiliconFlowModel(..)
  , ChatParams(..), ChatSetting(..), ChatStatus(..), chatSettingAlternative, chatSettingMaybeWrapper
  , ChatAPI(..), APIKey(..)
  , printMessage
  ) where

import Control.Exception (try, SomeException)
import Control.Applicative
import Control.Monad.Except
import Control.Monad.ExceptionReturn
import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Network.HTTP.Client hiding (Proxy)
import Data.Aeson as A
import Data.Bifunctor
import Data.Default
import Data.Either
import Data.Text (Text, pack)
import Data.HList
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
  | OpenRouter OpenRouterModel
  | SiliconFlow SiliconFlowModel
  deriving (Show, Read, Eq)

data OpenAIModel     = GPT4oMini | GPT4o | O3Mini deriving (Show, Read, Eq)
data DeepSeekModel   = DeepSeekChat | DeepSeekReasoner deriving (Show, Read, Eq)
data LocalModel      = DeepSeekR1_14B | DeepSeekR1_32B | Qwen2_5_32B | Command_R_Latest deriving (Show, Read, Eq)
data OpenRouterModel = OR_DeepSeekV3_Free | OR_DeepSeekR1_Free | OR_DeepSeekR1 deriving (Show, Read, Eq)
data SiliconFlowModel = SF_DeepSeekV3 deriving (Show, Read, Eq)

modelEndpoint :: ChatModel -> String
modelEndpoint OpenAI {}      = "https://api.openai.com/v1/chat/completions"
modelEndpoint DeepSeek {}    = "https://api.deepseek.com/chat/completions"
modelEndpoint Local {}       = "http://10.52.1.55:11434/api/chat" -- ^ my local network, won't work for anyone else
modelEndpoint OpenRouter {}  = "https://openrouter.ai/api/v1/chat/completions"
modelEndpoint SiliconFlow {} = "https://api.siliconflow.cn/v1/chat/completions"

instance ToJSON ChatModel where
  toJSON (OpenAI GPT4oMini)           = "gpt-4o-mini"
  toJSON (OpenAI GPT4o)               = "gpt-4o"
  toJSON (OpenAI O3Mini)              = "o3-mini"
  toJSON (DeepSeek DeepSeekChat)      = "deepseek-chat"
  toJSON (DeepSeek DeepSeekReasoner)  = "deepseek-reasoner"
  toJSON (Local DeepSeekR1_14B)       = "deepseek-r1:14b"
  toJSON (Local DeepSeekR1_32B)       = "deepseek-r1:32b"
  toJSON (Local Qwen2_5_32B)          = "qwen2.5:32b"
  toJSON (Local Command_R_Latest)     = "command-r:latest"
  toJSON (OpenRouter OR_DeepSeekR1)   = "deepseek/deepseek-r1"
  toJSON (OpenRouter OR_DeepSeekV3_Free) = "deepseek/deepseek-chat:free"
  toJSON (OpenRouter OR_DeepSeekR1_Free) = "deepseek/deepseek-r1:free"
  toJSON (SiliconFlow SF_DeepSeekV3)  = "deepseek-ai/DeepSeek-V3"

-- | Modified ChatParams with tool support
data ChatParams (md :: ChatModel) (ts :: k) = ChatParams
  { chatMarkDown  :: Bool         -- ^ Enable markdown formatting
  , chatSetting   :: ChatSetting  -- ^ Temperature and system message
  , chatManager   :: Manager      -- ^ HTTP manager
  , chatTimeout   :: Int          -- ^ Timeout in microseconds
  --, chatTools     :: CList ToolClass ts  -- ^ List of tools
  }

data ChatSetting = ChatSetting
  { systemMessage        :: Maybe Message
  , systemTemp           :: Maybe Double
  , systemMaxToolDepth   :: Maybe Int       -- ^ Maximum tool call attempts
  , systemApiKeys        :: Maybe APIKey       -- ^ API keys for chat models
  , chatTruncateThinking :: Maybe Bool
  } deriving (Show, Eq, Read, Generic, NFData)

instance Default ChatSetting where
  def = ChatSetting Nothing Nothing Nothing Nothing Nothing

defaultSystemMaxToolDepth :: Int
defaultSystemMaxToolDepth = 5

chatSettingMaybeWrapper :: Maybe ChatSetting -> ChatSetting
chatSettingMaybeWrapper = fromMaybe def

chatSettingAlternative :: Maybe ChatSetting -> ChatSetting -> ChatSetting
chatSettingAlternative mnew alt = ChatSetting
  { systemMessage        = (systemMessage        =<< mnew) <|> systemMessage alt
  , systemTemp           = (systemTemp           =<< mnew) <|> systemTemp    alt
  , systemMaxToolDepth   = (systemMaxToolDepth   =<< mnew) <|> systemMaxToolDepth alt
  , systemApiKeys        = (systemApiKeys        =<< mnew) <|> systemApiKeys alt
  , chatTruncateThinking = (chatTruncateThinking =<< mnew) <|> chatTruncateThinking alt
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

instance ChatAPI (Local Qwen2_5_32B) where
  chatModel  = Local Qwen2_5_32B
  type ChatCompletionResponse (Local Qwen2_5_32B) = ChatCompletionResponseOllama

instance ChatAPI (Local Command_R_Latest) where
  chatModel  = Local Command_R_Latest
  type ChatCompletionResponse (Local Command_R_Latest) = ChatCompletionResponseOllama

instance ChatAPI (OpenRouter OR_DeepSeekR1_Free) where
  chatModel  = OpenRouter OR_DeepSeekR1_Free
  type ChatCompletionResponse (OpenRouter OR_DeepSeekR1_Free) = ChatCompletionResponseOpenAI

instance ChatAPI (OpenRouter OR_DeepSeekV3_Free) where
  chatModel  = OpenRouter OR_DeepSeekV3_Free
  type ChatCompletionResponse (OpenRouter OR_DeepSeekV3_Free) = ChatCompletionResponseOpenAI

instance ChatAPI (SiliconFlow SF_DeepSeekV3) where
  chatModel  = SiliconFlow SF_DeepSeekV3
  type ChatCompletionResponse (SiliconFlow SF_DeepSeekV3) = ChatCompletionResponseOpenAI

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
  | AssistantMessage { content :: Text, thinking :: Maybe Text, pureToolCall :: Maybe Bool }
  | ToolMessage      { content :: Text, toolMeta :: Maybe ToolMeta }
  deriving (Generic, Eq, Ord, Read, NFData)

mapMessage :: (Text -> Text) -> Message -> Message
mapMessage f (SystemMessage    c)     = SystemMessage    $ f c
mapMessage f (UserMessage      c)     = UserMessage      $ f c
mapMessage f (AssistantMessage c t p) = AssistantMessage (f c) (f <$> t) p
mapMessage f (ToolMessage      c m)   = ToolMessage      (f c) m

-- | Removes the thinking part of the assistant message
truncateThinking :: Message -> Message
truncateThinking (AssistantMessage c (Just _) p) = AssistantMessage c Nothing p
truncateThinking m = m

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
          ct        <- v .:  "content"
          reasoning <- v .:? "reasoning"
          let (thinking, ct') = case (reasoning, parseThinking ct) of
                (Just r, _) -> (Just r, ct)
                (_, p)      -> p
          return $ AssistantMessage ct' thinking Nothing
      "tool"      -> do
          ToolMessage <$> (v .: "content") <*> v .:? "toolMeta"
      _           -> error "Invalid role in message"

parseThinking :: Text -> (Maybe Text, Text)
parseThinking ct = maybe (Nothing, ct)
  (\(think, content) -> if T.null (T.strip think) then (Nothing, content) else (Just think, content))
  $ runParser
    ( string "<think>"
    *> ( (,)
          <$> (manyTill' (string "</think>") getItem <* (string "</think>" >> many spaceOrEnter))
          <*> many' getItem
       )
    ) ct

-- | Warning: due to the deepseek model unable to recognize the role "tool", we use "user" instead
instance ToJSON (ModelDependent (DeepSeek DeepSeekReasoner) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c Nothing _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c (Just think) _))
    = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>\n" <> c <> "</think>\n\n" <> think)]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text) , "content" .= c]
  -- ^ deepseek model does not support tool role, and we use user role instead

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (DeepSeek a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _ _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text) , "content" .= c]

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (OpenAI a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _ _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  -- ^ we haven't implemented tool role for openai model yet, so we use user role instead

deriving via (ModelDependent (OpenAI a) Message) instance ToJSON (ModelDependent (OpenRouter b) Message)
deriving via (ModelDependent (OpenAI a) Message) instance ToJSON (ModelDependent (SiliconFlow b) Message)

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (Local a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)   , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)     , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _ _)) = A.object ["role" .= ("assistant" :: Text), "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text), "content" .= c]
  -- ^ we haven't implemented tool role for ollama api yet, so we use user role instead

instance ToJSON (ModelDependent (Local DeepSeekR1_32B) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c Nothing _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c (Just think) _))
    = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>\n" <> c <> "</think>\n\n" <> think)]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text) , "content" .= c]
  -- ^ deepseek r1:32b model does not support tool role, and we use user role instead

deriving via (ModelDependent (Local DeepSeekR1_32B) Message) instance ToJSON (ModelDependent (Local DeepSeekR1_14B) Message)

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

deriving via (ModelDependent (OpenAI a) ChatRequest) instance ToJSON (ModelDependent (SiliconFlow b) ChatRequest)

instance ToJSON (ModelDependent (DeepSeek a) ChatRequest) where
  toJSON (ModelDependent chatReq) = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= map (ModelDependent @(DeepSeek a)) (messages chatReq)
    , "temperature" .= temperature chatReq
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ]
    -- <> [ "tools" .= tls | Just tls <- [tools chatReq] ]

instance ToJSON (ModelDependent (OpenRouter a) ChatRequest) where
  toJSON (ModelDependent chatReq) = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= map (ModelDependent @(OpenRouter a)) (messages chatReq)
    , "temperature" .= temperature chatReq
    , "provider" .= A.object
      [ "sort" .= ("price" :: Text)
      ]
    , "logprobs" .= False
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ]

instance {-# OVERLAPPING #-} ToJSON (ModelDependent (OpenRouter OR_DeepSeekR1_Free) ChatRequest) where
  toJSON (ModelDependent chatReq) = A.object $
    [ "model" .= chatReqModel chatReq
    , "messages" .= map (ModelDependent @(OpenRouter OR_DeepSeekR1_Free)) (messages chatReq)
    , "temperature" .= temperature chatReq
    , "provider" .= A.object
      [ "ignore" .= [ "Targon" :: Text ] -- ^ ignore Targon
      ]
    , "logprobs" .= False
    , "include_reasoning" .= True
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ]

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

generateRequestBody :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts) => ChatParams md ts -> [Message] -> BL.ByteString
generateRequestBody param mes = encode $ ModelDependent @md $
  ChatRequest
    { chatReqModel = chatModel @md
    , messages     = sysMessage : mes
    , temperature  = fromMaybe 0.5 (systemTemp $ chatSetting param)
    , stream       = case chatModel @md of
        DeepSeek _    -> Just False
        OpenAI _      -> Nothing
        Local _       -> Just False
        OpenRouter _  -> Just False
        SiliconFlow _ -> Just False
    -- , tools       = Nothing
    }
  where sysMessage = generateSystemPrompt @md @ts @m param

generateSystemPrompt :: forall md ts m. ConstraintList (ToolClass m) ts => ChatParams md ts -> Message
generateSystemPrompt params
  | chatMarkDown params = SystemMessage $
      "You are a endearing catgirl assistant named '喵喵'. \
      \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T'. \
      \ Always answer in markdown format, put your formulas in latex form enclosed by $ or $$. \
      \ Do not use \\( \\) or \\[ \\] for formulas."
      <> appendToolPrompts @m (Proxy @ts) "\n---\n"
  | otherwise =
      maybe
        (SystemMessage $
          "You are the helpful endearing catgirl assistant named '喵喵'. You adores using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]' (no space after ',')."
          <> appendToolPrompts @m (Proxy @ts) "\n---\n"
        )
        (SystemMessage . (<> appendToolPrompts @m (Proxy @ts) "\n---\n"))
        (fmap content . systemMessage $ chatSetting params)

data APIKey = APIKey
  { apiKeyOpenAI      :: Maybe Text
  , apiKeyDeepSeek    :: Maybe Text
  , apiKeyOpenRouter  :: Maybe Text
  , apiKeySiliconFlow :: Maybe Text
  } deriving (Show, Read, Eq, Generic, NFData)

data GetAPIKey
  = NoAPIKeyRequired
  | APIKeyRequired (Maybe Text)

getApiKeyByModel :: ChatModel -> APIKey -> GetAPIKey
getApiKeyByModel (OpenAI _)     apiKey  = APIKeyRequired $ apiKeyOpenAI apiKey
getApiKeyByModel (DeepSeek _)   apiKey  = APIKeyRequired $ apiKeyDeepSeek apiKey
getApiKeyByModel (Local _)    _         = NoAPIKeyRequired
getApiKeyByModel (OpenRouter _) apiKey  = APIKeyRequired $ apiKeyOpenRouter apiKey
getApiKeyByModel (SiliconFlow _) apiKey = APIKeyRequired $ apiKeySiliconFlow apiKey

fetchChatCompletionResponse :: forall md ts m. (MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => Manager -> Int -> APIKey -> ChatParams md ts -> [Message] -> m (Either Text (ChatCompletionResponse md))
fetchChatCompletionResponse manager _ apiKey model msg = do
  -- let customTimeout = 120 * 1000000 -- 120 seconds in microseconds
  -- let customManagerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro customTimeout }
  -- manager <- liftIO $ newManager customManagerSettings
  request <- liftIO $ parseRequest (modelEndpoint $ chatModel @md)
  let requestBody = generateRequestBody @md @ts @m model msg --promptMessage prompt
  $(logInfo) $ T.intercalate "\n" $
    [ ""
    , "------------------- Message Start -------------------" ]
    <> map showMessage msg <>
    [ "------------------- Message End ---------------------" ]
  -- liftIO $ putStrLn "------------------- Request Body -------------------"
  -- liftIO $ putStrLn $ "Request body = " <> show requestBody
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
      --liftIO $ putStrLn $ "Request = " <> show request'
      result <- liftIO $ do
        mres <- try @SomeException $ httpLbs request' manager
        -- putStrLn $ "Response = " <> show mres
        case mres of
          Right res -> return $ Right res
          Left e -> return $ Left $ toText e
      return $ bimap id responseBody result >>= first T.pack . eitherDecode
    NoAPIKeyRequired -> do
      let request' = request
            { method = "POST"
            , requestBody = RequestBodyBS (BL.toStrict requestBody)
            , requestHeaders =
                [ ("Content-Type", "application/json")
                ]
            }
      --liftIO $ putStrLn $ "Request = " <> show request'
      result <- liftIO $ do
        mres <- try @SomeException $ httpLbs request' manager
        -- putStrLn $ "Response = " <> show mres
        case mres of
          Right res -> return $ Right res
          Left e -> return $ Left $ toText e
      return $ bimap id responseBody result >>= first T.pack . eitherDecode

instance GetMessage (ChatCompletionResponseOpenAI) where
  getMessage = fmap message . maybe (Left "No choices in response") Right . listToMaybe . choices

instance GetMessage (ChatCompletionResponseOllama) where
  getMessage = return . ollamaResponse

readApiKeyFile :: MonadIO m => ExceptT Text m APIKey
readApiKeyFile = ExceptT
  . fmap
    (bimap
      ((T.concat ["Expect api key file \"", T.pack apiKeyFile, "\", while trying to read this file, the following error occured: "] <>) . T.pack . show)
      id
    )
  . liftIO
  . try @SomeException
  $ read <$> readFile apiKeyFile

simpleChat :: (MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => Manager -> Int -> ChatParams md ts -> String -> ExceptT Text m Text
simpleChat man timeOut model prompt = do
  apiKey <- readApiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse man timeOut apiKey model [promptMessage prompt]
  return $ fromRight "" $ content <$> getMessage result

data ChatStatus = ChatStatus
  { chatStatusToolDepth      :: !Int
  , chatStatusTotalToolCalls :: !Int
  , chatStatusMessages       :: ![Message]
  } deriving (Show, Eq, Generic, NFData)

-- | Remember that ExceptT is right monad transformer, it composes inside out
-- to preserve state, it should be inner than StateT
newtype ChatT md tools m a = ChatT { runChatT :: ExceptT Text (ReaderStateT (ChatParams md tools) ChatStatus m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadState ChatStatus, MonadReader (ChatParams md tools)) via ExceptT Text (ReaderStateT (ChatParams md tools) ChatStatus m)

deriving newtype instance MonadLogger m => MonadLogger (ChatT md tools m)

instance MonadTrans (ChatT md tools) where
  lift = ChatT . lift . lift

liftE :: Monad m => ExceptT Text m a -> ChatT md tools m a
liftE = ChatT . ExceptT . lift . runExceptT

-- | Enhanced message chat with tool handling
agent :: (MonadIO m, MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatT md ts m [Message]
agent = do
  params    <- ask
  trunThink <- asks (fromMaybe True . chatTruncateThinking . chatSetting)
  prevMsgs  <- (if trunThink then map truncateThinking else id) <$> gets chatStatusMessages
  toolDepth <- gets chatStatusToolDepth
  mapiKeys  <- asks (systemApiKeys . chatSetting)
  apiKeys   <- liftE $ pureEMsg "No API key found!" mapiKeys
  let man = chatManager params
      timeOut = chatTimeout params
  if fromMaybe defaultSystemMaxToolDepth (systemMaxToolDepth (chatSetting params)) <= toolDepth
  then liftE $ throwE "Maximum tool depth exceeded"
  else do
      --liftIO $ putStrLn "Fetching chat completion response..."
      response <- liftE . ExceptT $ fetchChatCompletionResponse man timeOut apiKeys params prevMsgs
      amsg' <- liftE . pureE $ getMessage response
      $(logInfo) $ showMessage amsg'
      case parseToolCall (content amsg') of
        Nothing -> return $ [amsg']
        Just (Nothing, ToolCallPair toolCallName args) -> do
          let amsg = case amsg' of
                AssistantMessage {} -> amsg' { pureToolCall = Just True } -- this assistant message is a pure tool call
                _ -> amsg'
          skip_toolmsg <- handleToolCall toolCallName args (ToolMeta "tool_call_id" toolCallName 0) -- replace with actual tool call id
          case skip_toolmsg of
            Left Skipped -> liftE $ throwE "Skipped"
            Right toolmsg -> do
              $(logInfo) $ showMessage toolmsg
              modify $ \st -> st
                { chatStatusToolDepth      = chatStatusToolDepth st + 1
                , chatStatusTotalToolCalls = chatStatusTotalToolCalls st + 1
                , chatStatusMessages       = chatStatusMessages st <> [amsg, toolmsg]
                }
              ([amsg, toolmsg] <>) <$> agent
        Just (Just _, ToolCallPair toolCallName args) -> do
          let amsg = case amsg' of
                AssistantMessage {} -> amsg' { pureToolCall = Just False } -- this assistant message is a mix of response and tool call
                _ -> amsg'
          skip_toolmsg <- handleToolCall toolCallName args (ToolMeta "tool_call_id" toolCallName 0) -- replace with actual tool call id
          case skip_toolmsg of
            Left Skipped -> liftE $ throwE "Skipped"
            Right toolmsg -> do
              $(logInfo) $ showMessage toolmsg
              modify $ \st -> st
                { chatStatusToolDepth      = chatStatusToolDepth st + 1
                , chatStatusTotalToolCalls = chatStatusTotalToolCalls st + 1
                , chatStatusMessages       = chatStatusMessages st <> [amsg, toolmsg]
                }
              ([amsg, toolmsg] <>) <$> agent

printMessage :: Message -> IO ()
printMessage = TIO.putStrLn . showMessage

showMessage :: Message -> Text
showMessage (SystemMessage c)               = "SystemMessage: "     <> c
showMessage (UserMessage c)                 = "UserMessage: "       <> c
showMessage (AssistantMessage c Nothing _)  = "AssistantMessage: "  <> c
showMessage (AssistantMessage c (Just t) _) = "AssistantThinking: " <> t <> "\nAssistantMessage: " <> c
showMessage (ToolMessage c tm)              = "ToolMessage: "       <> c <> "\nToolMeta: "         <> toText tm

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will only read apiKey from chat setting
statusChat :: forall md ts m. (MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatParams md ts -> ChatStatus -> m (Either Text [Message], ChatStatus)
statusChat = runReaderStateT . runExceptT . runChatT $ agent

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will read apiKey from file if no api key is found in the chat setting
statusChatReadAPIKey :: forall md ts m. (MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatParams md ts -> ChatStatus -> m (Either Text [Message], ChatStatus)
statusChatReadAPIKey params st = do
  apiKey <- either (const Nothing) Just <$> liftIO (runExceptT readApiKeyFile)
  let params' = params { chatSetting = (chatSetting params) { systemApiKeys = systemApiKeys (chatSetting params) <|> apiKey } }
  statusChat @md @ts params' st

-- | Tool calls are discarded in the output
-- system message will be appended so don't add it to the input
-- will try to read apiKey file if no api key is found in the chat setting
messageChat :: forall md ts m. (MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatParams md ts -> [Message] -> ExceptT Text m Message
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
messagesChat :: forall md ts m. (MonadLogger m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatParams md ts -> [Message] -> ExceptT Text m [Message]
messagesChat params prevMsg
  | Just _ <- systemApiKeys (chatSetting params) = ExceptT $ fmap fst $ statusChat @md params (ChatStatus 0 0 prevMsg)
  | otherwise = do
      apiKey <- readApiKeyFile
      let params' = params { chatSetting = (chatSetting params) { systemApiKeys = Just apiKey } }
      ExceptT $ fmap fst $ statusChat @md @ts params' (ChatStatus 0 0 prevMsg)

data Skipped = Skipped
-- | Handle tool execution, no recursion
handleToolCall :: forall md ts m. (ConstraintList (ToolClass m) ts, MonadIO m) => Text -> Value -> ToolMeta -> ChatT md ts m (Either Skipped Message)
handleToolCall toolCallName args md = do
  -- Find matching tool
  eOutput <-
    case pickConstraint (Proxy @(ToolClass m)) (Proxy @ts) ((== toolCallName) . toolName (Proxy @m)) of
      Just toolCont -> toolCont $ \tool -> do
        toolOutput <- lift $ runExceptT $ do
          input  <- ExceptT $ pure (jsonToInput (Proxy @m) tool args          )
          toolHandlerTextError (Proxy @m) tool input
        case toolOutput of
          Left "Tool Returned Error: SkipOutput" -> return $ Left Skipped
          _ -> return $ Right $ wrapToolOutput toolOutput
        -- Execute tool, tool error is caught and wrapped for agent to handle
      Nothing       -> liftE . throwE $ "Unknown tool: " <> toolCallName

  case eOutput of
    Left Skipped -> return $ Left Skipped
    Right output -> do
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
      return $ Right toolMsg
  where wrapToolOutput (Right t) = A.object ["tool_output" .= t]
        wrapToolOutput (Left e)  = A.object ["tool_error"  .= e]
