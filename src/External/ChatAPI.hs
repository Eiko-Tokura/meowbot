{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia, RecordWildCards #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, TypeFamilies #-}

module External.ChatAPI
  ( simpleChat, Message(..), mapMessage, statusChat, statusChatDefault, messageChat, messageChatDefault, messagesChat, messagesChatDefault
  , statusChatReadAPIKey, statusChatReadAPIKeyDefault
  , ChatModel(..)
  , OpenAIModel(..), DeepSeekModel(..), LocalModel(..)
  , OpenRouterModel(..), SiliconFlowModel(..), AnthropicModel(..), XcApiModel(..)
  , ChatParams(..), ChatSetting(..), ChatStatus(..), chatSettingAlternative, chatSettingMaybeWrapper
  , ChatAPI(..), APIKey(..)
  , EstimateTokens(..)
  , printMessage
  , findApiKey
  ) where


import Control.Applicative
import Control.DeepSeq
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Effect
import Control.Monad.Logger
import Data.Aeson as A
import Data.Bifunctor
import Data.Default
import Data.HList
import Data.Time
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.List.NonEmpty (NonEmpty(..))
import External.ChatAPI.Tool
import External.ChatAPI.Cost
import External.ChatAPI.Models
import External.ChatAPI.ModelPricing
import GHC.Generics (Generic)
import Network.HTTP.Client hiding (Proxy)
import Module.RS
import Module.Logging

import Parser.Run

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Utils.TokenEstimator (estimateTokens)
import Utils.Text
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

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
  } deriving (Show, Read, Eq, Generic, NFData)

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

instance ChatAPI (OpenAI O1Mini) where
  chatModel  = OpenAI O1Mini
  type ChatCompletionResponse (OpenAI O1Mini) = ChatCompletionResponseOpenAI

instance ChatAPI (DeepSeek DeepSeekChat) where
  chatModel  = DeepSeek DeepSeekChat
  type ChatCompletionResponse (DeepSeek DeepSeekChat) = ChatCompletionResponseOpenAI

instance ChatAPI (DeepSeek DeepSeekReasoner) where
  chatModel  = DeepSeek DeepSeekReasoner
  type ChatCompletionResponse (DeepSeek DeepSeekReasoner) = ChatCompletionResponseOpenAI

instance ChatAPI (Local DummyTestModel) where
  chatModel  = Local DummyTestModel
  type ChatCompletionResponse (Local DummyTestModel) = ChatCompletionResponseOpenAI

instance ChatAPI (Local Qwen2_5_32B) where
  chatModel  = Local Qwen2_5_32B
  type ChatCompletionResponse (Local Qwen2_5_32B) = ChatCompletionResponseOllama

instance ChatAPI (Local Qwen3_30B) where
  chatModel  = Local Qwen3_30B
  type ChatCompletionResponse (Local Qwen3_30B) = ChatCompletionResponseOllama

instance ChatAPI (Local Command_R_Latest) where
  chatModel  = Local Command_R_Latest
  type ChatCompletionResponse (Local Command_R_Latest) = ChatCompletionResponseOllama

instance ChatAPI (Local QwQ) where
  chatModel  = Local QwQ
  type ChatCompletionResponse (Local QwQ) = ChatCompletionResponseOllama

instance ChatAPI (OpenRouter OR_DeepSeekR1_Free) where
  chatModel  = OpenRouter OR_DeepSeekR1_Free
  type ChatCompletionResponse (OpenRouter OR_DeepSeekR1_Free) = ChatCompletionResponseOpenAI

instance ChatAPI (OpenRouter OR_DeepSeekV3_Free) where
  chatModel  = OpenRouter OR_DeepSeekV3_Free
  type ChatCompletionResponse (OpenRouter OR_DeepSeekV3_Free) = ChatCompletionResponseOpenAI

instance ChatAPI (SiliconFlow SF_DeepSeekV3) where
  chatModel  = SiliconFlow SF_DeepSeekV3
  type ChatCompletionResponse (SiliconFlow SF_DeepSeekV3) = ChatCompletionResponseOpenAI

instance ChatAPI (SiliconFlow SF_DeepSeekR1) where
  chatModel  = SiliconFlow SF_DeepSeekR1
  type ChatCompletionResponse (SiliconFlow SF_DeepSeekR1) = ChatCompletionResponseOpenAI

instance ChatAPI (XcApi XC_Claude_3_5) where
  chatModel  = XcApi XC_Claude_3_5
  type ChatCompletionResponse (XcApi XC_Claude_3_5) = ChatCompletionResponseOpenAI

instance ChatAPI (XcApi XC_Claude_3_7) where
  chatModel  = XcApi XC_Claude_3_7
  type ChatCompletionResponse (XcApi XC_Claude_3_7) = ChatCompletionResponseOpenAI

data ChatCompletionResponseOpenAI = ChatCompletionResponseOpenAI
  { responseId :: Text
  , object     :: Text
  , created    :: Int
  , choices    :: [Choice]
  , usage      :: Maybe Object
  } deriving (Show)

data Choice = Choice
  { index         :: Int
  , message       :: Message
  , finish_reason :: Maybe Text
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
  | AssistantMessage { content :: Text, thinking :: Maybe Text, pureToolCall :: Maybe Bool, withToolCall :: Maybe (NonEmpty ToolText, (ToolCallPair -> Text) -> Text) }
  | ToolMessage      { content :: Text, toolMeta :: Maybe ToolMeta }
  deriving (Eq, Read, Generic, NFData)

mapMessage :: (Text -> Text) -> Message -> Message
mapMessage f (SystemMessage    c)        = SystemMessage    $ f c
mapMessage f (UserMessage      c)        = UserMessage      $ f c
mapMessage f (AssistantMessage c t p wt) = AssistantMessage (f c) (f <$> t) p (second (f .) <$> wt)
mapMessage f (ToolMessage      c m)      = ToolMessage      (f c) m

-- | Removes the thinking part of the assistant message
truncateThinking :: Message -> Message
truncateThinking (AssistantMessage c (Just _) p wt) = AssistantMessage c Nothing p wt
truncateThinking m = m

-- | Wrapper for model dependent content (toJSON and FromJSON)
newtype ModelDependent m a = ModelDependent { runModelDependent :: a }
  deriving (Show, Eq, Generic)
  deriving newtype (NFData)

deriving instance Show Message

instance FromJSON ChatCompletionResponseOpenAI where
  parseJSON = withObject "ChatCompletionResponse" $ \v -> ChatCompletionResponseOpenAI
    <$> v .:  "id"       -- due to the name 'id' is a keyword in Haskell, we use 'responseId' instead
    <*> v .:  "object"
    <*> v .:  "created"
    <*> v .:  "choices"
    <*> v .:? "usage"

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
          return $ AssistantMessage ct' thinking Nothing Nothing
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
  toJSON (ModelDependent (AssistantMessage c Nothing _ _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c (Just think) _ _))
    = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>\n" <> c <> "</think>\n\n" <> think)]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text) , "content" .= c]
  -- ^ deepseek model does not support tool role, and we use user role instead

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (DeepSeek a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _ _ _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text) , "content" .= c]

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (OpenAI a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _ _ _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
  -- ^ we haven't implemented tool role for openai model yet, so we use user role instead

deriving via (ModelDependent (OpenAI a) Message) instance ToJSON (ModelDependent (OpenRouter b) Message)
deriving via (ModelDependent (OpenAI a) Message) instance ToJSON (ModelDependent (SiliconFlow b) Message)
deriving via (ModelDependent (OpenAI a) Message) instance ToJSON (ModelDependent (XcApi b) Message)

instance {-# OVERLAPPABLE #-} ToJSON (ModelDependent (Local a) Message) where
  toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)   , "content" .= c]
  toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)     , "content" .= c]
  toJSON (ModelDependent (AssistantMessage c _ _ _)) = A.object ["role" .= ("assistant" :: Text), "content" .= c]
  toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text), "content" .= c]
  -- ^ we haven't implemented tool role for ollama api yet, so we use user role instead

-- instance ToJSON (ModelDependent (Local DeepSeekR1_32B) Message) where
--   toJSON (ModelDependent (SystemMessage    c)  ) = A.object ["role" .= ("system" :: Text)    , "content" .= c]
--   toJSON (ModelDependent (UserMessage      c)  ) = A.object ["role" .= ("user" :: Text)      , "content" .= c]
--   toJSON (ModelDependent (AssistantMessage c Nothing _ _)) = A.object ["role" .= ("assistant" :: Text) , "content" .= c]
--   toJSON (ModelDependent (AssistantMessage c (Just think) _ _))
--     = A.object ["role" .= ("assistant" :: Text) , "content" .= ("<think>\n" <> c <> "</think>\n\n" <> think)]
--   toJSON (ModelDependent (ToolMessage      c _)) = A.object ["role" .= ("user" :: Text) , "content" .= c]
--   -- ^ deepseek r1:32b model does not support tool role, and we use user role instead
-- 
-- deriving via (ModelDependent (Local DeepSeekR1_32B) Message) instance ToJSON (ModelDependent (Local DeepSeekR1_14B) Message)

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
deriving via (ModelDependent (OpenAI a) ChatRequest) instance ToJSON (ModelDependent (XcApi b) ChatRequest)

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
    , "options" .= A.object
        ["temperature" .= temperature chatReq]
    ]
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ]

promptMessage :: String -> Message
promptMessage prompt = UserMessage (pack prompt)

generateRequestBody :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts) => ChatParams md ts -> Maybe Message -> [Message] -> BL.ByteString
generateRequestBody param mSys mes = encode $ ModelDependent @md $
  ChatRequest
    { chatReqModel = chatModel @md
    , messages     = maybe mes (: mes) mSys
    , temperature  = fromMaybe 0.5 (systemTemp $ chatSetting param)
    , stream       = case chatModel @md of
        DeepSeek _    -> Just False
        OpenAI _      -> Nothing
        Local _       -> Just False
        OpenRouter _  -> Just False
        SiliconFlow _ -> Just False
        Anthropic _   -> Just False
        XcApi _       -> Just False
    -- , tools       = Nothing
    }

estimateInputTokens :: Maybe Message -> [Message] -> Int
estimateInputTokens mSys mes
  = maybe 0 (estimateTokens . content) mSys
  + sum (estimateTokens . content <$> mes)

estimateOutputTokens :: Message -> Int
estimateOutputTokens (AssistantMessage c t _ _) = estimateTokens c + maybe 0 estimateTokens t
estimateOutputTokens _ = 0

data GenerateSystemPromptBehavior = GenerateSystemPromptBehavior
  { staticToolPrompt                   :: Bool
  , useDefaultSystemMessageWhenNothing :: Bool
    -- ^ if True, use default system message when no system message is provided
    -- otherwise, the system message will be omitted, i.e. no system message will be generated
  }
instance Default GenerateSystemPromptBehavior where
  def = GenerateSystemPromptBehavior
    { staticToolPrompt                   = False
    , useDefaultSystemMessageWhenNothing = True
    }

-- | Reads system message from the chat setting and generates a system prompt based on tool list
generateSystemPrompt
  :: forall md ts m. (Monad m, ConstraintList (ToolClass m) ts)
  => GenerateSystemPromptBehavior -> ChatParams md ts -> m (Maybe Message)
generateSystemPrompt GenerateSystemPromptBehavior{..} params = do

  toolPrompts <-
    if staticToolPrompt
    then pure $ computeToolPrompts    @m (Proxy @ts) "\n---\n"
    else computeToolPromptsWithEnable @m (Proxy @ts) "\n---\n"

  if chatMarkDown params
  then
      pure $ Just $ SystemMessage $
        "You are a endearing catgirl assistant named '喵喵'. \
        \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T'. \
        \ Always answer in markdown format, put your formulas in latex form enclosed by $ or $$. \
        \ Do not use \\( \\) or \\[ \\] for formulas."
        <> toolPrompts
  else
      pure $ maybe
        ( if useDefaultSystemMessageWhenNothing
          then Just $ SystemMessage $
                 "You are the helpful endearing catgirl assistant named '喵喵'. You adores using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]' (no space after ',')."
                 <> toolPrompts
          else Nothing
        )
        (Just . SystemMessage . (<> toolPrompts) . content)
        (systemMessage $ chatSetting params)

data APIKey = APIKey
  { apiKeyOpenAI      :: Maybe Text
  , apiKeyDeepSeek    :: Maybe Text
  , apiKeyOpenRouter  :: Maybe Text
  , apiKeySiliconFlow :: Maybe Text
  , apiKeyAnthropic   :: Maybe Text
  , apiKeyXcApi       :: Maybe Text
  } deriving (Show, Read, Eq, Generic, NFData)

findApiKey :: ChatModel -> ChatSetting -> Maybe Text
findApiKey (OpenAI _)      setting = apiKeyOpenAI      =<< systemApiKeys setting
findApiKey (DeepSeek _)    setting = apiKeyDeepSeek    =<< systemApiKeys setting
findApiKey (Local _)       _       = Just "LOCAL_NO_API_KEY_NEEDED"
findApiKey (OpenRouter _)  setting = apiKeyOpenRouter  =<< systemApiKeys setting
findApiKey (SiliconFlow _) setting = apiKeySiliconFlow =<< systemApiKeys setting
findApiKey (Anthropic _)   setting = apiKeyAnthropic   =<< systemApiKeys setting
findApiKey (XcApi _)       setting = apiKeyXcApi       =<< systemApiKeys setting

data GetAPIKey
  = NoAPIKeyRequired
  | APIKeyRequired (Maybe Text)

getApiKeyByModel :: ChatModel -> APIKey -> GetAPIKey
getApiKeyByModel (OpenAI _)     apiKey  = APIKeyRequired $ apiKeyOpenAI apiKey
getApiKeyByModel (DeepSeek _)   apiKey  = APIKeyRequired $ apiKeyDeepSeek apiKey
getApiKeyByModel (Local _)    _         = NoAPIKeyRequired
getApiKeyByModel (OpenRouter _) apiKey  = APIKeyRequired $ apiKeyOpenRouter apiKey
getApiKeyByModel (SiliconFlow _) apiKey = APIKeyRequired $ apiKeySiliconFlow apiKey
getApiKeyByModel (Anthropic _) apiKey   = APIKeyRequired $ apiKeyAnthropic apiKey
getApiKeyByModel (XcApi _) apiKey       = APIKeyRequired $ apiKeyXcApi apiKey

fetchChatCompletionResponse :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => Manager -> Int -> APIKey -> ChatParams md ts -> Maybe Message -> [Message] -> EffT '[ModelPricing, LoggingModule] '[ErrorText "chat-error"] m (ChatCompletionResponse md, EstimateTokens)
fetchChatCompletionResponse manager _ allApiKey model mSys msg = do
  request <- liftIO $ parseRequest (modelEndpoint $ chatModel @md)
  utcTime <- liftIO getCurrentTime
  price <- asksModule modelPricing
  let tokenPrice  = price (chatModel @md) utcTime
      requestBody = generateRequestBody @md @ts @m model mSys msg --promptMessage prompt
  $(logInfo) $ T.intercalate "\n" $
    [ ""
    , "model: " <> T.pack (show $ chatModel @md)
      <> " , endpoint: " <> T.pack (modelEndpoint $ chatModel @md)
      <> " , estimated input tokens: " <> T.pack (show $ estimateInputTokens mSys msg)
      <> " , estimated input cost: " <> T.pack (show $ tokenCost <$> tokenPrice <*> pure (TokenConsumption (estimateInputTokens mSys msg) 0 (Just meowBotCacheHitRate)))
    , "------------------- Message Start -------------------" ]
    <> map showMessage msg <>
    [ "------------------- Message End ---------------------" ]

  case getApiKeyByModel (chatModel @md) allApiKey of
    APIKeyRequired Nothing       -> effThrow $ errorText @"chat-error" $ "No API key found for the model " <> T.pack (show $ chatModel @md)
    APIKeyRequired (Just apiKey') -> do
      let request' = request
            { method = "POST"
            , requestBody = RequestBodyBS (BL.toStrict requestBody)
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Authorization", "Bearer " <> encodeUtf8 apiKey')
                ]
            }
          apiInfo = APIInfo apiKey' tokenPrice (chatModel @md)
      result <- liftIO $ do
        mres <- try @SomeException $ httpLbs request' manager
        case mres of
          Right res -> return $ Right res
          Left e -> return $ Left $ "Exception In Getting Response: " <> toText e
      baseEitherInWith (errorText @"chat-error") $ handleReturnRawResult apiInfo result
    NoAPIKeyRequired -> do
      let request' = request
            { method = "POST"
            , requestBody = RequestBodyBS (BL.toStrict requestBody)
            , requestHeaders =
                [ ("Content-Type", "application/json")
                ]
            }
          apiInfo = APIInfo "LOCAL_NO_API_KEY_NEEDED" tokenPrice (chatModel @md)
      result <- liftIO $ do
        mres <- try @SomeException $ httpLbs request' manager
        case mres of
          Right res -> return $ Right res
          Left e -> return $ Left $ "Exception In Getting Response: " <> toText e
      baseEitherInWith (errorText @"chat-error") $ handleReturnRawResult apiInfo result
    where handleReturnRawResult info res = do
            let response = second responseBody res >>= \resBody ->
                  case eitherDecode resBody of
                    Right parsedJson -> Right parsedJson
                    Left  jsonError -> Left $ "Exception when parsing: " <> T.pack jsonError <> "\nResponse body: " <> TL.toStrict (TLE.decodeUtf8 resBody)
            return $ (\resp -> (resp, EstimateTokens
              { inputTokens  = estimateInputTokens mSys msg
              , outputTokens = either (const 0) estimateOutputTokens (getMessage resp)
              , apiCalls     = 1
              , apiErrors    = case res of
                  Left _  -> 1
                  Right _ -> 0
              , apiSkips     = 0
              , apiInfo      = Just info
              })) <$> response

instance GetMessage ChatCompletionResponseOpenAI where
  getMessage = fmap message . maybe (Left "No choices in response") Right . listToMaybe . choices

instance GetMessage ChatCompletionResponseOllama where
  getMessage = return . ollamaResponse

readApiKeyFile :: MonadIO m => EffT '[] '[ErrorText "read-api-key-error"] m APIKey
readApiKeyFile =
  liftIOSafeWith
    (\(e :: SomeException) -> errorText @"read-api-key-error"
      $ "Exception when reading api key file " <> T.pack apiKeyFile <> ": " <> T.pack (show e)
    )
    $ read <$> readFile apiKeyFile

simpleChat
  :: (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m)
  => Manager -> Int -> ChatParams md ts -> String
  -> EffT '[ModelPricing, LoggingModule] '[ErrorText "read-api-key-error", ErrorText "chat-error"] m (Text, EstimateTokens)
simpleChat man timeOut model prompt = do
  apiKey <- embedEffT readApiKeyFile
  sysMsg <- lift $ generateSystemPrompt def model
  (result, usage) <- embedEffT $ fetchChatCompletionResponse man timeOut apiKey model sysMsg [promptMessage prompt]
  return (either (const "") content (getMessage result), usage)

data ChatStatus = ChatStatus
  { chatStatusToolDepth      :: !Int
  , chatStatusTotalToolCalls :: !Int
  , chatStatusMessages       :: ![Message]
  , chatEstimateTokens       :: !EstimateTokens
  } deriving (Show, Eq, Generic, NFData)

type ChatT md tools m = EffT '[RModule (ChatParams md tools), SModule ChatStatus, ModelPricing, LoggingModule] '[ErrorText "chat-error"] m

-- | Enhanced message chat with tool handling
agent :: forall md ts m. (MonadIO m, ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatT md ts m [Message]
agent = do
  params    <- askR @(ChatParams md ts)
  trunThink <- asksR @(ChatParams md ts) (fromMaybe True . chatTruncateThinking . chatSetting)
  prevMsgs  <- (if trunThink then map truncateThinking else id) <$> getsS chatStatusMessages
  toolDepth <- getsS chatStatusToolDepth
  mapiKeys  <- asksR @(ChatParams md ts) (systemApiKeys . chatSetting)
  mSys      <- lift $ generateSystemPrompt def params
  apiKeys   <- pureMaybeInWith @(ErrorText "chat-error") "No API key found!" mapiKeys
  let man     = chatManager params
      timeOut = chatTimeout params
  if fromMaybe defaultSystemMaxToolDepth (systemMaxToolDepth (chatSetting params)) <= toolDepth
  then effThrow @(ErrorText "chat-error") "Maximum tool depth exceeded"
  else do
    let getAmsg = do
          (response, usage) <- embedMods $ fetchChatCompletionResponse man timeOut apiKeys params mSys prevMsgs
          amsg <- pureEitherInWith (errorText @"chat-error") $ getMessage response
          if T.null (content amsg)
          then return Nothing
          else return $ Just (amsg, usage)

    mAmsg' <- getAmsg
    (amsg', usage) <- pureMaybeInWith @(ErrorText "chat-error") "No assistant message found!" mAmsg'
    modifyS $ \st -> st { chatEstimateTokens = chatEstimateTokens st <> usage }
    $(logInfo) $ showMessage amsg'
    case parseManyToolCalls (content amsg') of
      Nothing -> return [amsg']
      Just (_toolText , Nothing   , toolCalls) -> continueAgent True  Nothing amsg' toolCalls
      Just (toolTexts , Just holed, toolCalls) -> continueAgent False (Just (toolTexts, holed)) amsg' toolCalls
      where continueAgent isPureToolCall withToolCall amsg' toolCalls = do
              let amsg = case amsg' of
                    AssistantMessage {} -> amsg'
                      { pureToolCall = Just isPureToolCall
                      , withToolCall = withToolCall
                      }
                    _ -> amsg'
              skip_toolmsgs :: NonEmpty (Either Skipped Message) <- forM toolCalls $ \(ToolCallPair toolName args) -> handleToolCall toolName args (ToolMeta "tool_call_id" toolName 0)
              if Left Skipped `elem` skip_toolmsgs
              then do
                modifyS $ \st -> st { chatEstimateTokens = st.chatEstimateTokens {apiSkips = st.chatEstimateTokens.apiSkips + 1 }}
                effThrow @(ErrorText "chat-error") "Skipped"
              else do
                toolMsgs <- forM skip_toolmsgs $ \case
                  Left Skipped -> do
                    effThrow @(ErrorText "chat-error") "Skipped"
                  Right toolmsg -> do
                    $(logInfo) $ showMessage toolmsg
                    modifyS $ \st -> st
                      { chatStatusToolDepth      = chatStatusToolDepth st + 1
                      , chatStatusTotalToolCalls = chatStatusTotalToolCalls st + 1
                      --, chatStatusMessages       = chatStatusMessages st <> [amsg, toolmsg]
                      }
                    return toolmsg
                modifyS $ \st -> st { chatStatusMessages = chatStatusMessages st <> (amsg : NE.toList toolMsgs) }
                ((amsg : NE.toList toolMsgs) <>) <$> agent
                    -- ([amsg, toolmsg] <>) <$> agent
              -- case skip_toolmsg of
              --   Left Skipped -> do
              --     modifyS $ \st -> st { chatEstimateTokens = st.chatEstimateTokens {apiSkips = st.chatEstimateTokens.apiSkips + 1 }}
              --     effThrow @(ErrorText "chat-error") "Skipped"
              --   Right toolmsg -> do
              --     $(logInfo) $ showMessage toolmsg
              --     modifyS $ \st -> st
              --       { chatStatusToolDepth      = chatStatusToolDepth st + 1
              --       , chatStatusTotalToolCalls = chatStatusTotalToolCalls st + 1
              --       , chatStatusMessages       = chatStatusMessages st <> [amsg, toolmsg]
              --       }
              --     ([amsg, toolmsg] <>) <$> agent

printMessage :: Message -> IO ()
printMessage = TIO.putStrLn . showMessage

showMessage :: Message -> Text
showMessage (SystemMessage c)               = "SystemMessage: "     <> c
showMessage (UserMessage c)                 = "UserMessage: "       <> c
showMessage (AssistantMessage c Nothing _ _)  = "AssistantMessage: "  <> c
showMessage (AssistantMessage c (Just t) _ _) = "AssistantThinking: " <> t <> "\nAssistantMessage: " <> c
showMessage (ToolMessage c tm)              = "ToolMessage: "       <> c <> "\nToolMeta: "         <> toText tm

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will only read apiKey from chat setting
statusChat :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m)
  => ChatParams md ts -> EffT '[SModule ChatStatus, ModelPricing, LoggingModule] '[ErrorText "chat-error"] m [Message]
statusChat r = runRModule r agent

statusChatDefault :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m)
  => ChatParams md ts -> [Message]
  -> EffT
      '[ModelPricing, LoggingModule]
      NoError
      m
      (Either Text [Message], ChatStatus)
statusChatDefault params prevMsg
  = fmap (first $ first toText)
  . runSModule (ChatStatus 0 0 prevMsg mempty)
  . errorToEitherAll
  $ statusChat @md @ts params

-- | If using this you will need to maintain the chat status
-- the first system message should not be included in the input, as it will be calculated and appended using params
-- will read apiKey from file if no api key is found in the chat setting
statusChatReadAPIKey :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatParams md ts -> EffT '[SModule ChatStatus, ModelPricing, LoggingModule] '[ErrorText "chat-error"] m [Message]
statusChatReadAPIKey params = do
  apiKey <- embedEffT . errorToMaybe $ readApiKeyFile
  let params' = params { chatSetting = (chatSetting params) { systemApiKeys = systemApiKeys (chatSetting params) <|> apiKey } }
  statusChat @md @ts params'

statusChatReadAPIKeyDefault :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m)
  => ChatParams md ts -> [Message]
  -> EffT
      '[ModelPricing, LoggingModule]
      NoError
      m
      (Either Text [Message], ChatStatus)
statusChatReadAPIKeyDefault params prevMsg
  = fmap (first $ first toText)
  . runSModule (ChatStatus 0 0 prevMsg mempty)
  . errorToEitherAll
  $ statusChatReadAPIKey @md @ts params

-- | Tool calls are discarded in the output
-- system message will be appended so don't add it to the input
-- will try to read apiKey file if no api key is found in the chat setting
messageChat :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m) => ChatParams md ts -> EffT '[SModule ChatStatus, ModelPricing, LoggingModule] '[ErrorText "chat-error", ErrorText "read-api-key-error"] m Message
messageChat params --prevMsg
  | Just _ <- systemApiKeys (chatSetting params)
      = last <$> embedEffT (statusChat @md params)
  | otherwise = do
      apiKey <- embedEffT readApiKeyFile
      let params' = params { chatSetting = (chatSetting params) { systemApiKeys = Just apiKey } }
      last <$> embedEffT (statusChat @md @ts params')

messageChatDefault param msgs = (first $ second last) <$> messagesChatDefault param msgs
-- | Tool calls are also included in the output
-- system message will be appended so don't add it to the input
-- will try to read apiKey file if no api key is found in the chat setting
-- returns the list of new messages: assistant tool calls followed by the final response
messagesChat :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m)
  => ChatParams md ts
  -> EffT
      '[SModule ChatStatus, ModelPricing, LoggingModule]
      '[ErrorText "chat-error", ErrorText "read-api-key-error"]
      m
      [Message]
messagesChat params --prevMsg
  | Just _ <- systemApiKeys (chatSetting params) = embedEffT $ statusChat @md params
  | otherwise = do
      apiKey <- embedEffT readApiKeyFile
      let params' = params { chatSetting = (chatSetting params) { systemApiKeys = Just apiKey } }
      embedEffT $ statusChat @md @ts params'
      -- (ChatStatus 0 0 prevMsg mempty)
  -- where defChatStatus = ChatStatus 0 0 prevMsg mempty

messagesChatDefault :: forall md ts m. (ChatAPI md, ConstraintList (ToolClass m) ts, MonadIO m)
  => ChatParams md ts -> [Message]
  -> EffT
      '[ModelPricing, LoggingModule]
      NoError
      m
      (Either Text [Message], ChatStatus)
messagesChatDefault params prevMsg
  = fmap (first $ first toText)
  . runSModule (ChatStatus 0 0 prevMsg mempty)
  . errorToEitherAll
  $ messagesChat @md @ts params

data Skipped = Skipped deriving (Show, Eq, Generic, NFData)
-- | Handle tool execution, no recursion
handleToolCall :: forall md ts m. (ConstraintList (ToolClass m) ts, MonadIO m) => Text -> Value -> ToolMeta -> ChatT md ts m (Either Skipped Message)
handleToolCall toolCallName args md = do
  -- Find matching tool
  eOutput <-
    case pickConstraint (Proxy @(ToolClass m)) (Proxy @ts) ((== toolCallName) . toolName (Proxy @m)) of
      Just toolCont -> embedEffT . toolCont $ \tool -> do
        toolOutput <- errorToEither $ do
          input <- pureEitherInWith id $ jsonToInput (Proxy @m) tool args
          toolHandlerTextError (Proxy @m) tool input
        case toolOutput of
          Left "Tool Returned Error: SkipOutput" -> return $ Left Skipped
          _ -> return $ Right $ wrapToolOutput toolOutput
        -- Execute tool, tool error is caught and wrapped for agent to handle
      Nothing       -> effThrowIn $ (errorText @"chat-error") $ "Unknown tool: " <> toolCallName

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
