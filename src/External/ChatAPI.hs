{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module External.ChatAPI
  ( simpleChat, Message(..), messageChat
  , ChatModel(..), OpenAIModel(..), DeepSeekModel(..)
  , ChatParams(..), ChatSetting(..), chatSettingMaybeWrapper, chatSettingAlternative
  ) where

import Control.Exception (try, SomeException)
import Control.Applicative
import Control.Monad.Trans.Except
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson as A
import Data.Bifunctor
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Control.DeepSeq
import External.ChatAPI.Function

data ChatModel = 
  OpenAI OpenAIModel | DeepSeek DeepSeekModel deriving (Show, Eq)

data OpenAIModel = GPT4oMini | GPT4o deriving (Show, Eq)
data DeepSeekModel = DeepSeekChat | DeepSeekReasoner deriving (Show, Eq)

data ChatParams  = ChatParams
  { chatModel     :: ChatModel
  , markDown      :: Bool
  , chatSetting   :: ChatSetting
  } deriving (Show)

data ChatSetting = ChatSetting
  { systemMessage :: Maybe Message
  , systemTemp    :: Maybe Double
  } deriving (Show, Eq, Read, Generic, NFData)

chatSettingMaybeWrapper :: Maybe ChatSetting -> ChatSetting
chatSettingMaybeWrapper = fromMaybe (ChatSetting Nothing Nothing)

chatSettingAlternative :: Maybe ChatSetting -> ChatSetting -> ChatSetting
chatSettingAlternative mnew def = ChatSetting
  { systemMessage = systemMessage def <|> (systemMessage =<< mnew)
  , systemTemp    = systemTemp    def <|> (systemTemp =<< mnew)
  }

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

data Message = Message
  { role    :: Text
  , content :: Text
  } deriving (Generic, Eq, Ord, Read, NFData)

deriving instance Show Message

instance FromJSON ChatCompletionResponse where
  parseJSON = withObject "ChatCompletionResponse" $ \v -> ChatCompletionResponse
    <$> v .: "id"       -- due to the name 'id' is a keyword in Haskell, we use 'responseId' instead
    <*> v .: "object"
    <*> v .: "created"
    <*> v .: "choices"
    <*> v .: "usage"

instance FromJSON Choice
instance FromJSON Message

apiKeyFile :: FilePath
apiKeyFile = "apiKey"

data ChatRequest = ChatRequest {
  model :: Text,
  messages :: [Message],
  temperature :: Double,
  stream :: Maybe Bool,
  tools :: Maybe [Tool]
} deriving (Show, Generic)

instance ToJSON Message
instance ToJSON ChatRequest where
  toJSON chatReq = A.object $
    [ "model" .= model chatReq
    , "messages" .= messages chatReq
    , "temperature" .= temperature chatReq
    ] 
    <> [ "stream" .= stream | Just stream <- [stream chatReq] ] 
    <> [ "tools" .= tls | Just tls <- [tools chatReq] ]


promptMessage :: String -> Message
promptMessage prompt = Message "user" (pack prompt)

generateRequestBody :: ChatParams -> [Message] -> BL.ByteString
generateRequestBody (ChatParams model md mset) mes = encode $
  ChatRequest
    { model       = modelString model
    , messages    = sysMessage : mes
    , temperature = fromMaybe 0.5 (systemTemp mset)
    , stream      = Just False
    , tools       = Nothing
    }
  where sysMessage = if md then Message
                          "system"
                          "You are a endearing catgirl assistant named '喵喵'. \
                          \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T'.  \
                          \ Always answer in markdown format, put your formulas in latex form enclosed by $ or $$. Do not use \\( \\) or \\[ \\] for formulas."
                        else fromMaybe (Message
                          "system"
                          "You are the endearing catgirl assistant named '喵喵'. You adore using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]'."
                          ) (systemMessage mset)

modelString :: ChatModel -> Text
modelString (OpenAI GPT4oMini)          = "gpt-4o-mini"
modelString (OpenAI GPT4o)              = "gpt-4o"
modelString (DeepSeek DeepSeekChat)     = "deepseek-chat"
modelString (DeepSeek DeepSeekReasoner) = "deepseek-reasoner"

modelEndpoint :: ChatModel -> String
modelEndpoint OpenAI {}     = "https://api.openai.com/v1/chat/completions"
modelEndpoint (DeepSeek {}) = "https://api.deepseek.com/chat/completions"

data APIKey = APIKey
  { apiKeyOpenAI :: Maybe Text
  , apiKeyDeepSeek :: Maybe Text
  } deriving (Show, Read)

getApiKeyByModel :: ChatModel -> APIKey -> Maybe Text
getApiKeyByModel (OpenAI _)   apiKey = apiKeyOpenAI apiKey
getApiKeyByModel (DeepSeek _) apiKey = apiKeyDeepSeek apiKey

fetchChatCompletionResponse :: APIKey -> ChatParams -> [Message] -> IO (Either Text ChatCompletionResponse)
fetchChatCompletionResponse apiKey model msg = do
  let customTimeout = 40 * 1000000 -- 40 seconds in microseconds
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

simpleChat :: ChatParams -> String -> ExceptT Text IO Text
simpleChat model prompt = do
  apiKey <- readApiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse apiKey model [promptMessage prompt]
  return $ displayResponse result

messageChat :: ChatParams -> [Message] -> ExceptT Text IO Text
messageChat params prevMsg = do
  apiKey <- readApiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse apiKey params prevMsg
  return $ turnResponseToMsg result
    where turnResponseToMsg res = let strRes = displayResponse res in strRes --Message "assistant" $ pack strRes
