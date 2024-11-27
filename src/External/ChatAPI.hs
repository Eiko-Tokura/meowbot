{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module External.ChatAPI
  (
    simpleChat, Message(..), messageChat, ChatModel(..), ChatParams(..), ChatSetting(..), chatSettingMaybeWrapper
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Trans.Except
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.Bifunctor
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Internal as BLI
import GHC.Generics (Generic)
import Control.DeepSeq

data ChatModel = GPT3 | GPT4 deriving (Show, Eq)
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
  temperature :: Double
} deriving (Show, Generic)

instance ToJSON Message
instance ToJSON ChatRequest

promptMessage :: String -> Message
promptMessage prompt = Message "user" (pack prompt)

generateRequestBody :: ChatParams -> [Message] -> ByteString
generateRequestBody (ChatParams model md mset) mes = toStrict $ encode $
  ChatRequest strModel (sysMessage : mes) (fromMaybe 0.5 (systemTemp mset))
  where sysMessage = if md then Message
                          "system"
                          "You are a endearing catgirl assistant named '喵喵'. \
                          \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T'.  \
                          \ Always answer in markdown format, put your formulas in latex form enclosed by $ or $$. Do not use \\( \\) or \\[ \\] for formulas."
                        else fromMaybe (Message
                          "system"
                          "You are the endearing catgirl assistant named '喵喵'. You adore using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]'."
                          ) (systemMessage mset)
        strModel = case model of
          GPT3 -> "gpt-4o-mini"
          GPT4 -> "gpt-4o"

type APIKey = Text
fetchChatCompletionResponse :: APIKey -> ChatParams -> [Message] -> IO (Either Text ChatCompletionResponse)
fetchChatCompletionResponse apiKey model msg = do
  let customTimeout = 40 * 1000000 -- 40 seconds in microseconds
  let customManagerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro customTimeout }
  manager <- newManager customManagerSettings
  request <- parseRequest "https://api.openai.com/v1/chat/completions"
  let requestBody = generateRequestBody model msg --promptMessage prompt
  let request' = request
        { method = "POST"
        , requestBody = RequestBodyBS requestBody
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> encodeUtf8 apiKey)
            ]
        }
  result <- try (httpLbs request' manager) :: IO (Either SomeException (Response BLI.ByteString))
  return $ bimap (T.pack . show) responseBody result >>= first T.pack . eitherDecode

displayResponse :: ChatCompletionResponse -> Text
displayResponse inp = let chos = choices inp in
  case chos of
    []         -> ""
    headChos:_ -> (content . message) headChos

readApiKeyFile = ExceptT . fmap (bimap ((T.concat ["Expect api key file \"", T.pack apiKeyFile, "\", while trying to read this file, the following error occured: "] <>) . T.pack . show) (T.pack . head . lines)) . try @SomeException $ readFile apiKeyFile

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
