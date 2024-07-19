{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module External.ChatAPI 
  ( 
    simpleChat, Message(..), messageChat, ChatModel(..), ChatParams(..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.Bifunctor
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Internal as BLI
import GHC.Generics (Generic)

data ChatModel = GPT3 | GPT4 deriving (Show, Eq)
data ChatParams  = ChatParams 
  { chatModel :: ChatModel
  , markDown :: Bool 
  , systemMessage :: Maybe Message
  } deriving (Show)

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
  } deriving (Generic, Eq, Ord, Read)

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
generateRequestBody (ChatParams model md msys) mes = toStrict $ encode $
  ChatRequest strModel (systemMessage : mes) 0.6
  where systemMessage = if md then Message 
                          "system"
                          "You are a endearing catgirl assistant named '喵喵'. \
                          \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T'.  \
                          \ Always answer in markdown format, put your formulas in latex form enclosed by $ or $$. Do not use \\( \\) or \\[ \\] for formulas."  
                        else fromMaybe (Message 
                          "system" 
                          "You are the endearing catgirl assistant named '喵喵'. You adore using whisker-twitching symbols such as 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]'."
                          ) msys
                          -- "You are a delightful catgirl assistant named '喵喵'. You have a penchant for using adorable symbols like 'owo', '>w<', 'qwq', 'T^T', and a special cat symbol '[CQ:face,id=307]'. When presented with a question, always start by explaining the relevant definitions and break down the query into simpler parts. Think systematically and conclude your response with a concise answer. If not presented with a question, prioritize being endearingly cute."
                          --
                          -- \ When asked by a question, don't give answer first, give the definitions involved and break down a complex problem into simple pieces. \
                          -- \ think step by step, conclude your answer at the end. \
                          -- "You are a cute catgirl assistant named '喵喵'. \
                          -- \ You love to use cute symbols like 'owo', '>w<', 'qwq', 'T^T', you also love to use a cute symbol '[CQ:face,id=307]', which shows a cat with extended paws. \
                          -- \ Sometimes you add 'meow~' or '喵~' to the end of a sentence. \
                          -- \ When asked by a question, don't give answer first, give the definitions involved and break down a complex problem into simple pieces, \
                          -- \ think step by step, conclude your answer at the end. \
                          -- \ When not asked by a question, just be cute."
        strModel = case model of
          GPT3 -> "gpt-4o-mini"
          GPT4 -> "gpt-4o"

type APIKey = String
fetchChatCompletionResponse :: APIKey -> ChatParams -> [Message] -> IO (Either String ChatCompletionResponse)
fetchChatCompletionResponse apiKey model msg = do
  let customTimeout = 30 * 1000000 -- 30 seconds in microseconds
  let customManagerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro customTimeout }
  manager <- newManager customManagerSettings
  request <- parseRequest "https://api.openai.com/v1/chat/completions"
  let requestBody = generateRequestBody model msg --promptMessage prompt
  let request' = request
        { method = "POST"
        , requestBody = RequestBodyBS requestBody
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> encodeUtf8 (pack apiKey))
            ]
        }
  result <- try (httpLbs request' manager) :: IO (Either SomeException (Response BLI.ByteString))
  return $ bimap show responseBody result >>= eitherDecode

displayResponse :: ChatCompletionResponse -> String
displayResponse inp = let chos = choices inp in
  case chos of
    [] -> ""
    _ -> (unpack . content . message . head) chos

simpleChat :: ChatParams -> String -> ExceptT String IO String
simpleChat model prompt = do
  apiKey <- ExceptT . fmap (bimap show (head . lines)) . try @SomeException $ readFile apiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse apiKey model [promptMessage prompt]
  return $ displayResponse result

messageChat :: ChatParams -> [Message] -> ExceptT String IO String
messageChat params prevMsg = do
  apiKey <- ExceptT . fmap (bimap show (head . lines)) . try @SomeException $ readFile apiKeyFile
  result <- ExceptT $ fetchChatCompletionResponse apiKey params prevMsg
  return $ turnResponseToMsg result
    where turnResponseToMsg res = let strRes = displayResponse res in strRes --Message "assistant" $ pack strRes
