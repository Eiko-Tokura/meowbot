{-# LANGUAGE DerivingVia, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module External.ChatAPI.Function where

import Data.Aeson
import Data.Text (Text, unpack)
import GHC.Generics
import Data.String (IsString, fromString)
import qualified Data.Map as M
import Data.Time.Clock

import Control.Monad.Reader
import Control.Monad.State

data Tool = Tool
  { toolType  :: Text
  , functions :: [Function]
  } deriving (Show)

data Function = Function
  { name        :: Text
  , description :: Text
  , parameters  :: [Parameter]
  } deriving (Show, Generic, ToJSON)

data Parameter = Parameter
  { paramType  :: Text
  , properties :: Properties
  , required   :: [PropertyName]
  , additionalProperties :: Bool
  } deriving (Show, Generic)

newtype Properties = Properties [Property] deriving Show

data Property = Property
  { propName  :: PropertyName
  , propType  :: PropertyType
  , propItems :: Maybe PropertyType
  , propDescription :: Text
  } deriving Show

newtype PropertyName = PropertyName { unPropertyName :: Text } deriving (Show, ToJSON, FromJSON, IsString) via Text
data PropertyType = StringType | NumberType | ObjectType | ArrayType deriving Show

instance ToJSON Tool where
  toJSON t = object
    [ "type"      .= toolType t
    , "functions" .= functions t
    ]

instance ToJSON Parameter where
  toJSON p = object
    [ "type"                 .= paramType p
    , "properties"           .= properties p
    , "required"             .= required p
    , "additionalProperties" .= additionalProperties p
    ]

instance ToJSON Properties where
  toJSON (Properties ps) = object
    [ (fromString $ unpack $ unPropertyName $ propName p) .= object
      [ "type" .= propType p
      , "items" .= object
        [ "type" .= propItems p
        ]
      , "description" .= propDescription p
      ]
    | p <- ps
    ]

instance ToJSON PropertyType where
  toJSON StringType = "string"
  toJSON NumberType = "number"
  toJSON ObjectType = "object"
  toJSON ArrayType  = "array"

-- function call example:
--
-- 1. providing tools
--tools = [
--    {
--        "type": "function",
--        "function": {
--            "name": "get_delivery_date",
--            "description": "Get the delivery date for a customer's order. Call this whenever you need to know the delivery date, for example when a customer asks 'Where is my package'",
--            "parameters": {
--                "type": "object",
--                "properties": {
--                    "order_id": {
--                        "type": "string",
--                        "description": "The customer's order ID."
--                    }
--                },
--                "required": ["order_id"],
--                "additionalProperties": False
--            }
--        }
--    }
--]
--
-- 2. assistant calls the function
--Choice(
--    finish_reason='tool_calls',
--    index=0,
--    logprobs=None,
--    message=chat.completionsMessage(
--        content=None,
--        role='assistant',
--        function_call=None,
--        tool_calls=[
--            chat.completionsMessageToolCall(
--                id='call_62136354',
--                function=Function(
--                    arguments='{"order_id":"order_12345"}',
--                    name='get_delivery_date'),
--                type='function')
--        ])
--)
--
-- 3. returning the result
-- # Create a message containing the result of the function call
-- function_call_result_message = {
--     "role": "tool",
--     "content": json.dumps({
--         "order_id": order_id,
--         "delivery_date": delivery_date.strftime('%Y-%m-%d %H:%M:%S')
--     }),
--     "tool_call_id": response['choices'][0]['message']['tool_calls'][0]['id']
-- }
--
-- # Prepare the chat completion call payload
--
-- completion_payload = {
--     "model": "gpt-4o",
--     "messages": [
--         {"role": "system", "content": "You are a helpful customer support assistant. Use the supplied tools to assist the user."},
--         {"role": "user", "content": "Hi, can you tell me the delivery date for my order?"},
--         {"role": "assistant", "content": "Hi there! I can help with that. Can you please provide your order ID?"},
--         {"role": "user", "content": "i think it is order_12345"},
--         response['choices'][0]['message'],
--         function_call_result_message
--     ]
-- }


-- potential tools:
--
-- ## Note taking for assistant
-- creating notes, a note will have a title and contents
--
-- each time assistant is called, the list of notes will be displayed to the assistant
--
-- reading notes, assistant can call the notes by id to decide what to do next
--
-- deleting notes, assistant can delete notes by id
--   if a note is not used (read) for a long time, it will be deleted automatically
--
-- editing notes, assistant can edit notes by id
--
-- ## Time management for assistant
--
-- adding timer, assistant can add a timer with a title and a trigger time
--
-- ## Web browsing for assistant
--
-- wget, assistant can download a file from the internet
--
-- curl, assistant can use curl to form a request to the internet

-- | For each id, there is a list of notes
-- data Assistant id context = Assistant
--   { assistantState :: AssistantState id context
--   , tools  :: [Tool]
--   } deriving (Show)
-- canonical use case is MeowMeow a = AssistantT ChatId [Message] Meow a
newtype AssistantT id context m a
  = AssistantT { runAssistant :: ReaderT [Tool] (StateT (AssistantState id context) m) a }
  deriving (Functor, Applicative, Monad, MonadReader [Tool], MonadState (AssistantState id context)) via ReaderT [Tool] (StateT (AssistantState id context) m)

instance MonadTrans (AssistantT id context) where
  lift = AssistantT . lift . lift
  {-# INLINE lift #-}

data AssistantState id context = AssistantState
  { notes  :: !(M.Map id [Note])
  , timers :: !(M.Map id [Timer context])
  } deriving (Show)

data Timer context = Timer
  { timerTitle   :: Text
  , triggerTime  :: UTCTime
  , timerContext :: context
  } deriving (Show, Read)

data Note = Note
  { noteTitle   :: Text
  , noteTime    :: UTCTime
  , noteContent :: Text
  } deriving (Show, Read)

