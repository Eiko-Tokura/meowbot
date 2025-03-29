-- | This module converts two types of CQMessages,
-- converting raw style to array style, and vice versa.
module MeowBot.CQMessage.Convert where

import qualified Data.Vector as V
import Data.Maybe
import Data.Aeson
import Data.Aeson.KeyMap as KM
import MeowBot.Data
import MeowBot.CQCode
import MeowBot.Parser

-- string message:
--  {"self_id":3055323571,"user_id":754829466,"time":1742741328,"message_id":1267259583,"message_seq":118,"message_type":"private","sender":{"user_id":754829466,"nickname":"Eiko","card":""},"raw_message":"!info","font":14,"sub_type":"friend","message":"!info","message_format":"string","post_type":"message"}
--
-- {"self_id":3055323571,"user_id":754829466,"time":1742742510,"message_id":1593923941,"message_seq":119,"message_type":"private","sender":{"user_id":754829466,"nickname":"Eiko","card":""},"raw_message":"!info &#91;CQ:qq=123,name=&#91;&#93;","font":14,"sub_type":"friend","message":"!info &#91;CQ:qq=123,name=&#91;&#93;","message_format":"string","post_type":"message"}
--
-- array message:
-- {"self_id":3055323571,"user_id":754829466,"time":1742741328,"message_id":1267259583,"message_seq":118,"message_type":"private","sender":{"user_id":754829466,"nickname":"Eiko","card":""},"raw_message":"!info","font":14,"sub_type":"friend","message":[{"type":"text", "data":{"text":"!info"}}],"message_format":"array","post_type":"message"}

data CQMessageObject
  = CQMessageString Object
  | CQMessageArray  Object
  deriving (Show)

newtype ArrayMessage = ArrayMessage { unArrayMessage :: [Either CQCode Text] }

instance ToJSON ArrayMessage where
  toJSON = Array . V.fromList . fmap f . unArrayMessage
    where
      f (Left cqcode) = toJSON cqcode
      f (Right text)  = object ["type" .= ("text" :: Text), "data" .= object ["text" .= text]]

instance FromJSON CQMessageObject where
  parseJSON = withObject "CQMessageObject" $ \o -> do
    messageFormat :: Text <- o .: "message_format"
    case messageFormat of
      "string" -> pure $ CQMessageString o
      "array"  -> pure $ CQMessageArray  o
      _        -> fail "Unknown message format"

instance ToJSON CQMessageObject where
  toJSON (CQMessageString o) = Object o
  toJSON (CQMessageArray  o) = Object o

stringToArray :: CQMessageObject -> CQMessageObject
stringToArray (CQMessageArray o)  = CQMessageArray o
stringToArray (CQMessageString o) = CQMessageArray
  $ insert "message_format" (String "array")
  $ insert "message" (toJSON arrayMsg)
  $ o
  where
    arrayMsg :: ArrayMessage
    arrayMsg = ArrayMessage
      $ stringMsgToArrayMsgFunction
      $ fromMaybe ""
      $ resultToMaybe
      $ fromJSON @Text
      $ fromMaybe Null
      $ KM.lookup "message" o

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing
