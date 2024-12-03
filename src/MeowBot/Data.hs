{-# LANGUAGE LambdaCase, DerivingStrategies, DeriveAnyClass, OverloadedStrings, DerivingVia #-}
module MeowBot.Data
  ( module MeowBot.MetaMessage
  , UserId(..), GroupId(..), ChatId(..)
  , Chat, ChatRoom
  , WholeChat--, AllData(..), OtherData(..)
  --, SavedData(..)

  , CQMessage(..), CQEventType(..)
  , Sender(..), Role(..), ResponseData(..)

  , BotName
  , BotModules(..)
  --, CommandValue
  , EssentialContent

  , Text, pack, unpack

  , showCQ, cqmsgToEssentialContent, emptyCQMessage
  ) where

import MeowBot.CommandRule
import MeowBot.MetaMessage
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), withObject, withText)
import Data.Aeson.Types (Parser, (.:?))
import Data.Additional
import qualified MeowBot.Parser as MP
import Data.Text (Text, unpack, pack)
import MeowBot.Parser (cqmsg)
import Data.Maybe

data ChatId = GroupChat GroupId | PrivateChat UserId
  deriving (Show, Eq, Ord, Read, Generic, NFData)

type Chat = [MP.Tree CQMessage]

type ChatRoom = (ChatId, Chat)

type WholeChat = [ChatRoom]  -- [(ChatId, [Tree CQMessage])]
type BotName = Maybe String

data BotModules = BotModules
  { canUseGroupCommands   :: [CommandId]
  , canUsePrivateCommands :: [CommandId]
  , nameOfBot :: BotName
  , globalSysMsg :: Maybe String
  } deriving (Show)

data CQEventType = GroupMessage | PrivateMessage | Response | HeartBeat | SelfMessage | UnknownMessage
  deriving (Show, Eq, Read, Generic, NFData)

data CQMessage = CQMessage
  { eventType    :: CQEventType
  , messageId    :: Maybe Int
  , groupId      :: Maybe GroupId
  , userId       :: Maybe UserId
  , sender       :: Maybe Sender
  , message      :: Maybe Text
  , time         :: Maybe Int
  , responseData :: Maybe ResponseData
  , echoR        :: Maybe Text
  , absoluteId   :: Maybe Int
  , metaMessage  :: Maybe MetaMessage
  } deriving (Show, Eq, Generic, NFData)

instance HasAdditionalData CQMessage where
  getAdditionalData = maybe [] additionalData . metaMessage
  modifyAdditionalData f cqmsg = cqmsg {metaMessage = modifyAdditionalData f <$> metaMessage cqmsg}

data Sender = Sender
  { senderNickname :: Maybe Text
  , senderCard     :: Maybe Text
  , senderRole     :: Maybe Role
  } deriving (Show, Read, Eq, Generic, NFData)

instance FromJSON Sender where
  parseJSON = withObject "Sender" $ \o -> do
    nickname <- o .:? "nickname"
    card     <- o .:? "card"
    role     <- o .:? "role"
    return Sender { senderNickname = nickname, senderCard = card, senderRole = role }

data Role = ROwner | RAdmin | RMember | RUnknown
  deriving (Show, Read, Eq, Generic, NFData)

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "owner" -> return ROwner
    "admin" -> return RAdmin
    "member" -> return RMember
    _ -> return RUnknown

emptyCQMessage = CQMessage UnknownMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newtype ResponseData = ResponseData
  { message_id :: Maybe Int
  } deriving (Show, Read, Eq, NFData) via (Maybe Int)
    deriving (Generic)

instance FromJSON ResponseData where
  parseJSON = withObject "ResponseData" $ \o -> do
    messageid <- o .:? "message_id"
    return ResponseData { message_id = messageid }

instance FromJSON CQMessage where
  parseJSON = withObject "CQMessage" $ \obj -> do
    postType      <- obj .:? "post_type"       :: Parser (Maybe Text)
    messageType   <- obj .:? "message_type"    :: Parser (Maybe Text)
    metaEventType <- obj .:? "meta_event_type" :: Parser (Maybe Text)
    dataObj       <- obj .:? "data"
    message       <- obj .:? "raw_message"
    strmsg        <- obj .:? "raw_message" :: Parser (Maybe Text)
    let eventType = case (postType, metaEventType, messageType, dataObj) of
          (Just "message", _,  Just "private", _) -> PrivateMessage
          (Just "message", _, Just "group", _) -> GroupMessage
          (Nothing, _, Nothing, Just _) -> Response
          (Just "meta_event", Just "heartbeat", Nothing, Nothing) -> HeartBeat
          _ -> UnknownMessage
    CQMessage <$> pure eventType
              <*> obj .:? "message_id"
              <*> obj .:? "group_id"
              <*> obj .:? "user_id"
              <*> obj .:? "sender"
              <*> pure message
              <*> obj .:? "time"
              <*> pure dataObj
              <*> obj .:? "echo"
              <*> pure Nothing
              <*> pure ( case message of
                Nothing -> Nothing
                Just _ -> MP.runParser cqmsg $ fromMaybe "" strmsg )

showCQ :: CQMessage -> String
showCQ cqmsg = concat [absId, messageType, " ",  chatId, senderId, " ", senderName, senderCard', ": ", messageContent] --, mcqcodes]
  where messageType    = show $ eventType cqmsg
        absId          = maybe "" (\c -> "[" <> show c <> "] ") . absoluteId $ cqmsg
        chatId         = maybe "" show . groupId $ cqmsg
        senderId       = maybe "" (\c -> "(" ++ show c ++ ")") . userId $ cqmsg
        senderName     = maybe "" unpack $ senderNickname =<< sender cqmsg
        senderCard'    = surround $ maybe "" (unpack) $ senderCard =<< sender cqmsg
        messageContent = maybe "" unpack $ message cqmsg
        surround s     = if null s then s else "(" ++ s ++ ")"
        -- mcqcodes       = maybe "" (("\n"++) . show) $ mNonEmpty . cqcodes =<< metaMessage cqmsg
        -- mNonEmpty []   = Nothing
        -- mNonEmpty l    = Just l

type EssentialContent = (Text, ChatId, UserId, MessageId, Sender)
cqmsgToEssentialContent :: CQMessage -> Maybe EssentialContent
cqmsgToEssentialContent cqmsg =
  (,,,,) <$> (fmap onlyMessage . metaMessage $ cqmsg)
         <*> (case eventType cqmsg of
               GroupMessage -> GroupChat <$> groupId cqmsg
               PrivateMessage -> PrivateChat <$> userId cqmsg
               _ -> Nothing
             )
         <*> userId cqmsg
         <*> messageId cqmsg
         <*> sender cqmsg

