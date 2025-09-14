{-# LANGUAGE DeriveAnyClass, OverloadedStrings, DerivingVia #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module MeowBot.Data
  ( module MeowBot.MetaMessage
  , module MeowBot.Data
  , UserId(..), GroupId(..), ChatId(..), BotId(..)

  , CQMessage(..), CQEventType(..)
  , ReceCQMessage(..), SentCQMessage(..)
  , Sender(..), Role(..), roleToText
  , ResponseData(..)

  , Flag(..)
  , NoticeType(..), NoticeSubType(..), GroupDecreaseSubType(..), GroupIncreaseSubType(..)
  , RequestType(..), RequestGroupSubType(..)

  , ActionAPI(..), ActionForm(..)
  , QueryAPI(..), QueryAPIResponse(..), QueryType(..), WithEcho(..)
  , GroupBasicInfo(..)

  , BotName(..)
  , BotModules(..)
  , BotInstance(..)
  , DebugFlag(..), RunningFlag(..), IdentityFlag(..), ProxyFlag(..), LogFlag(..), CommandFlags(..), WatchDogFlag(..)
  , EssentialContent

  , module Utils.Text

  , showCQ, cqmsgToEssentialContent, emptyCQMessage, cqmsgToCid
  ) where

import Control.DeepSeq (NFData)
import Data.Default
import External.ProxyWS (ProxyData)
import GHC.Generics
import MeowBot.CommandRule
import MeowBot.Data.CQHttp.Action
import MeowBot.Data.CQHttp.CQMessage
import MeowBot.Data.CQHttp.Notice
import MeowBot.Data.CQHttp.Query
import MeowBot.Data.ChatId
import MeowBot.MetaMessage
import Utils.Lens
import Utils.Text
import qualified MeowBot.Parser as MP

import Database.Persist.Sqlite

newtype BotId = BotId { unBotId :: Int } deriving (Show, Eq, Ord, Read, Generic)
  deriving newtype (PersistField, PersistFieldSql, NFData, Num, Default)

-- | Structured and Unstructured Chat
-- recent messages top, older messages bottom
type ChatRoom = (ChatId, ([MP.Tree CQMessage], [CQMessage]))

type WholeChat = [ChatRoom]  -- [(ChatId, [Tree CQMessage])]
newtype BotName = BotName { maybeBotName :: Maybe String } deriving (Eq, Show)

instance ToText BotName Text where
  toText (BotName Nothing)  = "喵喵"
  toText (BotName (Just n)) = pack n

type RunningMode     = [DebugFlag]
data DebugFlag       = DebugJson | DebugCQMessage | DebugOther String deriving (Eq, Show)
data RunningFlag     = RunClient String Int | RunServer String Int deriving (Eq, Show)
data IdentityFlag    = UseName String | UseId BotId | UseSysMsg String deriving (Eq, Show)
data ProxyFlag       = ProxyFlag String Int deriving (Eq, Show)
newtype LogFlag      = LogFlag FilePath deriving (Eq, Show)
data WatchDogFlag    = WatchDogFlag
    Int -- ^ interval in seconds
    String -- ^ action command
  deriving (Eq, Show)
newtype CommandFlags = CommandFlag CommandId deriving (Eq, Show)

data BotInstance = BotInstance
  { botRunFlag       :: RunningFlag
  , botIdentityFlags :: [IdentityFlag]
  , botCommandFlags  :: [CommandFlags]
  , botDebugFlags    :: [DebugFlag]
  , botProxyFlags    :: [ProxyFlag]
  , botLogFlags      :: [LogFlag]
  , botWatchDogFlags :: [WatchDogFlag]
  } deriving (Eq, Show)

data BotModules = BotModules
  { canUseGroupCommands   :: [CommandId]
  , canUsePrivateCommands :: [CommandId]
  , nameOfBot             :: BotName
  , botId                 :: BotId
  , globalSysMsg          :: Maybe Text
  , proxyTChans           :: [ProxyData]
  , logFile               :: [FilePath]
  , botInstance           :: BotInstance
  } deriving (Show)

makeLenses_ ''BotModules
makeLenses_ ''BotInstance
