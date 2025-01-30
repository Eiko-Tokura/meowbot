module Command.Chat where

import Command
import Command.Md
import MeowBot
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import External.ChatAPI
import External.ChatAPI.Tool
import MeowBot.Parser (Parser, Chars)
import Parser.Definition (IsStream)
import qualified MeowBot.Parser as MP

import Control.Applicative

import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

type MeowTools = '[] -- empty for now

modelCat :: ChatModel
modelCat = DeepSeek DeepSeekChat

modelSuperCat :: ChatModel
modelSuperCat = DeepSeek DeepSeekReasoner

meowMaxToolDepth :: Int
meowMaxToolDepth = 5

-- | A new command that enables the bot to chat with users
-- should be more powerful than the previous legacy command Cat
-- maybe we will eventually deprecate the old command Cat
--
-- the idea :
-- * Every chat_id will maintain its state
-- * Context will be given
-- * Model has the ability to use tools, taking notes
-- - might consider: ability to initiate new messages
--     In current model, since meowmeow is based on a event-driven model, in principle most commands are run in response to a message
--
-- * Chat command is not enabled by default, it should be enabled by the user
--   it will randomly interact with the user in a group chat in low probability
--   but when being called (@meowmeow or called by command), it will respond 100%
--
-- What we will do first is to try this in private chat, providing note taking and scheduled message
commandChat :: BotCommand
commandChat = BotCommand Chat $ botT $ do
  undefined
