{-# LANGUAGE OverloadedStrings #-}
module Command 
  ( BotCommand
  , doBotCommands
  , botT
  ) where

import MeowBot.BotStructure
import Data.Text (Text, pack)
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.WebSockets (Connection, sendTextData)

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.ReaderState as RS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)

type BotCommand = ReaderStateT WholeChat OtherData IO [BotAction]
-- data ReaderStateT r s m a = ReaderStateT {runReaderStateT :: r -> s -> m (a, s)}
-- BotCommand is a monadic value of the monad (ReaderStateT WholeChat OtherData IO)

botT :: Monad m => MaybeT (ReaderStateT WholeChat OtherData m) [a] -> ReaderStateT WholeChat OtherData m [a]
botT = fmap (fromMaybe []) . runMaybeT

doBotAction :: Connection -> BotAction -> ReaderStateT WholeChat OtherData IO ()
doBotAction conn (BASendPrivate (PrivateId uid) txt) = RS.get >>= lift . sendPrivate conn uid txt . Just . pack . show . message_number 
doBotAction conn (BASendGroup (GroupId gid) txt)     = RS.get >>= lift . sendGroup   conn gid txt . Just . pack . show . message_number
doBotAction _ ba = lift $ putStrLn $ "Invalid BotAction: " ++ show ba

sendPrivate :: Connection -> Int -> Text -> Maybe Text -> IO ()
sendPrivate conn uid text mecho = do
  sendTextData conn $ encode (SendMessageForm "send_private_msg" (PrivateParams uid text) mecho)
  putStrLn $ concat ["-> user ", show uid, ": ", T.unpack text]

sendGroup :: Connection -> Int -> Text -> Maybe Text -> IO ()
sendGroup conn gid text mecho = do
  sendTextData conn $ encode (SendMessageForm "send_group_msg" (GroupParams gid text) mecho)
  putStrLn $ concat ["-> group ", show gid, ": ", T.unpack text]

-- | Input all data, all commands, do the commands that is required by the input, then return updated data
doBotCommands ::  Connection -> [BotCommand] -> StateT AllData IO () 
doBotCommands conn commands = globalize wholechat otherdata AllData $ do
  actions <- sequence commands
  doBotAction conn `mapM_` concat actions

