{-# LANGUAGE OverloadedStrings #-}
module Command where

import MeowBot.BotStructure
import Data.Text (Text, pack)
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.WebSockets (Connection, sendTextData)

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.ReaderState
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)

type BotCommand = ReaderStateT WholeChat OtherData IO [BotAction]
-- data ReaderStateT r s m a = ReaderStateT {runReaderStateT :: r -> s -> m (a, s)}
-- BotCommand is a monadic value of the monad (ReaderStateT WholeChat OtherData IO)

botT :: Monad m => MaybeT (ReaderStateT WholeChat OtherData m) [a] -> ReaderStateT WholeChat OtherData m [a]
botT = fmap (fromMaybe []) . runMaybeT

doBotAction :: Connection -> BotAction -> IO ()
doBotAction conn (BASendPrivate (PrivateId uid) txt) = sendPrivate conn uid txt
doBotAction conn (BASendGroup (GroupId gid) txt) = sendGroup conn gid txt
doBotAction _ ba = putStrLn $ "Invalid BotAction: " ++ show ba

sendPrivate :: Connection -> Int -> Text -> IO ()
sendPrivate conn uid text = do
  sendTextData conn $ encode (SendMessageForm "send_private_msg" (PrivateParams uid text) (Just $ pack . show $ uid))
  putStrLn $ concat ["-> user ", show uid, ": ", T.unpack text]

sendGroup :: Connection -> Int -> Text -> IO ()
sendGroup conn gid text = do
  sendTextData conn $ encode (SendMessageForm "send_group_msg" (GroupParams gid text) (Just $ pack . show $ gid))
  putStrLn $ concat ["-> group ", show gid, ": ", T.unpack text]

-- Input all data, all commands, do the commands that is required by the input, then return updated data
doBotCommands ::  Connection -> [BotCommand] -> StateT AllData IO () 
doBotCommands conn commands = do
  actions <- globalize wholechat otherdata AllData `mapM` commands
  mapM_ (lift . doBotAction conn) $ concat actions
  
-- doBotCommands :: Connection -> AllData -> [BotCommand] -> IO AllData
-- doBotCommands conn alldata botcommands = do
--  l_lBotActionXOtherData <- 
--    filter (not . null) <$> (($ other_data) . ($ whole_chat) . runReaderStateT) `mapM` botcommands
--  case l_lBotActionXOtherData of
--    [] -> return alldata 
--    _  -> do 
--      let lBA = (fst . head) l_lBotActionXOtherData
--      case lBA of
--        [] -> return ()
--        (_:_) -> doBotAction conn `mapM_` lBA
--      return $ AllData 
--        (wholechat alldata)
--        (updateOtherData (otherdata alldata) ((snd . head) l_lBotActionXOtherData))

