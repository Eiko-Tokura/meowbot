{-# LANGUAGE OverloadedStrings #-}
module MeowBot.Action where

import MeowBot.BotStructure
import MeowBot.CommandRule
import MeowBot.Update
import System.Meow
import System.General
import Control.Concurrent.Async (Async, asyncThreadId, async)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

-- | Abstract representation of sending a message to a chat id.
baSendToChatId :: ChatId -> Text -> BotAction
baSendToChatId (GroupChat gid)   txt = BASendGroup gid txt
baSendToChatId (PrivateChat uid) txt = BASendPrivate uid txt

-- | runing an ExceptT String IO String action with string result, and send the result to a chat id. Handles exceptions.
sendIOeToChatId :: EssentialContent -> ExceptT Text IO Text -> Meow [BotAction]
sendIOeToChatId (_, cid, _, mid, _) ioess = do
  ess <- lift $ runExceptT ioess
  case ess of
    Right str -> do
      insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid str ]
    Left err -> return [ baSendToChatId cid ("喵~出错啦：" <> err) ]

sendIOeToChatIdAsync :: EssentialContent -> ExceptT Text IO Text -> IO (Async (Meow [BotAction]))
sendIOeToChatIdAsync (_, cid, _, mid, _) ioess = async $ do
  ess <- runExceptT ioess
  case ess of
    Right str -> return $ do
      insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid str ]
    Left err -> return $ return [ baSendToChatId cid ("喵~出错啦：" <> err) ]

-- | send message to a chat id, recording the message as reply.
sendToChatId :: MonadIO m => EssentialContent -> Text -> MeowT r mods m [BotAction]
sendToChatId (_, cid, _, mid, _) str = meowSendToChatIdFull cid (Just mid) [] [] str
--([baSendToChatId cid str], insertMyResponseHistory utc cid (generateMetaMessage str [] [MReplyTo mid]) other_data )

-- | send message to a chat id, recording the message as reply (optional in Maybe MessageId), with additional data and meta items.
-- Also increase the message number (absolute id)
meowSendToChatIdFull :: MonadIO m => ChatId -> Maybe MessageId -> [AdditionalData] -> [MetaMessageItem] -> Text -> MeowT r mods m [BotAction]
meowSendToChatIdFull cid mid adt items str = do
  let meta = generateMetaMessage str adt ([MReplyTo mid' | Just mid' <- pure mid ] ++ items)
  insertMyResponseHistory cid meta
  return [ baSendToChatId cid str ]

