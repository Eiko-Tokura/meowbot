{-# LANGUAGE OverloadedStrings #-}
module MeowBot.Action where

import MeowBot.BotStructure
import MeowBot.Update
import System.Meow
import System.General
import Module
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Trans.Except

-- | Abstract representation of sending a message to a chat id.
-- will NOT insert the message into the history.
baSendToChatId :: ChatId -> Text -> BotAction
baSendToChatId (GroupChat gid)   txt = BASendGroup gid txt
baSendToChatId (PrivateChat uid) txt = BASendPrivate uid txt

-- | runing an ExceptT String IO String action with string result, and send the result to a chat id. Handles exceptions.
-- will insert the message into the history.
sendIOeToChatId :: EssentialContent -> ExceptT Text IO Text -> Meow [BotAction]
sendIOeToChatId (_, cid, _, mid, _) ioess = do
  ess <- lift $ runExceptT ioess
  case ess of
    Right str -> do
      insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid str ]
    Left err -> return [ baSendToChatId cid ("喵~出错啦：" <> err) ]

-- | runing an ExceptT String IO String action in Async, and send the result to a chat id. Handles exceptions.
-- will insert the message into the history.
-- using this together with BAAsync return types for a long-time IO action that might block the main thread like fetching chat-gpt or webpage response, or a local running IO action like generating a large image.
sendIOeToChatIdAsync :: EssentialContent -> ExceptT Text IO Text -> IO (Async (Meow [BotAction]))
sendIOeToChatIdAsync (_, cid, _, mid, _) ioess = async $ do
  ess <- runExceptT ioess
  case ess of
    Right str -> return $ do
      insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid str ]
    Left err -> return $ return [ baSendToChatId cid ("喵~出错啦：" <> err) ]

-- | send message to a chat id, recording the message as reply.
sendToChatId :: (HasSystemRead (TVar (Maybe SentCQMessage)) r, MonadIO m) => EssentialContent -> Text -> MeowT r mods m [BotAction]
sendToChatId (_, cid, _, mid, _) str = meowSendToChatIdFull cid (Just mid) [] [] str
--([baSendToChatId cid str], insertMyResponseHistory utc cid (generateMetaMessage str [] [MReplyTo mid]) other_data )

-- | send message to a chat id, recording the message as reply in meta message (optional in Maybe MessageId), with additional data and meta items.
-- Also increase the message number (absolute id)
-- will insert the message into the history.
meowSendToChatIdFull :: (HasSystemRead (TVar (Maybe SentCQMessage)) r, MonadIO m)
  => ChatId            -- ^ chat id to send to
  -> Maybe MessageId   -- ^ message id to reply to, if Nothing, will not record the message as reply.
  -> [AdditionalData]  -- ^ additional data to attach to the message
  -> [MetaMessageItem] -- ^ meta items to attach to the message
  -> Text              -- ^ message content
  -> MeowT r mods m [BotAction]
meowSendToChatIdFull cid mid adt items str = do
  let meta = generateMetaMessage str adt ([MReplyTo mid' | Just mid' <- [mid] ] ++ items)
  insertMyResponseHistory cid meta
  return [ baSendToChatId cid str ]

meowAsyncSplitSendToChatIdFull
  :: ChatId            -- ^ chat id to send to
  -> Maybe MessageId   -- ^ message id to reply to, if Nothing, will not record the message as reply.
  -> [AdditionalData]  -- ^ additional data to attach to the message
  -> [MetaMessageItem] -- ^ meta items to attach to the message
  -> Int               -- ^ split delay
  -> [Text]            -- ^ message content
  -> Meow [BotAction]
meowAsyncSplitSendToChatIdFull _ _ _ _ _ [] = return []
meowAsyncSplitSendToChatIdFull cid mid adt items _ (txt:[]) = do
  act1 <- meowSendToChatIdFull cid mid adt items txt
  return $ act1
meowAsyncSplitSendToChatIdFull cid mid adt items delay (txt:rest) = do
  act1 <- meowSendToChatIdFull cid mid adt items txt
  act2 <- fmap (pure . BAAsync) . liftIO $ async $ do
      threadDelay delay
      return $ meowAsyncSplitSendToChatIdFull cid mid adt items delay rest
  return $ act1 <> act2

-- | Turnning an meow action that returns an action that asynchronously returns a Meow [BotAction] into a Meow [BotAction]
asyncMeow :: Meow (IO (Meow [BotAction])) -> Meow [BotAction]
asyncMeow masync = do
  asyncAction  <- masync -- meow part
  asyncWrapped <- liftIO $ async asyncAction -- async
  return $ [BAAsync asyncWrapped]

