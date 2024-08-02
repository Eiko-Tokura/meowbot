{-# LANGUAGE OverloadedStrings #-}
module Command.SetSysMessage where

import Command
import qualified MonParserF as MP
import MeowBot.BotStructure
import qualified Data.Text as T
import Data.List
import External.ChatAPI

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

commandSetSysMessage :: BotCommand
commandSetSysMessage = BotCommand System $ botT $ do
  ess@(msg, cid, _, _) <- MaybeT $ getEssentialContent <$> ask
  msys <- pureMaybe $ MP.mRunParserF sysMsgParser msg
  other_data <- lift get
  let sd = savedData other_data
  --pureMaybe $ checkIfIsGroupNeedBeAllowedUsers sd (cid, uid)
  let msysMsg = Message "system" . T.pack <$> msys
  case msysMsg of
    Just sysMsg -> lift $ do
      put other_data {savedData = sd {sysMessages = insert (cid, sysMsg) $ sysMessages $ savedData other_data}}
      onlyState $ sendToChatId ess "系统消息已设置owo!"
    Nothing -> lift $ do
      put other_data {savedData = sd {sysMessages = filter ((/= cid) . fst) $ sysMessages $ savedData other_data}}
      onlyState $ sendToChatId ess "系统消息已返回默认owo!"
  where
    sysMsgParser = do
      MP.headCommand "system"
      MP.commandSeparator
      setOrUnset <- MP.string "set" `MP.eitherParse` MP.string "unset"
      case setOrUnset of
        Left _ -> Just <$> MP.many MP.item
        Right _ -> return Nothing

