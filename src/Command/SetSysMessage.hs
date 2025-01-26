{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Command.SetSysMessage
  (
    commandSetSysMessage
  ) where

import Command
import MeowBot.Parser ((|+|), (<|>), tshow)
import qualified MeowBot.Parser as MP
import MeowBot
import qualified Data.Text as T
import Data.Bifunctor
import External.ChatAPI

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

commandSetSysMessage :: BotCommand
commandSetSysMessage = BotCommand System $ botT $ do
  ess@(msg, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  sysMsgParser' <- lift $ commandParserTransformByBotName sysMsgParser
  msys <- pureMaybe $ MP.runParser sysMsgParser' msg
  other_data <- lift query
  let sd = savedData other_data
  --pureMaybe $ checkIfIsGroupNeedBeAllowedUsers sd (cid, uid)
  let msysSet = first (SystemMessage . T.pack <$>) msys
  case msysSet of
    Left msysMsg -> lift $ do
      change $ \other_data -> other_data {savedData = sd {chatSettings = updateSysSetting msysSet cid $ chatSettings $ savedData other_data}}
      case msysMsg of
        Just _ -> sendToChatId ess "系统消息已设置owo!"
        Nothing -> sendToChatId ess "系统消息已返回默认owo!"
    Right temp -> lift $ do
      change $ \other_data -> other_data {savedData = sd {chatSettings = updateSysSetting msysSet cid $ chatSettings $ savedData other_data}}
      sendToChatId ess $ "系统温度已设置为" <> tshow temp <> " owo!"
  where
    sysMsgParser =
      ( do
        MP.headCommand "system"
        MP.commandSeparator
        ($(MP.stringQ "set") >> MP.commandSeparator >> Just <$> MP.some MP.item) <|> ($(MP.stringQ "unset") >> return Nothing)
      ) |+| (MP.headCommand "temperature" >> MP.commandSeparator >> MP.positiveFloat)

updateSysSetting :: Either (Maybe Message) Double -> ChatId -> [(ChatId, ChatSetting)] -> [(ChatId, ChatSetting)]
updateSysSetting (Left msys) cid []
  = [(cid, ChatSetting msys Nothing Nothing Nothing)]
updateSysSetting (Left msys) cid (x0@(cid', ChatSetting _ mt _ _) : xs)
  | cid == cid' = (cid, ChatSetting msys mt Nothing Nothing) : xs
  | otherwise   = x0 : updateSysSetting (Left msys) cid xs
updateSysSetting (Right temp) cid []
  = [(cid, ChatSetting Nothing (Just temp) Nothing Nothing)]
updateSysSetting (Right temp) cid (x0@(cid', ChatSetting ms _ _ _) : xs)
  | cid == cid' = (cid, ChatSetting ms (Just temp) Nothing Nothing) : xs
  | otherwise   = x0 : updateSysSetting (Right temp) cid xs
