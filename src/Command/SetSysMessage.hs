{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Command.SetSysMessage
  (
    commandSetSysMessage
  ) where

import Command
import MeowBot.Parser ((|+|), (<|>), tshow)
import qualified MeowBot.Parser as MP
import MeowBot.BotStructure
import qualified Data.Text as T
import Data.Bifunctor
import External.ChatAPI

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

commandSetSysMessage :: BotCommand
commandSetSysMessage = BotCommand System $ botT $ do
  ess@(msg, cid, _, _) <- MaybeT $ getEssentialContent <$> ask
  sysMsgParser' <- lift $ commandParserTransformByBotName sysMsgParser
  msys <- pureMaybe $ MP.runParser sysMsgParser' msg
  other_data <- lift get
  let sd = savedData other_data
  --pureMaybe $ checkIfIsGroupNeedBeAllowedUsers sd (cid, uid)
  let msysSet = first (Message "system" . T.pack <$>) msys
  case msysSet of
    Left msysMsg -> lift $ do
      put other_data {savedData = sd {chatSettings = updateSysSetting msysSet cid $ chatSettings $ savedData other_data}}
      case msysMsg of
        Just _ -> onlyState $ sendToChatId ess "系统消息已设置owo!"
        Nothing -> onlyState $ sendToChatId ess "系统消息已返回默认owo!"
    Right temp -> lift $ do
      put other_data {savedData = sd {chatSettings = updateSysSetting msysSet cid $ chatSettings $ savedData other_data}}
      onlyState $ sendToChatId ess $ "系统温度已设置为" <> tshow temp <> " owo!"
  where
    sysMsgParser =
      ( do
        MP.headCommand "system"
        MP.commandSeparator
        ($(MP.stringQ "set") >> MP.commandSeparator >> Just <$> MP.some MP.item) <|> ($(MP.stringQ "unset") >> return Nothing)
      ) |+| (MP.headCommand "temperature" >> MP.commandSeparator >> MP.positiveFloat)

updateSysSetting :: Either (Maybe Message) Double -> ChatId -> [(ChatId, ChatSetting)] -> [(ChatId, ChatSetting)]
updateSysSetting (Left msys) cid [] = [(cid, ChatSetting msys Nothing)]
updateSysSetting (Left msys) cid (x0@(cid', ChatSetting _ mt) : xs)
  | cid == cid' = (cid, ChatSetting msys mt) : xs
  | otherwise   = x0 : updateSysSetting (Left msys) cid xs
updateSysSetting (Right temp) cid [] = [(cid, ChatSetting Nothing $ Just temp)]
updateSysSetting (Right temp) cid (x0@(cid', ChatSetting ms _) : xs)
  | cid == cid' = (cid, ChatSetting ms $ Just temp) : xs
  | otherwise   = x0 : updateSysSetting (Right temp) cid xs
