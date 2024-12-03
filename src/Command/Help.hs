{-# LANGUAGE OverloadedStrings #-}
module Command.Help where

import Command
import Command.Random (helpRandom)
import Command.Study (helpStudy)
import Command.Poll (helpPoll)
import MeowBot.BotStructure
import Data.Maybe (fromMaybe)
import qualified MeowBot.Parser as MP

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

commandHelp :: BotCommand
commandHelp = BotCommand Help $ botT $ do
  (msg, cid, _, _, _) <- MaybeT $ getEssentialContent <$> ask
  mbotName <- lift $ nameOfBot . botModules <$> get
  helpParser' <- lift $ commandParserTransformByBotName $ helpParser mbotName
  helpText <- pureMaybe $ MP.runParser helpParser' msg
  return [baSendToChatId cid helpText]
  where
    helpParser botName = do
      MP.headCommand "help"
      MP.spaces0
      mParam <- MP.tryMaybe . MP.asumE $ map ((\str -> MP.string str <* MP.spaces0 <* MP.end) . fst) helpList;
      case mParam of
        Just str -> return $ fromMaybe "" $ lookup str helpList
        Nothing  -> return . pack . concat $
          [ "你好，这里是" ++ fromMaybe "Eiko的喵喵" botName ++ "~目前支持的命令："
          , concatMap (("\n:" ++ ) . fst) helpList
          , "\n可以使用 :help <command>如:help cat来查看详细命令的帮助。"
          ]

helpList :: [(String, Text)]
helpList =
  [ ("cat", ":cat <message>\n让喵喵使用GPT-3帮助您回答问题，" <> replyHelp)
  , ("supercat", ":supercat <message>\n喵喵使用GPT-4帮助您回答问题，只有部分群和用户可用。" <> replyHelp)
  , ("mdcat", ":mdcat <message>\n喵喵将cat命令的回复变成一张markdown图片，" <> replyHelp)
  , ("mdsupercat", ":mdsupercat <message>\n喵喵将supercat的回复变成一张markdown图片，只有部分群和用户可用。" <> replyHelp)
  , ("md", ":md <markdown>\n接受一段markdown，将其转换为一张图片发给你。")
  , ("system", ":system set / unset <message>\n设置群/私聊的gpt系统提示消息")
  , ("temperature", ":temperature <float>\n设置群/私聊的gpt系统温度")
  , ("user", ":user <add/remove/list> <admin/allowed> <uid>\n管理用户组权限，仅admin Group可用")
  , ("group", ":group <add/remove/list> <allowedGroup> <gid>\n管理群组权限，仅admin Group可用")
  , ("rule", ":rule <add/remove/list> <rule>\n管理命令规则, 仅admin Group可用")
  , ("help", ":help <command>\n查看帮助")
  , ("aokana", ":aokana [flags/search items]\n 随机选取一段苍彼的语音对话！可用flags: -asuka, -misaki, -mashiro, -rika, -list. 搜索项和flags之间用空格分隔。搜索项也可以用英文引号括起来，这样会包含空格。搜索会在四种语言中进行。")
  , ("random", helpRandom)
  , ("study", helpStudy)
  , ("poll", helpPoll)
  ]
  where replyHelp = "您可以回复我的消息以使用上下文。回复会以Tree的数据结构组织，您可以回复任何一个节点，但有长度限制。回复时可以指定:cat,:supercat,:mdcat等指令，不指定时默认使用:cat回复。"

