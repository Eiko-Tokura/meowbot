{-# LANGUAGE TemplateHaskell, PartialTypeSignatures, ScopedTypeVariables, OverloadedStrings #-}
module Command.Cat where

import Command
import Command.Md
import MeowBot.BotStructure
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import External.ChatAPI
import MeowBot.Parser (Parser, Chars)
import Parser.Definition (IsStream)
import qualified MeowBot.Parser as MP
import Control.Monad.IOe

import Control.Applicative

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

commandCat :: BotCommand
commandCat = BotCommand Cat $ botT $ do
  (msg, cid, uid, mid, sender) <- MaybeT $ getEssentialContent <$> asks fst
  other_data <- lift get
  whole_chat <- lift $ asks fst
  let sd = savedData other_data
  let msys = ChatSetting
               ((systemMessage =<< lookup cid (chatSettings sd)) <|> (fmap (Message "system" . T.pack) . globalSysMsg $ botModules other_data))
               (systemTemp =<< lookup cid (chatSettings sd))
  -- looking for custom system message
      botname = nameOfBot $ botModules other_data
  lChatModelMsg <- pureMaybe $ MP.runParser (treeCatParser botname msys mid) (getFirstTree whole_chat)
  let rlChatModelMsg = reverse lChatModelMsg
      params@(ChatParams model md _) = fst . head $ rlChatModelMsg
      ioEChatResponse = messageChat params $ (map snd . reverse . take 20) rlChatModelMsg
  cid <- pureMaybe $ checkAllowedCatUsers sd model cid
  asyncAction <- liftIO $ (if md then sendIOeToChatIdMdAsync else sendIOeToChatIdAsync) (msg, cid, uid, mid, sender) ioEChatResponse
  return $ pure $ BAAsync $ asyncAction
  where checkAllowedCatUsers _  GPT3 anybody = return anybody
        checkAllowedCatUsers sd GPT4 g@(GroupChat gid) = mIf ((gid, AllowedGroup) `elem` groupGroups sd) g
        checkAllowedCatUsers sd GPT4 p@(PrivateChat uid) = mIf ((uid, Allowed) `elem` userGroups sd) p

catParser :: (Chars sb) => BotName -> ChatSetting -> Parser sb Char (ChatParams, String)
catParser (Just botname) msys = do
  MP.spaces0
  parseMeowMeow
  where
    parseMeowMeow = do
      MP.string botname
      MP.commandSeparator2
      str <- MP.some MP.item
      return (ChatParams GPT3 False msys, str)
catParser Nothing msys = do
  MP.spaces0
  parseCat <|> parseMeowMeow
  where
    parseCat = do
      MP.just ':' <|> MP.just '：'
      md <- MP.optBool $ $(MP.stringQ "md")
      modelStr <- $(MP.stringQ "cat") <|> $(MP.stringQ "supercat")
      MP.commandSeparator
      str <- MP.some MP.item
      let model = case modelStr of
                    "supercat" -> GPT4
                    _ -> GPT3
      return (ChatParams model md msys, str)
    parseMeowMeow = do
      $(MP.stringQ "喵喵")
      MP.commandSeparator2
      str <- MP.some MP.item
      return (ChatParams GPT3 False msys, str)

replyCatParser :: (Chars sb) => BotName -> ChatSetting -> Parser sb Char (ChatParams, String)
replyCatParser name msys = catParser name msys <|> ( do
  MP.spaces0
  str <- MP.some MP.item
  return (ChatParams GPT3 False msys, str)
  )

treeCatParser :: forall s. (IsStream s CQMessage) => BotName -> ChatSetting -> Int -> Parser s CQMessage [(ChatParams, Message)]
treeCatParser name msys mid = do
  elist  <- Right <$>
    --   ( do
    --     firstUMsg <- MP.satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage] ) -- will be dropped
    --     firstAMsg <- MP.satisfy (\cqm -> eventType cqm == SelfMessage)
    --     case ( do
    --             MP.runParser aokanaParser (extractMetaMessage firstUMsg)  -- the first user input should be an aokana command
    --             meta <- metaMessage firstAMsg
    --             return $ MP.withChatSetting meta -- there might be modified system message
    --          ) of
    --       Just msys' ->
    --         do
    --           innerList <- MP.many
    --             (do
    --               umsg <- MP.satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage])
    --               amsg <- MP.satisfy (\cqm -> eventType cqm == SelfMessage)
    --               let (params, metaUMsg) = fromMaybe (ChatParams GPT3 False (chatSettingMaybeWrapper msys'), "") $ MP.runParser (catParser name (chatSettingMaybeWrapper msys')) (extractMetaMessage umsg)
    --               return [ (params, Message { role = "user", content = T.pack metaUMsg})
    --                      , (params, Message { role = "assistant", content = extractMetaMessage amsg})
    --                      ]
    --             )
    --           let params = ChatParams GPT3 False (chatSettingMaybeWrapper msys')
    --           return (msys', [(params, Message "assistant" $ extractMetaMessage firstAMsg)] : innerList)
    --       Nothing -> MP.zero
    -- )
    -- MP.|+|
      MP.many (do
          umsg <- MP.satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage])
          amsg <- MP.satisfy (\cqm -> eventType cqm == SelfMessage)
          let (params, metaUMsg) = fromMaybe (ChatParams GPT3 False msys, "") $ MP.runParser (replyCatParser name msys) (extractMetaMessage umsg)
          return [ (params, Message { role = "user" , content = T.pack metaUMsg })
                 , (params, Message { role = "assistant", content = extractMetaMessage amsg})
                 ]
        ) :: Parser s CQMessage (Either (Maybe ChatSetting, [[(ChatParams, Message)]]) [[(ChatParams, Message)]])

  lastMsg <- MP.satisfy (\cqm -> (eventType cqm `elem` [GroupMessage, PrivateMessage]) && messageId cqm == Just mid)
  case elist of
    Right list ->
      case MP.runParser (if null list then catParser name msys else replyCatParser name msys) (extractMetaMessage lastMsg) of
            Just (params, metaLast) -> return $ concat list ++
                [ (params, Message { role = "user", content = T.pack metaLast}) ]
            _ -> MP.zero
    Left (msys', list) -> case MP.runParser (replyCatParser name (chatSettingMaybeWrapper msys')) (extractMetaMessage lastMsg) of
            Just (params, metaLast) -> return $ concat list ++
                [ (params, Message { role = "user", content = T.pack metaLast}) ]
            _ -> MP.zero
  where extractMetaMessage CQMessage{metaMessage = Nothing} = ""
        extractMetaMessage CQMessage{metaMessage = Just mmsg} = MP.onlyMessage mmsg

