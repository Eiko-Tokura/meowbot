{-# LANGUAGE TemplateHaskell, PartialTypeSignatures, ScopedTypeVariables, OverloadedStrings #-}
module Command.Cat where

import Command
import Command.Md
import MeowBot
import Data.Maybe (fromMaybe, listToMaybe)
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
import Control.Monad.Except
import Control.Concurrent

type MeowTools = '[FibonacciTool]

type ModelCat      = DeepSeek DeepSeekChat
type ModelSuperCat = DeepSeek DeepSeekReasoner

modelCat      = chatModel @ModelCat
modelSuperCat = chatModel @ModelSuperCat

type ChatParamsPerModel ts = Either (ChatParams ModelCat ts) (ChatParams ModelSuperCat ts)

meowMaxToolDepth :: Int
meowMaxToolDepth = 5

commandCat :: BotCommand
commandCat = BotCommand Cat $ botT $ do
  (msg, cid, uid, mid, sender) <- MaybeT $ getEssentialContent <$> query
  other_data <- query
  whole_chat <- query
  botmodules <- query
  botname    <- query
  let sd = savedData other_data
  let msys = ChatSetting
               ((systemMessage =<< lookup cid (chatSettings sd)) <|> (fmap (SystemMessage . T.pack) . globalSysMsg $ botmodules))
               (systemTemp =<< lookup cid (chatSettings sd))
               (Just 5)
               Nothing
  -- looking for custom system message
  lChatModelMsg <- pureMaybe $ MP.runParser (treeCatParser botname msys mid) (getFirstTree whole_chat)
  let rlChatModelMsg = reverse lChatModelMsg
      params = fst . head $ rlChatModelMsg
      md = either chatMarkDown chatMarkDown params
      ioEChatResponse = case params of
        Left paramCat       -> messagesChat @ModelCat      @MeowTools paramCat      $ (map snd . reverse . take 20) rlChatModelMsg
        Right paramSuperCat -> messagesChat @ModelSuperCat @MeowTools paramSuperCat $ (map snd . reverse . take 20) rlChatModelMsg
  cid <- pureMaybe $ checkAllowedCatUsers (chatModel @ModelCat) sd cid
  asyncAction <- liftIO $ actionSendMessages md (msg, cid, uid, mid, sender) ioEChatResponse
  $(logDebug) $ "created async: " <> T.pack (show asyncAction)
  return $ pure $ BAAsync $ asyncAction
  where checkAllowedCatUsers _  _ anybody = return anybody
        -- checkAllowedCatUsers sd modelSuperCat g@(GroupChat gid) = mIf ((gid, AllowedGroup) `elem` groupGroups sd) g
        -- checkAllowedCatUsers sd modelSuperCat p@(PrivateChat uid) = mIf ((uid, Allowed) `elem` userGroups sd) p

type UseMarkdown = Bool
actionSendMessages :: UseMarkdown -> EssentialContent -> ExceptT Text IO [Message] -> IO (Async (Meow [BotAction]))
actionSendMessages usemd essc@(_, cid, _, mid, _) ioess = async $ do
  ess <- runExceptT ioess
  case ess of
    Left err   -> return . return $ [ baSendToChatId cid ("喵~出错啦：" <> err) ]
    Right msgs -> casingMessages usemd essc msgs
  where casingMessages :: UseMarkdown -> EssentialContent -> [Message] -> IO (Meow [BotAction])
        casingMessages _ _ [] = return . return $ []
        casingMessages usemd essc (msg:rest) = case msg of
          AssistantMessage { content = str, thinking = Nothing } -> return $ if usemd
            then do
              mdcq <- liftIO . runExceptT $ turnMdCQCode str
              case mdcq of
                Left err   -> return [ baSendToChatId cid ("喵~出错啦：" <> err) ]
                Right mdcq -> do
                  send <- meowSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage msg] mdcq
                  waitAndDoRest <- liftIO . actionSendMessages usemd essc $ do
                      liftIO $ threadDelay 2000000
                      ExceptT $ return $ Right rest
                  return $ send <> [BAAsync waitAndDoRest]
            else do
              send <- meowSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage msg] str
              waitAndDoRest <- liftIO . actionSendMessages usemd essc $ do
                  liftIO $ threadDelay 2000000
                  ExceptT $ return $ Right rest
              return $ send <> [BAAsync waitAndDoRest]
          AssistantMessage { thinking = Just think } -> return $ do
            send <- meowSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage msg] $ "(..." <> T.strip think <> ")"
            waitAndDoRest <- liftIO . actionSendMessages usemd essc $ do
                liftIO $ threadDelay 1000000
                ExceptT $ return $ Right $ msg { thinking = Nothing } : rest
            return $ send <> [BAAsync waitAndDoRest]
          _ -> casingMessages usemd essc rest

catParser :: (Chars sb) => BotName -> ChatSetting ->
  Parser sb Char
    ( ChatParamsPerModel ts
    , String
    )
catParser (BotName (Just botname)) msys = do
  MP.spaces0
  parseCat <|> parseMeowMeow
  where
    parseMeowMeow = do
      MP.string botname
      MP.commandSeparator2
      str <- MP.some MP.item
      return (Left $ ChatParams False msys, str)
    parseCat = do
      MP.string botname
      MP.just ':' <|> MP.just '：'
      md <- MP.optBool $ $(MP.stringQ "md")
      modelStr <- $(MP.stringQ "cat") <|> $(MP.stringQ "supercat")
      MP.commandSeparator
      str <- MP.some MP.item
      case modelStr of
         "supercat" -> return . (, str) . Left  $ ChatParams md (Just msys `chatSettingAlternative` ChatSetting Nothing (Just 0.2) Nothing Nothing)
         _          -> return . (, str) . Right $ ChatParams md (Just msys `chatSettingAlternative` ChatSetting Nothing (Just 0.7) Nothing Nothing)
catParser (BotName Nothing) msys = do
  MP.spaces0
  parseCat <|> parseMeowMeow
  where
    parseCat = do
      MP.just ':' <|> MP.just '：'
      md <- MP.optBool $ $(MP.stringQ "md")
      modelStr <- $(MP.stringQ "cat") <|> $(MP.stringQ "supercat")
      MP.commandSeparator
      str <- MP.some MP.item
      case modelStr of
         "supercat" -> return . (, str) . Left  $ ChatParams md (Just msys `chatSettingAlternative` ChatSetting Nothing (Just 0.2) Nothing Nothing)
         _          -> return . (, str) . Right $ ChatParams md (Just msys `chatSettingAlternative` ChatSetting Nothing (Just 0.7) Nothing Nothing)
    parseMeowMeow = do
      $(MP.stringQ "喵喵")
      MP.commandSeparator2
      str <- MP.some MP.item
      return (Left $ ChatParams False msys, str)

replyCatParser :: (Chars sb) => BotName -> ChatSetting -> Parser sb Char (ChatParamsPerModel ts, String)
replyCatParser name msys = catParser name msys <|> ( do
  MP.spaces0
  str <- MP.some MP.item
  return (Left $ ChatParams False msys, str)
  )

treeCatParser :: forall s ts. (IsStream s CQMessage) => BotName -> ChatSetting -> Int -> Parser s CQMessage [(ChatParamsPerModel ts, Message)]
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
    --               let (params, metaUMsg) = fromMaybe (ChatParams modelCat False (chatSettingMaybeWrapper msys'), "") $ MP.runParser (catParser name (chatSettingMaybeWrapper msys')) (extractMetaMessage umsg)
    --               return [ (params, Message { role = "user", content = T.pack metaUMsg})
    --                      , (params, Message { role = "assistant", content = extractMetaMessage amsg})
    --                      ]
    --             )
    --           let params = ChatParams modelCat False (chatSettingMaybeWrapper msys')
    --           return (msys', [(params, Message "assistant" $ extractMetaMessage firstAMsg)] : innerList)
    --       Nothing -> MP.zero
    -- )
    -- MP.|+|
      MP.many (do
          umsg <- MP.satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage])
          amsg <- MP.satisfy (\cqm -> eventType cqm == SelfMessage )
          msg <- maybe MP.zero return $ do
            mtmAmsg <- metaMessage amsg
            listToMaybe [ m | MMessage m <- metaMessageItems mtmAmsg ]
          let (params, metaUMsg) = fromMaybe (Left $ ChatParams False msys, "") $ MP.runParser (replyCatParser name msys) (extractMetaMessage umsg)
          return [ (params, UserMessage { content = T.pack metaUMsg })
                 , (params, msg)
                 ]
        ) :: Parser s CQMessage (Either (Maybe ChatSetting, [[(ChatParamsPerModel ts, Message)]]) [[(ChatParamsPerModel ts, Message)]])

  lastMsg <- MP.satisfy (\cqm -> (eventType cqm `elem` [GroupMessage, PrivateMessage]) && messageId cqm == Just mid)
  case elist of
    Right list ->
      case MP.runParser (if null list then catParser name msys else replyCatParser name msys) (extractMetaMessage lastMsg) of
            Just (params, metaLast) -> return $ concat list ++
                [ (params, UserMessage { content = T.pack metaLast}) ]
            _ -> MP.zero
    Left (msys', list) -> case MP.runParser (replyCatParser name (chatSettingMaybeWrapper msys')) (extractMetaMessage lastMsg) of
            Just (params, metaLast) -> return $ concat list ++
                [ (params, UserMessage { content = T.pack metaLast}) ]
            _ -> MP.zero
  where extractMetaMessage CQMessage{metaMessage = Nothing} = ""
        extractMetaMessage CQMessage{metaMessage = Just mmsg} = MP.onlyMessage mmsg

