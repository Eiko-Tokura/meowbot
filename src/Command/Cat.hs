{-# LANGUAGE TemplateHaskell, PartialTypeSignatures, ScopedTypeVariables, OverloadedStrings, OverloadedRecordDot #-}
module Command.Cat where

import Command
import Command.Md
import Command.Cat.CatSet
import MeowBot
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import External.ChatAPI hiding (SystemMessage)
import qualified External.ChatAPI as API
-- import External.ChatAPI.Tool
import MeowBot.CostModel
import MeowBot.Parser (Parser, Chars)
import MeowBot.BotStatistics
import Parser.Definition (IsStream)
import qualified MeowBot.Parser as MP

import Control.Applicative

import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Module.Logging
import Control.Monad.Effect
import Control.Monad.Except
import Control.Concurrent

import Data.Default
import Data.HList
import Data.Proxy
import Data.Coerce
import Data.Bifunctor

import Utils.RunDB
import Utils.Persist
import Data.PersistModel
import Module.ConnectionManager

type MeowTools = '[] --FibonacciTool]

type ModelCat      = DeepSeek DeepSeekChat
type ModelSuperCat = DeepSeek DeepSeekReasoner

modelCatDef      = chatModel @ModelCat
modelSuperCatDef = chatModel @ModelSuperCat

type ChatMdSetting m ts = (Bool -> ChatSetting -> ChatParams m ts) -> ChatParams m ts

type ChatSettingPerModel m1 m2 ts = Either (ChatMdSetting m1 ts) (ChatMdSetting m2 ts)
type ChatParamsPerModel m1 m2 ts = Either (ChatParams m1 ts) (ChatParams m2 ts)

commandCat :: BotCommand
commandCat = BotCommand Cat $ botT $ do
  (msg, cid, uid, mid, sender) <- MaybeT $ getEssentialContent <$> query
  other_data <- query
  whole_chat <- query
  botmodules <- query
  botname    <- query
  botid      <- query
  mcatSetCommand <- (`MP.runParser` msg) <$> lift (commandParserTransformByBotName catSetParser)
  ConnectionManagerModuleRead man timeout <- lift askModule
  case mcatSetCommand of
    Just catSetCommand -> catSet catSetCommand
    Nothing -> do
      botSetting        <- lift $ fmap (fmap entityVal) . runMeowDB $ selectFirst [BotSettingBotId ==. botid] []
      botSettingPerChat <- lift $ fmap (fmap entityVal) . runMeowDB $ selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botid] []
      let sd = savedData other_data
      let activeChat = fromMaybe False $ asum
            [ botSettingPerChatActiveChat =<< botSettingPerChat
            , botSettingActiveChat =<< botSetting
            ] -- ^ whether to chat actively randomly
      let msys = ChatSetting
                   ( asum
                     [ fmap API.SystemMessage $ botSettingPerChatSystemMessage =<< botSettingPerChat
                     , systemMessage =<< lookup cid (chatSettings sd)
                     , fmap API.SystemMessage . globalSysMsg $ botmodules
                     , fmap API.SystemMessage $ botSettingSystemMessage =<< botSetting
                     ]
                   )
                   ( asum
                     [ botSettingPerChatSystemTemp =<< botSettingPerChat
                     , systemTemp =<< lookup cid (chatSettings sd)
                     , botSettingSystemTemp =<< botSetting
                     ]
                   )
                   ( asum
                     [ botSettingPerChatSystemMaxToolDepth =<< botSettingPerChat
                     , systemMaxToolDepth =<< lookup cid (chatSettings sd)
                     , botSettingSystemMaxToolDepth =<< botSetting
                     , Just 5
                     ]
                   )
                   ( asum
                     [ botSettingPerChatSystemAPIKey =<< botSettingPerChat
                     , systemApiKeys =<< lookup cid (chatSettings sd)
                     , botSettingSystemAPIKey =<< botSetting
                     ]
                   )
                   Nothing
      let modelCat = maybe modelCat runPersistUseShow (asum
            [ botSettingPerChatDefaultModel =<< botSettingPerChat
            , botSettingDefaultModel =<< botSetting
            ])
          modelSuperCat = maybe (DeepSeek DeepSeekReasoner) runPersistUseShow (asum
            [ botSettingPerChatDefaultModelS =<< botSettingPerChat
            , botSettingDefaultModelS =<< botSetting
            ])
          displayThinking = fromMaybe True $ asum
            [ botSettingPerChatDisplayThinking =<< botSettingPerChat
            , botSettingDisplayThinking =<< botSetting
            ]
      lChatModelMsg <- if activeChat
        then MaybeT . pure $ Nothing  -- disable cat command when active chat, we will use chat command instead
        else MaybeT . pure $ MP.runParser (treeCatParser botname msys mid) (getFirstTree whole_chat)

      needAction <- lift . runMeowDB $ serviceBalanceActionCheck botid cid
      breakAction <- case needAction of
        Nothing -> return $ Right []
        Just (DoNothing notis, winfo) -> do
          lift $ Right . concat <$> sequence [ checkSendNotis botname botid cid noti winfo | noti <- notis ]
        Just (DisableService notis, winfo) -> do
          lift $ Left . concat <$> sequence [ checkSendNotis botname botid cid noti winfo | noti <- notis ]

      LoggingRead logger <- lift askModule

      let addManager md cs = ChatParams md cs man timeout
          rlChatModelMsg = reverse lChatModelMsg -- the last message is on top
          params = fst . head $ rlChatModelMsg   -- take the last message model
          md = either chatMarkDown chatMarkDown (bimap ($ addManager) ($ addManager) params)  -- whether to use markdown
          ioEChatResponse = fmap (second Just) . runEffT00 . runLogging logger $ case params of

            Left  paramCat      ->
              case cfListPickElem modelsInUse (\(Proxy :: Proxy a) -> chatModel @a == modelCat) of
                Nothing ->
                  messagesChatDefault @ModelCat @MeowTools (coerce $ paramCat addManager) $ (map snd . reverse . take 20) rlChatModelMsg
                Just proxyCont -> proxyCont $ \(Proxy :: Proxy a) ->
                  messagesChatDefault @a @MeowTools (coerce $ paramCat addManager) $ (map snd . reverse . take 20) rlChatModelMsg

            Right paramSuperCat ->
              case cfListPickElem modelsInUse (\(Proxy :: Proxy a) -> chatModel @a == modelSuperCat) of
                Nothing ->
                  messagesChatDefault @ModelSuperCat @MeowTools (coerce $ paramSuperCat addManager) $ (map snd . reverse . take 20) rlChatModelMsg
                Just proxyCont -> proxyCont $ \(Proxy :: Proxy a) ->
                  messagesChatDefault @a @MeowTools (coerce $ paramSuperCat addManager) $ (map snd . reverse . take 20) rlChatModelMsg

      case breakAction of
        Left disableAndNotify -> do
          $logInfo "Service disabled due to insufficient balance"
          return disableAndNotify
        Right extraNotify -> do
          asyncAction <- liftIO $ actionSendMessages displayThinking md (msg, cid, uid, mid, sender) (return ()) ioEChatResponse
          $(logDebug) $ "created async: " <> T.pack (show asyncAction)
          return $ extraNotify <> [BAAsync asyncAction]

type UseMarkdown = Bool
type DisplayThinking = Bool
actionSendMessages
  :: DisplayThinking -> UseMarkdown
  -> EssentialContent
  -> Meow () -- ^ arbitrary action performed in Meow [BotAction] returned
  -> IO (Either Text [Message], Maybe ChatStatus)
  -> IO (Async (Meow [BotAction]))
actionSendMessages displayThink usemd essc@(_, cid, _, mid, _) act ioess = async $ do
  (ess, mstat) <- ioess
  let logStatAction = [BASimpleAction $ logBotStatistics cid (StatTokens stat) | Just stat <- [mstat]]
  fmap (logStatAction <>) <$> case ess of
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
                  waitAndDoRest <- liftIO . actionSendMessages displayThink usemd essc (return ()) $ do
                      liftIO $ threadDelay 2000000
                      return (Right rest, Nothing)
                  act
                  return $ send <> [BAAsync waitAndDoRest]
            else do
              send <- meowSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage msg] str
              waitAndDoRest <- liftIO . actionSendMessages displayThink usemd essc (return ()) $ do
                  liftIO $ threadDelay 2000000
                  return (Right rest, Nothing)
              act
              return $ send <> [BAAsync waitAndDoRest]
          AssistantMessage { thinking = Just think } -> do
            if displayThink
              then return $ do
                send <- meowSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage msg] $ "(..." <> T.strip think <> ")"
                waitAndDoRest <- liftIO . actionSendMessages displayThink usemd essc (return ()) $ do
                    liftIO $ threadDelay 2000000
                    return (Right $ msg { thinking = Nothing } : rest, Nothing)
                act
                return $ send <> [BAAsync waitAndDoRest]
              else
                casingMessages usemd essc $ msg { thinking = Nothing } : rest
          _ -> casingMessages usemd essc rest

catParser :: (Chars sb) => BotName -> ChatSetting ->
  Parser sb Char
    ( ChatSettingPerModel m1 m2 ts
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
      return (Left $ \cp -> cp False msys, str)
    parseCat = do
      MP.string botname
      MP.just ':' <|> MP.just '：'
      md <- MP.optBool $ $(MP.stringQ "md")
      modelStr <- $(MP.stringQ "cat") <|> $(MP.stringQ "supercat")
      MP.commandSeparator
      str <- MP.some MP.item
      case modelStr of
         "supercat" -> return . (, str) . Right $ \cp -> cp md (Just msys `chatSettingAlternative` def {systemTemp = Just 0.2})
         _          -> return . (, str) . Left  $ \cp -> cp md (Just msys `chatSettingAlternative` def {systemTemp = Just 0.7})
catParser (BotName Nothing) msys = do
  MP.spaces0
  parseCat <|> parseMeowMeow
  where
    parseMeowMeow = do
      $(MP.stringQ "喵喵")
      MP.commandSeparator2
      str <- MP.some MP.item
      return (Left $ \cp -> cp False msys, str)
    parseCat = do
      MP.just ':' <|> MP.just '：'
      md <- MP.optBool $ $(MP.stringQ "md")
      modelStr <- $(MP.stringQ "cat") <|> $(MP.stringQ "supercat")
      MP.commandSeparator
      str <- MP.some MP.item
      case modelStr of
         "supercat" -> return . (, str) . Right $ \cp -> cp md (Just msys `chatSettingAlternative` def {systemTemp = Just 0.2})
         _          -> return . (, str) . Left  $ \cp -> cp md (Just msys `chatSettingAlternative` def {systemTemp = Just 0.7})
replyCatParser :: (Chars sb) => BotName -> ChatSetting -> Parser sb Char (ChatSettingPerModel m1 m2 ts, String)
replyCatParser name msys = catParser name msys <|> ( do
  MP.spaces0
  str <- MP.some MP.item
  return (Left $ \cp -> cp False msys, str)
  )

treeCatParser :: forall s m1 m2 ts. (IsStream s CQMessage) => BotName -> ChatSetting -> Int -> Parser s CQMessage [(ChatSettingPerModel m1 m2 ts, Message)]
treeCatParser name msys mid = do
  elist  <- Right <$>
      MP.many (do
          umsg <- MP.satisfy (\cqm -> eventType cqm `elem` [GroupMessage, PrivateMessage])
          amsg <- MP.satisfy (\cqm -> eventType cqm == SelfMessage )
          msg <- maybe MP.zero return $ do
            mtmAmsg <- metaMessage amsg
            listToMaybe [ m | MMessage m <- metaMessageItems mtmAmsg ]
          let (params, metaUMsg) = fromMaybe (Left $ \cp -> cp False msys, "") $ MP.runParser (replyCatParser name msys) (extractMetaMessage umsg)
          return [ (params, UserMessage { content = T.pack metaUMsg })
                 , (params, msg)
                 ]
        ) :: Parser s CQMessage (Either (Maybe ChatSetting, [[(ChatSettingPerModel m1 m2 ts, Message)]]) [[(ChatSettingPerModel m1 m2 ts, Message)]])

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

