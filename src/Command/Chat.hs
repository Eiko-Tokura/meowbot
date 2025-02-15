{-# LANGUAGE TemplateHaskell #-}
module Command.Chat where

import Command
import Command.Cat (catParser)
import MeowBot
import MeowBot.GetInfo
import System.General (MeowT)
import Data.Maybe (fromMaybe, catMaybes)
import External.ChatAPI
import External.ChatAPI as API
import External.ChatAPI.Tool
import External.ChatAPI.Tool.Search
import External.ChatAPI.Tool.Scrape
import External.ChatAPI.MeowTool
import External.ChatAPI.MeowToolEnv
import qualified MeowBot.Parser as MP
import qualified Data.Map.Strict as SM
import qualified Data.Text as T

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState
import Command.Cat.CatSet
import Command.Hangman

import Module.ConnectionManager
import Data.PersistModel
import Data.Proxy
import Data.HList
import Data.Coerce
import MeowBot.CQCode
import Utils.RunDB
import Utils.Maybe
import Utils.Persist
import Utils.List
import Data.Additional.Default

import Probability.Foundation

type MeowTools =
  [ TimeTool
  , SkipTool
  , ActionTool
  , NoteToolAdd
  , NoteToolRead
  , NoteToolDelete
  , ScrapeTool
  , SearchTool
  ]

type ModelChat = Local DeepSeekR1_14B

-- | A new command that enables the bot to chat with users
-- should be more powerful than the previous legacy command Cat
-- maybe we will eventually deprecate the old command Cat
--
-- the idea :
-- * Every chat_id will maintain its state
-- * Context will be given
-- * Model has the ability to use tools, taking notes
-- - might consider: ability to initiate new messages
--     In current model, since meowmeow is based on a event-driven model, in principle most commands are run in response to a message
--
-- * Chat command is not enabled by default, it should be enabled by the user
--   it will randomly interact with the user in a group chat in low probability
--   but when being called (@meowmeow or called by command), it will respond 100%
--
-- What we will do first is to try this in private chat, providing note taking and scheduled message
commandChat :: BotCommand
commandChat = BotCommand Chat $ botT $ do
  (msg, cid, _, mid, _) <- MaybeT $ getEssentialContent <$> query
  _ <- MaybeT    $ invertMaybe_ . (`MP.runParser` msg) <$> commandParserTransformByBotName catSetParser
  hangmanParser' <- lift $ commandParserTransformByBotName hangmanParser
  let ignoredPatterns = MP.runParser hangmanParser'
  _ <- pureMaybe $ invertMaybe_ $ ignoredPatterns msg
  cqmsg <- queries getNewMsg
  other_data <- query
  -- whole_chat :: WholeChat <- query
  botmodules <- query
  botname    <- query
  botid      <- query
  ConnectionManagerModuleG man timeout <- query
  noteListing <- lift $ getNoteListing botname cid
  botSetting        <- lift $ fmap (fmap entityVal) . runDB $ getBy (UniqueBotId botid)
  botSettingPerChat <- lift $ fmap (fmap entityVal) . runDB $ getBy (UniqueBotIdChatId botid cid)
  let appendNoteListing :: Text -> Text
      appendNoteListing t = case noteListing of
        Nothing       -> t
        Just noteList -> t <> "\n\n---\n\nNotes:\n" <> noteList
      appendCQHelp :: Text -> Text
      appendCQHelp t = t <> "\n\n---\n\n" <> T.intercalate "\n"
        [ "You can include '[CQ:reply,id=<msg_id>]' (optional, at most one) to reply to a particular message with given msg_id. The person who is replied to will be notified."
        , "You can include '[CQ:at,qq=<user_id>]' (optional, unlimited number) to mention a particular user with given user_id, they will be notified."
        ]
      msys = ChatSetting
        ( fmap (API.SystemMessage . appendNoteListing . appendCQHelp) $ asum
          [ botSettingPerChatSystemMessage =<< botSettingPerChat
          , fmap content . systemMessage =<< lookup cid (chatSettings sd)
          , globalSysMsg $ botmodules
          , botSettingSystemMessage =<< botSetting
          , Just $ "You are the helpful, endearing catgirl assistant named '喵喵'. You, 喵喵 is chatting with people in a lively group chat. You, 喵喵, have a warm, playful personality and always aim to provide natural, cute, and engaging responses to everyone. You, 喵喵 adore using whisker-twitching symbols like 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]' to add a delightful touch to your messages. 喵喵 is friendly, spontaneous, and keep the conversation light and enjoyable for all participants."
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
      sd = savedData other_data
      displayThinking = fromMaybe False $ asum
        [ botSettingPerChatDisplayThinking =<< botSettingPerChat
        , botSettingDisplayThinking =<< botSetting
        ] -- ^ whether to display thinking message
      displayToolMessage = fromMaybe False $ asum
        [ botSettingPerChatDisplayToolMessage =<< botSettingPerChat
        , botSettingDisplayToolMessage =<< botSetting
        ] -- ^ whether to display tool message
      activeChat = fromMaybe False $ asum
        [ botSettingPerChatActiveChat =<< botSettingPerChat
        , botSettingActiveChat =<< botSetting
        ] -- ^ whether to chat actively randomly
      atReply = fromMaybe True $ asum
        [ botSettingPerChatAtReply =<< botSettingPerChat
        , botSettingAtReply =<< botSetting
        ] -- ^ whether to reply when ated
      activeProbability = fromMaybe 0.013 $ asum
        [ botSettingPerChatActiveProbability =<< botSettingPerChat
        , botSettingActiveProbability =<< botSetting
        ] -- ^ probability to chat actively
      maxMessageInState = fromMaybe 24 $ asum
        [ botSettingPerChatMaxMessageInState =<< botSettingPerChat
        , botSettingMaxMessageInState =<< botSetting
        ] -- ^ maximum number of messages to keep in state
      modelCat = fromMaybe modelCat $ runPersistUseShow <$> asum
        [ botSettingPerChatDefaultModel =<< botSettingPerChat
        , botSettingDefaultModel =<< botSetting
        ]
      newChatState = SM.empty :: AllChatState

      nullify :: Maybe Text -> Maybe Text
      nullify (Just s) | T.null s = Nothing
      nullify x = x

      toUserMessage :: CQMessage -> Message
      toUserMessage cqm = UserMessage $ mconcat $ catMaybes $
        [ fmap (\r -> "<role>" <> r <> "</role>") (roleToText =<< senderRole =<< sender cqm)
        , fmap (\t -> "<msg_id>" <> toText t <> "</msg_id>") (messageId cqm)
        , fmap (\(UserId uid) -> "<user_id>" <> toText uid <> "</user_id>") (userId cqm)
        , fmap (\t -> "<username>" <> t <> "</username>") (senderNickname =<< sender cqm)
        , fmap (\t -> "<nickname>" <> t <> "</nickname>") (nullify $ senderCard =<< sender cqm)
        , Just ": "
        , selectedContent . mixedMessage <$> metaMessage cqm
        ]

      selectedContent :: [Either CQCode Text] -> Text
      selectedContent []                          = ""
      selectedContent (Right t:rest)              = t <> selectedContent rest
      selectedContent (Left (CQImage _):rest)     = "[CQ:image,url=<too_long>]" <> selectedContent rest
      selectedContent (Left (CQRecord _):rest)    = "[CQ:record,url=<too_long>]" <> selectedContent rest
      selectedContent (Left cq@(CQAt {}):rest)    = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQReply {}):rest) = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQOther "face" _):rest)     = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQOther "markdown" _):rest) = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQOther "json" _):rest)     = embedCQCode cq <> selectedContent rest
      selectedContent (Left (CQOther "image" meta):rest)
        = case (filter (flip elem ["summary"] . fst) meta) of
            []       -> "[CQ:image,data=<unknown_data>]"
            filtered -> embedCQCode (CQOther "image" filtered)
        <> selectedContent rest
      selectedContent (Left (CQOther "mface" meta):rest)
        = case (filter (flip elem ["summary"] . fst) meta) of
            []       -> "[CQ:mface,data=<unknown_data>]"
            filtered -> embedCQCode (CQOther "mface" filtered)
        <> selectedContent rest
      selectedContent (Left (CQOther cqtype _):rest)
        = "[CQ:" <> cqtype <> ",data=<unknown_data>]"
        <> selectedContent rest
      selectedContent (Left _:rest) = selectedContent rest

      cqFilter :: CQCode -> Maybe CQCode
      cqFilter (CQImage _)         = Nothing
      cqFilter (CQOther "image" _) = Nothing
      cqFilter c@_                 = Just c

      updateChatState :: AllChatState -> AllChatState
      updateChatState s =
        let mstate = SM.lookup cid s in
        case mstate of
          Just cs -> SM.insert cid
            cs
              { chatStatus = (chatStatus cs)
                { chatStatusMessages = strictTakeTail maxMessageInState $ chatStatusMessages (chatStatus cs) ++ [toUserMessage cqmsg]
                , chatStatusToolDepth = 0 -- ^ reset tool depth
                }
              }
            s
          Nothing -> SM.insert cid (ChatState (ChatStatus 0 0 [toUserMessage cqmsg]) MeowIdle) s

      params = ChatParams False msys man timeout :: ChatParams ModelChat MeowTools

      --notCatSet = isNothing $ MP.runParser catSetParser msg

  MaybeT $ if activeChat then pure (Just ()) else pure Nothing
  -- ^ only chat when set to active
  $(logDebug) "Chat command is active"

  allChatState <- lift $ updateChatState <$> getTypeWithDef newChatState
  -- ^ get the updated chat state

  lift $ putType $ allChatState
  -- ^ update in the state

  chatState <- pureMaybe $ SM.lookup cid allChatState

  $(logDebug) "Determining if reply"
  determineIfReply atReply activeProbability cid msg botname msys chatState
  $(logInfo) "Replying"

  ioeResponse <- lift . embedMeowToolEnv . toIO $
    case (cfListPickElem modelsInUse (\(Proxy :: Proxy a) -> chatModel @a == modelCat)) of
      Nothing ->
        statusChatReadAPIKey @ModelChat @MeowTools @MeowToolEnvDefault (coerce params) $ chatStatus chatState
      Just proxyCont -> proxyCont $ \(Proxy :: Proxy a) ->
        statusChatReadAPIKey @a @MeowTools (coerce params) $ chatStatus chatState

  logger <- askLoggerIO

  asyncAction <- liftIO $ do
    async $ do
      (eNewMsg, newStatus) <- ioeResponse
      case eNewMsg of
        Left err -> do -- responded with error message
          flip runLoggingT logger $ $(logError) $ "Error: " <> err
          return $ do
            markMeow cid MeowIdle -- ^ update status to idle
            pure [] -- ^ do nothing
        Right newMsgs' -> do -- responded with new messages
          let newMsgs = map (mapMessage (MP.filterOutputTags ["role", "msg_id", "username", "nickname", "user_id"] . MP.cqcodeFix cqFilter)) newMsgs'
          return $ do
            markMeow cid MeowIdle -- ^ update status to idle
            mergeChatStatus maxMessageInState cid newMsgs newStatus
            let splitedMessageToSend = map (T.intercalate "\n---\n") . chunksOf 2 $ concatMap
                  (\case
                    AssistantMessage
                      { pureToolCall = Just True
                      , content      = c
                      , thinking     = mt
                      } -> [t | displayThinking, Just t <- [mt]] <> [c | displayToolMessage]
                    AssistantMessage
                      { content  = c
                      , thinking = mt
                      } -> [t | displayThinking, Just t <- [mt]] <> [c]
                    ToolMessage
                      { content = c
                      } -> [c | displayToolMessage]
                    m   -> [content m]
                  ) newMsgs
            meowAsyncSplitSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage (last newMsgs)] 2000 splitedMessageToSend

  -- update busy status
  lift $ markMeow cid MeowBusy

  return [BAAsync asyncAction] -- Async (Meow [BotAction])
  -- if you need to send multiple messages, you can use
  -- Async (Meow [send_msg_1, BAAsync (Async (Meow [send_msg_2]))])
  -- m a -> Async (m

markMeow :: ChatId -> MeowStatus -> Meow ()
markMeow cid meowStat = do
  allChatState <- getTypeWithDef SM.empty
  let mchatState = SM.lookup cid allChatState
  case mchatState of
    Nothing -> pure ()
    Just chatState -> do
      putType $ SM.insert cid chatState { meowStatus = meowStat } allChatState
      $(logDebug) $ "Marked meow status to " <> tshow meowStat

mergeChatStatus :: Int -> ChatId -> [Message] -> ChatStatus -> Meow ()
mergeChatStatus maxMessageInState cid newMsgs newStatus = do
  allChatState <- getTypeWithDef SM.empty
  let mchatState = SM.lookup cid allChatState
  case mchatState of
    Nothing -> pure ()
    Just chatState ->
      putType $ SM.insert cid
        chatState -- adding new messages to newest state
          { chatStatus = newStatus
            { chatStatusMessages = strictTakeTail maxMessageInState $ chatStatusMessages (chatStatus chatState) ++ newMsgs
            }
          }
        allChatState

determineIfReply :: Bool -> Double -> ChatId -> Text -> BotName -> ChatSetting -> ChatState -> MaybeT (MeowT MeowData Mods IO) ()
determineIfReply atReply prob GroupChat{} msg bn cs ChatState {meowStatus = MeowIdle} = do
  chance  <- getUniformR (0, 1 :: Double)
  lift $ $(logDebug) $ "Chance: " <> tshow chance
  replied <- if atReply
    then lift $ boolMaybe <$> beingReplied
    else return Nothing
  ated    <- lift $ boolMaybe <$> beingAt
  let chanceReply = boolMaybe $ chance <= prob
      parsed = void $ MP.runParser (catParser bn cs) msg
  pureMaybe $ chanceReply <|> parsed <|> replied <|> ated
determineIfReply _ _ PrivateChat{} msg _ _ ChatState {meowStatus = MeowIdle} = do
  if T.isPrefixOf ":" msg
  then pureMaybe Nothing
  else pureMaybe $ Just ()
determineIfReply _ _ _ _ _ _ _ = empty
