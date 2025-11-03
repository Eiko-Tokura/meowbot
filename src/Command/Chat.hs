{-# LANGUAGE TemplateHaskell, RecordWildCards, OrPatterns #-}
module Command.Chat where

import Command
import Command.Cat (catParser)
import MeowBot
import MeowBot.Prelude
import MeowBot.GetInfo
import MeowBot.BotStatistics
import External.ChatAPI as API
import External.ChatAPI.Tool
import External.ChatAPI.Tool.Search
import External.ChatAPI.Tool.Scrape
import External.ChatAPI.ModelPricing
import External.ChatAPI.Cost
import External.ChatAPI.MeowTool
import External.ChatAPI.MeowToolEnv
import qualified Data.BSeq as BSeq
import qualified MeowBot.Parser as MP
import qualified Data.Map.Strict as SM
import qualified Data.Text as T
import qualified Data.Foldable as Foldable

import Control.Applicative
import Control.Monad.Logger
import Module.RS
import Control.Monad.Effect
import Command.Cat.CatSet
import Command.Hangman

import Module.ConnectionManager
import Module.Logging
import Data.PersistModel
import Data.Proxy
import Data.HList
import Data.Coerce
import Data.Bifunctor
import MeowBot.CostModel
import MeowBot.Data.CQHttp.CQCode
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
  , CronTabTool
  , CronTabList
  , CronTabDelete
  , SetEssenceMessage
  , DeleteMessageTool
  , SetGroupBanTool
  , LeaveGroupTool
  , BlackListUserTool
  , BlackListListingTool
  ]

type ModelChat = Local QwQ

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
  -- find chat id whose oneOffActive = true
  let newChatState = SM.empty :: AllChatState
  allChatState <- lift $ getTypeWithDef newChatState
  let activeStateList = Foldable.find (\(_, cs) -> activeTriggerOneOff cs) (SM.toList allChatState)

  (mMsg, cid, mMid, mUid) <-
    case activeStateList of
      Nothing -> do -- trigger for the newest message
        (msg, cid, uid, mid, _sender) <- MaybeT $ getEssentialContent <$> query
        catParser' <- lift $ commandParserTransformByBotName catSetParser
        _ <- MaybeT $ pure $ (invertMaybe_ . (`MP.runParser` msg)) catParser'
        hangmanParser' <- lift $ commandParserTransformByBotName hangmanParser
        commandHead' <- lift $ commandParserTransformByBotName (MP.headCommand "" >> MP.itemsNotIn " ")
        -- let senderUsername  = senderNickname sender
        let qGroupManagerId = 2854196310
            ignoredPatterns = liftM2 (<|>)
              (fmap Left  . MP.runParser hangmanParser')
              (fmap Right . MP.runParser commandHead')
        _ <- MaybeT . pure $ invertMaybe_ $ ignoredPatterns msg
        guard $ uid /= qGroupManagerId
        return (Just msg, cid, Just mid, Just uid) -- trigger for the newest message
      Just (cid, _) -> do
        mess <- getEssentialContentChatId cid <$> query -- trigger for the specified cid instead
        return ( (\(m,_,_,_,_) -> m) <$> mess
               , cid
               , (\(_,_,_,mid,_) -> mid) <$> mess
               , Nothing
               )

  cqmsg  <- MaybeT $ queries getNewMsg
  cqmsg3 <- queries $ getNewMsgN 3
  other_data <- query
  -- whole_chat :: WholeChat <- query
  botmodules <- query
  botname    <- query
  botid      <- query
  utcTime    <- liftIO getCurrentTime
  ConnectionManagerModuleRead man timeout <- lift askModule
  noteListing <- lift $ getNoteListing botname cid
  botSetting        <- lift $ fmap (fmap entityVal) . runMeowDB $ getBy (UniqueBotId botid)
  botSettingPerChat <- lift $ fmap (fmap entityVal) . runMeowDB $ getBy (UniqueBotIdChatId botid cid)
  botUserBlackList  <- lift $ fmap (fmap entityVal) $ case mUid of
    Just uid' -> runMeowDB $ selectFirst
        [ BotUserBlackListBotId ==. botid
        , BotUserBlackListUserId ==. uid'
        , BotUserBlackListChatId ==. Just cid
        ] []
    Nothing   -> pure Nothing
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
        ( API.SystemMessage . appendNoteListing . appendCQHelp <$> asum
          [ botSettingPerChatSystemMessage =<< botSettingPerChat
          , fmap content . systemMessage =<< lookup cid (chatSettings sd)
          , globalSysMsg botmodules
          , botSettingSystemMessage =<< botSetting
          , Just "You are the helpful, endearing catgirl assistant named '喵喵'. You, 喵喵 is chatting with people in a lively group chat. You, 喵喵, have a warm, playful personality and always aim to provide natural, cute, and engaging responses to everyone. You, 喵喵 adore using whisker-twitching symbols like 'owo', '>w<', 'qwq', 'T^T', and the unique cat symbol '[CQ:face,id=307]' to add a delightful touch to your messages. 喵喵 is friendly, spontaneous, and keep the conversation light and enjoyable for all participants. Your program is written by Eiko (754829466) in Haskell, if user have technical questions about the bot, direct them to use :help command or ask Eiko directly."
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
      mentionReply = fromMaybe False $ asum
        [ botSettingPerChatMentionReply =<< botSettingPerChat
        , botSettingMentionReply =<< botSetting
        ] -- ^ whether to reply when mentioned
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
      modelCat = maybe modelCat runPersistUseShow (asum
        [ botSettingPerChatDefaultModel =<< botSettingPerChat
        , botSettingDefaultModel =<< botSetting
        ])

      blackListValid  = maybe True  (>= utcTime) (botUserBlackListValidTo =<< botUserBlackList)
      blackListed     = blackListValid && maybe False botUserBlackListBlackListed    botUserBlackList
      ignoredReaction = blackListValid && maybe False botUserBlackListIgnoreReaction botUserBlackList

      -- | If a text is empty, make it Nothing
      nullify :: Maybe Text -> Maybe Text
      nullify (Just s) | T.null s = Nothing
      nullify x = x

      toUserMessage :: CQMessage -> Message
      toUserMessage cqm = UserMessage $ mconcat $ catMaybes
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
      selectedContent (Left (CQImage _):rest)     = "[CQ:image,url=<too_long>]"  <> selectedContent rest
      selectedContent (Left (CQRecord _):rest)    = "[CQ:record,url=<too_long>]" <> selectedContent rest
      selectedContent (Left cq@(CQAt {}):rest)    = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQReply {}):rest) = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQOther "face" _):rest)     = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQOther "markdown" _):rest) = embedCQCode cq <> selectedContent rest
      selectedContent (Left cq@(CQOther "json" _):rest)     = embedCQCode cq <> selectedContent rest
      selectedContent (Left (CQOther "image" meta):rest)
        = case filter (flip elem ["summary"] . fst) meta of
            []       -> "[CQ:image,data=<unknown_data>]"
            filtered -> embedCQCode (CQOther "image" filtered)
        <> selectedContent rest
      selectedContent (Left (CQOther "mface" meta):rest)
        = case filter (flip elem ["summary"] . fst) meta of
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
      cqFilter c                   = Just c

      updateCurrentChatState :: (ChatState -> ChatState) -> AllChatState -> AllChatState
      updateCurrentChatState f s =
        let mstate = SM.lookup cid s in
        case mstate of
          Just cs -> SM.insert cid (f cs) s
          Nothing -> SM.insert cid (f def { chatStatus = ChatStatus 0 0 [] mempty }) s

      recordReplyTime :: UTCTime -> ChatState -> ChatState
      recordReplyTime utcTime cs =
        cs { replyTimes = BSeq.bSeqCons utcTime (replyTimes cs) }

      updateChatState :: AllChatState -> (Bool, AllChatState)
      updateChatState s =
        let mstate = SM.lookup cid s in
        case mstate of
          Just cs -> case activeTriggerOneOff cs of
            True -> (True,) $ SM.insert cid
              cs
                { chatStatus = (chatStatus cs)
                  { chatStatusMessages = optimalMeowTakeTailKeepAvg maxMessageInState $ chatStatusMessages (chatStatus cs) -- active trigger, not append
                  , chatStatusToolDepth = 0 -- ^ reset tool depth
                  , chatEstimateTokens = mempty
                  }
                , activeTriggerOneOff = False
                }
              s
            False -> (False,) $ SM.insert cid
              cs
                { chatStatus = (chatStatus cs)
                  { chatStatusMessages = optimalMeowTakeTailKeepAvg maxMessageInState $ chatStatusMessages (chatStatus cs) ++ [toUserMessage cqmsg]
                  , chatStatusToolDepth = 0 -- ^ reset tool depth
                  , chatEstimateTokens = mempty
                  }
                , activeTriggerOneOff = False
                }
              s
          Nothing -> (False, SM.insert cid def { chatStatus =  ChatStatus 0 0 [toUserMessage cqmsg] mempty } s)

      params = ChatParams False msys man timeout :: ChatParams ModelChat MeowTools

      --notCatSet = isNothing $ MP.runParser catSetParser msg

  MaybeT $ if activeChat && not blackListed then pure (Just ()) else pure Nothing
  -- ^ only chat when set to active
  $(logDebug) "Chat command is active"

  (oneOffActive, allChatState) <- lift $ updateChatState <$> getTypeWithDef newChatState
  -- ^ get the updated chat state

  lift $ putType allChatState
  -- ^ update in the state

  chatState <- MaybeT . pure $ SM.lookup cid allChatState

  when oneOffActive $ $(logInfo) $ "One off active, replying to " <> tshow cid

  -- determine If we should reply
  $(logDebug) "Determining if reply"
  determineIfReply oneOffActive atReply mentionReply ignoredReaction activeProbability cid cqmsg3 mMsg botname msys chatState utcTime
  $(logInfo) "Replying"

  breakAction <- lift . runMeowDB $ serviceBalanceGenerateActionCheckNotification botname botid cid
  LoggingRead logger <- lift (askModule @LoggingModule)

  case breakAction of
    Left disableAndNofity -> do
      $logInfo "Service disabled due to insufficient balance"
      return disableAndNofity
    Right extraAction -> do
      ioeResponse <- pure . runEffT00 . runLogging (liftLogger lift logger) . runModelPricing (ModelPricingRead modelPrice) . runSModule (chatStatus chatState) . fmap (first $ toText @_ @Text) . errorToEitherAll $
        case cfListPickElem modelsInUse (\(Proxy :: Proxy a) -> chatModel @a == modelCat) of
          Nothing ->
            statusChatReadAPIKey @ModelChat @MeowTools @MeowToolEnvDefault (coerce params)
          Just proxyCont -> proxyCont $ \(Proxy :: Proxy a) ->
            statusChatReadAPIKey @a @MeowTools (coerce params)

      logger <- askLoggerIO

      asyncAction <- lift
                      . embedError
                      . fmap (fmap
                         (\case
                           (RSuccess a, _) -> a
                           (RFailure elist, _) -> effThrowEList elist
                         )
                        )
                      . asyncEffT $
        (do
          (eNewMsg, newStatus) <- ioeResponse
          case eNewMsg of
            Left err -> do -- responded with error message
              flip runLoggingT logger $ $(logError) $ "Error: " <> err
              return $ do
                markMeow cid MeowIdle -- ^ update status to idle
                logBotStatistics cid (StatTokens newStatus)
                pure [] -- ^ do nothing
            Right newMsgs' -> do -- responded with new messages
              let newMsgs = map (mapMessage (MP.filterOutputTags ["role", "msg_id", "username", "nickname", "user_id"] . MP.cqcodeFix cqFilter)) newMsgs'
              return $ do
                markMeow cid MeowIdle -- ^ update status to idle
                logBotStatistics cid (StatTokens newStatus)
                mergeChatStatus maxMessageInState cid newMsgs newStatus
                let splitedMessageToSend = map (T.intercalate "\n---\n") . chunksOf 2 $ concatMap
                      (\case
                        AssistantMessage -- Case 1 : assistant returned pure tool call
                          { pureToolCall = Just True
                          , content      = c
                          , thinking     = mt
                          } -> [t | displayThinking, Just t <- [mt]] <> [c | displayToolMessage]
                        AssistantMessage -- Case 2 : assistant returned a message with tool call
                          { content  = c
                          , thinking = mt
                          , withToolCall = Just (_, rest)
                          } -> [t | displayThinking, Just t <- [mt]]
                            <> [if displayToolMessage then c else rest $ const "(Casting Spell...)"]
                        AssistantMessage -- Case 3 : assistant returned a message with no tool call
                          { content  = c
                          , thinking = mt
                          } -> [t | displayThinking, Just t <- [mt]] <> [c]
                        ToolMessage
                          { content = c
                          } -> [c | displayToolMessage]
                        m   -> [content m]
                      ) newMsgs
                meowAsyncSplitSendToChatIdFull cid mMid []
                  ([MReplyTo mid | Just mid <- pure mMid] <> [MMessage (last newMsgs)])
                  2_000_000 splitedMessageToSend
        )
      -- update busy status
      lift $ markMeow cid MeowBusy
      -- record the reply time
      lift $ updateCurrentChatState (recordReplyTime utcTime) <$> getTypeWithDef newChatState

      return $ extraAction <> [BAAsync asyncAction] -- Async (Meow [BotAction])
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

notAllNoText :: [CQMessage] -> Bool
notAllNoText = not . all (all
  (\case -- this case determines if a term Either CQCode Text contains no text
    Left _  -> True
    Right t -> T.null $ T.strip t
  ) . maybe [] mixedMessage . metaMessage)

determineIfReply :: Bool -> Bool -> Bool -> Bool -> Double -> ChatId -> [CQMessage] -> Maybe Text -> BotName -> ChatSetting -> ChatState -> UTCTime -> MaybeT (MeowT Mods IO) ()
determineIfReply True _ _ _ _ _ _ _ _ _ ChatState {meowStatus = MeowIdle} _ = MaybeT . pure $ Just ()
determineIfReply oneOff atReply mentionReply ignoredReaction prob GroupChat{} cqmsgs (Just msg) bn cs st@(ChatState {meowStatus = MeowIdle}) utc = do
  chance   <- getUniformR (0, 1 :: Double)
  chance2  <- getUniformR (0, 1 :: Double)
  lift $ $(logDebug) $ "Chance: " <> tshow chance
  replied <- lift $ boolMaybe <$> beingReplied
  let mentioned = boolMaybe $ mentionReply && case bn of
        BotName (Just name) -> T.isInfixOf (T.pack name) msg
        _                   -> False
  let thrSeconds = 300
      thrReplyCount = 3
      notIgnoredReaction = boolMaybe $ not ignoredReaction
  let -- | if last 180 seconds there are >= 4 replies, decrease the chance to reply exponentially
      recentReplyCount = length (filter
                          (\t -> diffUTCTime utc t < thrSeconds) -- last 180 seconds
                          (Foldable.toList (replyTimes st))
                          )
      rateLimitChance | recentReplyCount >= thrReplyCount + 1
        = Just
        $ 0.5 ** (fromIntegral recentReplyCount - fromIntegral thrReplyCount)
                      | otherwise = Nothing
      notRateLimited = (boolMaybe . not) (maybe False (< chance2) rateLimitChance)
  lift $ $(logDebug) $ T.intercalate "\n"
    [ "recentTimes: " <> tshow (replyTimes st)
    , "recentReplyCount: " <> tshow recentReplyCount
    , "rateLimitChance: " <> tshow rateLimitChance
    , "notRateLimited: " <> tshow notRateLimited
    ]
  case notRateLimited of
    Nothing -> lift $ $(logInfo) $ T.intercalate "\n"
      [ "Rate limited, not replying to " <> tshow cqmsgs
      , "recentReplyCount: " <> tshow recentReplyCount
      , "rateLimitChance: " <> tshow rateLimitChance
      ]
    _       -> pure ()

  ated    <- if atReply
    then lift $ boolMaybe <$> beingAt
    else return Nothing
  let chanceReply = do -- chance reply only happens when recent messages contains some text
        boolMaybe $ chance <= prob
        boolMaybe $ notAllNoText cqmsgs
      parsed = void $ MP.runParser (catParser bn cs) msg
  MaybeT . pure $ chanceReply <|> parsed <|> replied <|> mentioned <|> ated <|> boolMaybe oneOff
  MaybeT . pure $ notRateLimited
  MaybeT . pure $ notIgnoredReaction
determineIfReply _ _ _ _ _ PrivateChat{} _ (Just msg) _ _ ChatState {meowStatus = MeowIdle} _ = do
  if T.isPrefixOf ":" msg
  then MaybeT . pure $ Nothing
  else MaybeT . pure $ Just ()
determineIfReply _ _ _ _ _ _ _ _ _ _ _ _ = empty
