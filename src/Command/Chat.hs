{-# LANGUAGE TemplateHaskell, RecordWildCards, OrPatterns #-}
module Command.Chat where

import Command
import Command.Cat (catParser)
import MeowBot
import MeowBot.Prelude
import MeowBot.GetInfo as Info
import MeowBot.BotStatistics
import External.ChatAPI as API
import External.ChatAPI.Tool
import External.ChatAPI.Tool.Search
import External.ChatAPI.Tool.Scrape
import External.ChatAPI.ModelPricing
import External.ChatAPI.Cost
import External.ChatAPI.MeowTool
import External.ChatAPI.MeowToolEnv
import qualified MeowBot.Parser as MP
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Foldable as Foldable

import Control.Applicative
import Control.Monad.Logger
import Module.RS
import Control.Monad.Effect
import Command.Cat.CatSet
import Command.Hangman

import Module.BotGlobal
import Module.ConnectionManager
import Module.Logging
import Data.PersistModel
import Data.Proxy
import Data.HList
import Data.Coerce
import Data.Bifunctor
import Data.BSeq
import MeowBot.ChatBot.Types
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
  allChatState <- lift $ getTypeWithDef newChatState
  let activeStateList = Foldable.find (activeTriggerOneOff . snd) (HM.toList allChatState)

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
  inGlobalBots <- case mUid of
    Just uid' -> lift $ userIdInGlobalBots uid'
    Nothing   -> lift $ return False
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
  userHasPositiveCostModel <- lift $ Info.hasPositiveCostModel cid
  botSetting        <- lift $ fmap (fmap entityVal) . runMeowCoreDB $ getBy (UniqueBotId botid)
  botSettingPerChat <- lift $ fmap (fmap entityVal) . runMeowCoreDB $ getBy (UniqueBotIdChatId botid cid)
  botUserBlackList  <- lift $ fmap (fmap entityVal) $ case mUid of
    Just uid' -> runMeowCoreDB $ selectFirst
        [ BotUserBlackListBotId ==. botid
        , BotUserBlackListUserId ==. uid'
        , BotUserBlackListChatId ==. Just cid
        ]
        [ Desc BotUserBlackListTime ]
    Nothing   -> pure Nothing
  let appendNoteListing :: Text -> Text
      appendNoteListing t = case noteListing of
        Nothing       -> t
        Just noteList -> t <> "\n\n---\n\nNotes:\n" <> noteList
      appendCQHelp :: Text -> Text
      appendCQHelp t = t <> "\n\n---\n\n" <> T.intercalate "\n"
        [ "You can include '[CQ:reply,id=<msg_id>]' (optional, at most one) to reply to a particular message with given msg_id. The person being replied to will be notified."
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

      readBotSettingField :: a -> (b -> a) -> (BotSettingPerChat -> Maybe b) -> (BotSetting -> Maybe b) -> a
      readBotSettingField def apl f g = maybe def apl $ (f =<< botSettingPerChat) <|> (g =<< botSetting)

      displayThinking    = readBotSettingField False id botSettingPerChatDisplayThinking    botSettingDisplayThinking
      displayToolMessage = readBotSettingField False id botSettingPerChatDisplayToolMessage botSettingDisplayToolMessage
      activeChat         = readBotSettingField False id botSettingPerChatActiveChat         botSettingActiveChat
      mentionReply       = readBotSettingField False id botSettingPerChatMentionReply       botSettingMentionReply
      atReply            = readBotSettingField True  id botSettingPerChatAtReply            botSettingAtReply
      activeProbability  = readBotSettingField 0.013 id botSettingPerChatActiveProbability  botSettingActiveProbability
      maxMessageInState  = readBotSettingField 24    id botSettingPerChatMaxMessageInState  botSettingMaxMessageInState
      multiResponse      = readBotSettingField False id botSettingPerChatMultiResponse      botSettingMultiResponse
      withTimeStamp      = readBotSettingField False id botSettingPerChatWithTimeStamp      botSettingWithTimeStamp
      modelCat           = readBotSettingField modelCat runPersistUseShow botSettingPerChatDefaultModel botSettingDefaultModel

      blackListValid  = maybe True  (>= utcTime) (botUserBlackListValidTo =<< botUserBlackList)
      blackListed     = blackListValid && maybe False botUserBlackListBlackListed    botUserBlackList
      ignoredReaction = blackListValid && maybe False botUserBlackListIgnoreReaction botUserBlackList

      cqFilter :: CQCode -> Maybe CQCode
      cqFilter (CQImage _)         = Nothing
      cqFilter (CQOther "image" _) = Nothing
      cqFilter c                   = Just c

      params = ChatParams False msys man timeout :: ChatParams ModelChat MeowTools

      toMessageConf = ToUserMessageConfig
        { withUtcTime = if withTimeStamp then Just utcTime else Nothing
        }

  guardMaybeT $ activeChat && not blackListed
  -- ^ only chat when set to active
  $(logDebug) "Chat command is active"

  (oneOffActive, allChatState) <- lift $ updateAllChatStateTrigger maxMessageInState cid toMessageConf cqmsg <$> getTypeWithDef newChatState
  -- ^ get the updated chat state

  lift $ putType allChatState
  -- ^ update in the state

  chatState <- MaybeT . pure $ HM.lookup cid allChatState

  when oneOffActive $ $(logInfo) $ "One off active, replying to " <> tshow cid

  -- determine If we should reply
  $(logDebug) "Determining if reply"

  let determineIfReplyData = DetermineReplyData
        { shouldNotReply           = inGlobalBots
        , oneOffActive             = oneOffActive
        , atReply                  = atReply
        , mentionReply             = mentionReply
        , ignoredReaction          = ignoredReaction
        , activeProbability        = activeProbability
        , userHasPositiveCostModel = userHasPositiveCostModel
        , cid                      = cid
        , chatTimeSequence         = chatState.replyTimes
        , meowStatus               = chatState.meowStatus
        , multiResponse            = multiResponse
        , chatSetting              = msys
        , utcTime                  = utcTime
        }
  determineIfReply determineIfReplyData cqmsg3 mMsg botname

  $(logInfo) "Replying"

  breakAction <- lift . runMeowCoreDB $ serviceBalanceGenerateActionCheckNotification botname botid cid
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
                modifyAllChatState $ updateChatState cid (mergeChatState maxMessageInState newMsgs)
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
      lift $ modifyAllChatState (updateChatState cid $ recordReplyTime utcTime)

      return $ extraAction <> [BAAsync asyncAction] -- Async (Meow [BotAction])
      -- if you need to send multiple messages, you can use
      -- Async (Meow [send_msg_1, BAAsync (Async (Meow [send_msg_2]))])
      -- m a -> Async (m

modifyAllChatState :: (HM.HashMap ChatId ChatState -> HM.HashMap ChatId ChatState) -> Meow ()
modifyAllChatState f = putType . f =<< getTypeWithDef newChatState

markMeow :: ChatId -> MeowStatus -> Meow ()
markMeow cid meowStat = modifyAllChatState $ markMeowStatus cid meowStat

notAllNoText :: [CQMessage] -> Bool
notAllNoText = not . all (all
  (\case -- this case determines if a term Either CQCode Text contains no text
    Left _  -> True
    Right t -> T.null $ T.strip t
  ) . maybe [] mixedMessage . metaMessage)

data DetermineReplyData = forall length. DetermineReplyData
  { shouldNotReply           :: Bool
  , oneOffActive             :: Bool
  , atReply                  :: Bool
  , mentionReply             :: Bool
  , ignoredReaction          :: Bool
  , userHasPositiveCostModel :: Bool
  , multiResponse            :: Bool
  , activeProbability        :: Double
  , cid                      :: ChatId
  , chatTimeSequence         :: BSeq length UTCTime
  , meowStatus               :: MeowStatus
  , chatSetting              :: ChatSetting
  , utcTime                  :: UTCTime
  }

notLocked :: DetermineReplyData -> Bool
notLocked DetermineReplyData {..} | MeowIdle <- meowStatus = True
notLocked DetermineReplyData {..} | multiResponse = True
notLocked _ = False

type ShouldNotReply = Bool
determineIfReply :: DetermineReplyData -> [CQMessage] -> Maybe Text -> BotName -> MaybeT (MeowT Mods IO) ()
determineIfReply (shouldNotReply -> True) _ _ _ = MaybeT . pure $ Nothing
determineIfReply (oneOffActive   -> True) _ _ _ = MaybeT . pure $ Just ()
determineIfReply d@DetermineReplyData {..} cqmsgs (Just msg) bn | GroupChat{} <- cid, notLocked d = do
  chance   <- getUniformR (0, 1 :: Double)
  chance2  <- getUniformR (0, 1 :: Double)
  lift $ $(logDebug) $ "Chance: " <> tshow chance
  replied <- lift beingReplied
  let mentioned = case bn of
        BotName (Just name) -> T.isInfixOf (T.pack name) msg
        _                   -> False
  let thrSeconds    = 150
      thrReplyCount = if userHasPositiveCostModel then 7 else 4
  let -- | if last 150 seconds there are >= 5 replies, decrease the chance to reply exponentially
      recentReplyCount = length (filter
                          (\t -> diffUTCTime utcTime t < thrSeconds) -- last 180 seconds
                          (Foldable.toList chatTimeSequence)
                          )
      notRateLimitChance | recentReplyCount >= thrReplyCount + 1
        = Just
        $ 0.7 ** (fromIntegral recentReplyCount - fromIntegral thrReplyCount)
                      | otherwise = Nothing
      rateLimited = maybe False (< chance2) notRateLimitChance
  lift $ $(logDebug) $ T.intercalate "\n"
    [ "recentTimes: " <> tshow chatTimeSequence
    , "recentReplyCount: " <> tshow recentReplyCount
    , "notRateLimitChance: " <> tshow notRateLimitChance
    , "rateLimited: " <> tshow rateLimited
    ]
  case rateLimited of
    True -> lift $ $(logInfo) $ T.intercalate "\n"
      [ "Rate limited, not replying."
      , "recentReplyCount: " <> tshow recentReplyCount
      , "notRateLimitChance: " <> tshow notRateLimitChance
      ]
    _     -> pure ()

  ated    <- lift beingAt
  let chanceReply = -- chance reply only happens when recent messages contains some text
        chance <= activeProbability && notAllNoText cqmsgs
      parsed = isJust $ MP.runParser (catParser bn chatSetting) msg

  guardMaybeT $ chanceReply || parsed || replied || mentioned && mentionReply || ated && atReply || oneOffActive
  guardMaybeT $ not rateLimited && not ignoredReaction

determineIfReply d@DetermineReplyData {cid = PrivateChat{}} _ (Just msg) _ | notLocked d = do
  if T.isPrefixOf ":" msg
  then MaybeT . pure $ Nothing
  else MaybeT . pure $ Just ()
determineIfReply _ _ _ _ = empty
