{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Command.Cat.CatSet
  ( module Command.Cat.CatSet
  , catSetParser
  ) where

import Module.LogDatabase
import MeowBot
import qualified MeowBot.Parser as MP
import MeowBot.Parser
import qualified Data.Text as T
import qualified Data.Map.Strict as SM
import Data.Default
import Data.Additional.Default
import Data.HList hiding (In)
import Data.Proxy
import Data.Maybe
import qualified Data.BSeq as BSeq
import Control.Monad.Trans.Maybe
import Module.MeowTypes
import Control.Monad.Effect
import Control.Monad
import Utils.RunDB
import Utils.Persist
import Data.PersistModel
import External.ChatAPI hiding (SystemMessage)

import Utils.Unit
import Command.Cat.CatSet.Parser
import Data.Time.Clock

-- we will have to mantain a ChatState for each chat in the Chat command (not Cat command)
data ChatState = ChatState
  { chatStatus :: !ChatStatus
  , meowStatus :: !MeowStatus -- ^ avoids crafting too many messages simultaneously
  , activeTriggerOneOff :: !Bool
    -- ^ if set, this will override the active probability for this chat
    -- used to actively trigger a chat
    -- will be reset to Nothing after one chat run
  , replyTimes :: !(BSeq.BSeq 10 UTCTime)
  } deriving (Show, Eq)

data MeowStatus = MeowIdle | MeowBusy deriving (Show, Eq)

instance Default MeowStatus where def = MeowIdle
instance Default ChatState where
  def = ChatState
    { chatStatus = ChatStatus 0 0 [] mempty
    , meowStatus = def
    , activeTriggerOneOff = False
    , replyTimes = BSeq.BSeq mempty
    }

type AllChatState = SM.Map ChatId ChatState -- since we are keeping it as state, use strict map
instance IsAdditionalData AllChatState      -- use getTypeWithDef

modelsInUse :: CFList ChatAPI Proxy
  [ Local       Qwen3_30B
  , Local       Qwen2_5_32B
  , Local       Command_R_Latest
  , Local       DummyTestModel
  , DeepSeek    DeepSeekChat
  , DeepSeek    DeepSeekReasoner
  , OpenAI      GPT4oMini
  , OpenAI      GPT4o
  , OpenAI      O1Mini
  , OpenRouter  OR_DeepSeekR1_Free
  , OpenRouter  OR_DeepSeekV3_Free
  , SiliconFlow SF_DeepSeekV3
  , SiliconFlow SF_DeepSeekR1
  ]
modelsInUse = def

adminRestrictedModels :: [ChatModel]
adminRestrictedModels =
  [ DeepSeek DeepSeekReasoner
  , OpenAI O3Mini
  , OpenAI O1Mini
  , OpenAI GPT4o
  , SiliconFlow SF_DeepSeekR1
  ]

modelsInUseText :: [T.Text]
modelsInUseText = cfListMap modelsInUse $ \(Proxy :: Proxy a) -> tshow (chatModel @a)

testCatSetParser = MP.runParser catSetParser ":cat-set default displayThinking true"

helpCatSet = T.intercalate "\n" $
  [ "modify/view chat settings:"
  , ":cat-<action> [range] <item> [value]"
  , "  where"
  , ""
  , "* action is one of set | unset | view"
  , ""
  , "* range is one of default | perchat | perchat <chatid>"
  , "  if omitted, 'perchat' will be used"
  , "  only Admin can modify 'default' and 'perchat <chatid>' settings"
  , ""
  , "* item is one of "
  , "    displayThinking :: Bool"
  , "    displayToolMessage :: Bool"
  , "    defaultModel :: ChatModel"
  , "    defaultModelSuper :: ChatModel"
  , "    systemMessage :: Text"
  , "    systemTemp :: Double"
  , "    systemMaxToolDepth :: Int"
  , "    systemAPIKeyOpenAI :: Text"
  , "    systemAPIKeyDeepSeek :: Text"
  , "    systemAPIKeyOpenRouter :: Text"
  , "    systemAPIKeySiliconFlow :: Text"
  , "    activeChat :: Bool"
  , "    atReply :: Bool"
  , "    mentionReply :: Bool"
  , "    activeProbability :: Double"
  , "    maxMessageInState :: Int"
  , "   special item:"
  , "    note"
  , "    crontab"
  , ""
  , "* ChatModel can be one of \n" <> T.intercalate ", " modelsInUseText
  , ""
  , "Example:"
  , ":cat-set default displayThinking true"
  , ":cat-set perchat defaultModel Local DeepSeekR1_32B"
  , ""
  , "Clear chat context (only effective in chat mode):"
  , ":cat-clear"
  ]

insertBotSettingPerChatIfNotExists
  :: ( LogDatabase  `In` mods
     , MeowCoreDb `In` mods
     )
  => BotId -> ChatId -> MaybeT (MeowT mods IO) ()
insertBotSettingPerChatIfNotExists botid cid = lift $ runMeowCoreDB $ exists [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botid] >>= \case
    True -> return ()
    False -> insert_ $ def
      { botSettingPerChatChatId = cid
      , botSettingPerChatBotId = botid
      }

----------------------------------- catSet -----------------------------------

catSet ::
  ($(unitsNamed unitTestsCatSetParser)
  , LogDatabase `In` mods
  , MeowCoreDb `In` mods
  , MeowAllData mods
  ) => CatSetCommand -> MaybeT (MeowT mods IO) [BotAction]
catSet (Set Default item) = do
  (_, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  _ <- MaybeT $ runMeowCoreDB $ selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] []
  botid <- query
  let selector = [BotSettingBotId ==. botid]
  case item of
    DisplayThinking      mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingDisplayThinking =. mdt])
      return [ baSendToChatId cid $ "DisplayThinking set to " <> tshow mdt ]
    DisplayToolMessage   mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingDisplayToolMessage =. mdt])
      return [ baSendToChatId cid $ "DisplayToolMessage set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingDefaultModel =. fmap PersistUseShow mdt])
      return [ baSendToChatId cid $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingDefaultModelS =. fmap PersistUseShow mdt])
      return [ baSendToChatId cid $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemMessage =. mdt])
      return [ baSendToChatId cid $ "SystemMessage set to " <> toText mdt ]
    SystemTemp           mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemTemp =. mdt])
      return [ baSendToChatId cid $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemMaxToolDepth =. mdt])
      return [ baSendToChatId cid $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemAPIKeyOpenAI =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyOpenAI set to " <> tshow mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemAPIKeyDeepSeek =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyDeepSeek set to " <> tshow mdt ]
    SystemAPIKeyOpenRouter mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemAPIKeyOpenRouter =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyOpenRouter set to " <> tshow mdt ]
    SystemAPIKeySiliconFlow mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingSystemAPIKeySiliconFlow =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeySiliconFlow set to " <> tshow mdt ]
    ActiveChat           mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingActiveChat =. mdt])
      return [ baSendToChatId cid $ "ActiveChat set to " <> tshow mdt ]
    AtReply              mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingAtReply =. mdt])
      return [ baSendToChatId cid $ "AtReply set to " <> tshow mdt ]
    MentionReply         mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingMentionReply =. mdt])
      return [ baSendToChatId cid $ "MentionReply set to " <> tshow mdt ]
    ActiveProbability    mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingActiveProbability =. mdt])
      return [ baSendToChatId cid $ "ActiveProbability set to " <> tshow mdt ]
    MaxMessageInState mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingMaxMessageInState =. mdt])
      return [ baSendToChatId cid $ "MaxMessageInState set to " <> tshow mdt ]
    Note                 _   -> do
      return [ baSendToChatId cid $ "Not available" ]
    CronTab              _   -> do
      return [ baSendToChatId cid $ "Not available" ]
    EnableNotes         mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingEnableNotes =. mdt])
      return [ baSendToChatId cid $ "EnableNotes set to " <> tshow mdt ]
    EnableCronTab       mdt -> do
      lift $ runMeowCoreDB (updateWhere selector [BotSettingEnableCronTab =. mdt])
      return [ baSendToChatId cid $ "EnableCronTab set to " <> tshow mdt ]


catSet (Set PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (Set (PerChatWithChatId cid) item)

catSet (Set (PerChatWithChatId cid) item) = do
  botid <- query
  (_, cid', uid, _, _) <- MaybeT $ getEssentialContent <$> query
  -- | If the cid to be set matches the cid of the current chat, we allow it
  -- otherwise you must be an Admin to set it
  _ <- MaybeT $ (<|> if cid == cid' then Just () else Nothing) . void
       <$> runMeowCoreDB (selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] [])
  lift $ runMeowCoreDB $ exists [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botid] >>= \case
    True -> return ()
    False -> insert_ $ def
      { botSettingPerChatChatId = cid
      , botSettingPerChatBotId = botid
      }
  let selector = [BotSettingPerChatBotId ==. botid, BotSettingPerChatChatId ==. cid]
  case item of
    DisplayThinking      mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatDisplayThinking =. mdt]
      return [ baSendToChatId cid' $ "DisplayThinking set to " <> tshow mdt ]
    DisplayToolMessage   mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatDisplayToolMessage =. mdt]
      return [ baSendToChatId cid' $ "DisplayToolMessage set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      _ <- MaybeT $ if mdt `elem` fmap Just adminRestrictedModels -- requre Admin to set restricted models
          then fmap void $ runMeowCoreDB $ selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] []
          else return $ Just ()
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatDefaultModel =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid' $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatDefaultModelS =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid' $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemMessage =. mdt]
      return [ baSendToChatId cid' $ "SystemMessage set to " <> toText mdt ]
    SystemTemp           mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemTemp =. mdt]
      return [ baSendToChatId cid' $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemMaxToolDepth =. mdt]
      return [ baSendToChatId cid' $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemAPIKeyOpenAI =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyOpenAI set to " <> redacted mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemAPIKeyDeepSeek =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyDeepSeek set to " <> redacted mdt ]
    SystemAPIKeyOpenRouter mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemAPIKeyOpenRouter =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyOpenRouter set to " <> redacted mdt ]
    SystemAPIKeySiliconFlow mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatSystemAPIKeySiliconFlow =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeySiliconFlow set to " <> redacted mdt ]
    ActiveChat           mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatActiveChat =. mdt]
      return [ baSendToChatId cid' $ "ActiveChat set to " <> tshow mdt ]
    AtReply              mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatAtReply =. mdt]
      return [ baSendToChatId cid' $ "AtReply set to " <> tshow mdt ]
    MentionReply         mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatMentionReply =. mdt]
      return [ baSendToChatId cid' $ "MentionReply set to " <> tshow mdt ]
    ActiveProbability    mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatActiveProbability =. mdt]
      return [ baSendToChatId cid' $ "ActiveProbability set to " <> tshow mdt ]
    MaxMessageInState mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatMaxMessageInState =. mdt]
      return [ baSendToChatId cid' $ "MaxMessageInState set to " <> tshow mdt ]
    Note                 _   -> do
      return [ baSendToChatId cid $ "Not available" ]
    CronTab              _   -> do
      return [ baSendToChatId cid $ "Not available" ]
    EnableNotes         mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatEnableNotes =. mdt]
      return [ baSendToChatId cid' $ "EnableNotes set to " <> tshow mdt ]
    EnableCronTab       mdt -> do
      lift $ runMeowCoreDB $ updateWhere selector [BotSettingPerChatEnableCronTab =. mdt]
      return [ baSendToChatId cid' $ "EnableCronTab set to " <> tshow mdt ]

catSet (UnSet range item) = do
  botid <- query
  BotName botname <- query
  (_, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  case item of
    DisplayThinking         _ -> catSet (Set range $ DisplayThinking Nothing)
    DisplayToolMessage      _ -> catSet (Set range $ DisplayToolMessage Nothing)
    DefaultModel            _ -> catSet (Set range $ DefaultModel Nothing)
    DefaultModelSuper       _ -> catSet (Set range $ DefaultModelSuper Nothing)
    SystemMessage           _ -> catSet (Set range $ SystemMessage Nothing)
    SystemTemp              _ -> catSet (Set range $ SystemTemp Nothing)
    SystemMaxToolDepth      _ -> catSet (Set range $ SystemMaxToolDepth Nothing)
    SystemAPIKeyOpenAI      _ -> catSet (Set range $ SystemAPIKeyOpenAI Nothing)
    SystemAPIKeyDeepSeek    _ -> catSet (Set range $ SystemAPIKeyDeepSeek Nothing)
    SystemAPIKeyOpenRouter  _ -> catSet (Set range $ SystemAPIKeyOpenRouter Nothing)
    SystemAPIKeySiliconFlow _ -> catSet (Set range $ SystemAPIKeySiliconFlow Nothing)
    ActiveChat              _ -> catSet (Set range $ ActiveChat Nothing)
    AtReply                 _ -> catSet (Set range $ AtReply Nothing)
    MentionReply            _ -> catSet (Set range $ MentionReply Nothing)
    ActiveProbability       _ -> catSet (Set range $ ActiveProbability Nothing)
    MaxMessageInState       _ -> catSet (Set range $ MaxMessageInState Nothing)
    EnableNotes             _ -> catSet (Set range $ EnableNotes Nothing)
    EnableCronTab           _ -> catSet (Set range $ EnableCronTab Nothing)
    _ -> do
      cidOperate <- case range of
          Default -> pure Nothing
          PerChat -> pure $ Just cid
          PerChatWithChatId c -> pure $ Just c

      -- | If the cid to be set matches the cid of the current chat, we allow it
      -- otherwise you must be an Admin to set it
      _ <- MaybeT $ (<|> if Just cid == cidOperate then Just () else Nothing) . void
           <$> runMeowCoreDB (selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] [])

      case item of
        Note           (Just nid) | Just cid' <- cidOperate -> do
          lift $ runMeowCoreDB $ deleteWhere [AssistantNoteChatId ==. cid', AssistantNoteNoteId ==. nid, AssistantNoteBotName ==. botname]
          return [baSendToChatId cid $ "Note with id " <> tshow nid <> " in chat " <> tshow cid' <> " deleted"]
        Note           (Just nid) | Nothing <- cidOperate -> do
          lift $ runMeowCoreDB $ deleteWhere [AssistantNoteNoteId ==. nid, AssistantNoteBotName ==. botname]
          return [baSendToChatId cid $ "Note with id " <> tshow nid <> " deleted"]
        Note           Nothing    | Just cid' <- cidOperate -> do
          lift $ runMeowCoreDB $ deleteWhere [AssistantNoteChatId ==. cid', AssistantNoteBotName ==. botname]
          return [baSendToChatId cid $ "All notes for bot " <> toText botname <> " in chat " <> tshow cid' <> " deleted"]
        Note           Nothing    | Nothing <- cidOperate -> do
          lift $ runMeowCoreDB $ deleteWhere [AssistantNoteBotName ==. botname]
          return [baSendToChatId cid $ "All notes for bot " <> toText botname <> " deleted"]

        CronTab       (Just ctid) -> do
          lift $ runMeowCoreDB $ deleteWhere [BotCronJobBotId ==. botid, BotCronJobId ==. intToKey ctid]
          return [baSendToChatId cid $ "CronTab with id " <> tshow ctid <> " deleted"]
        CronTab       Nothing     | Just cid' <- cidOperate -> do
          lift $ runMeowCoreDB $ deleteWhere [BotCronJobBotId ==. botid, BotCronJobChatId ==. Just cid']
          return [baSendToChatId cid $ "All CronTabs for bot " <> toText botname <> " in chat " <> tshow cid' <> " deleted"]
        CronTab       Nothing     | Nothing <- cidOperate -> do
          lift $ runMeowCoreDB $ deleteWhere [BotCronJobBotId ==. botid]
          return [baSendToChatId cid $ "All CronTabs for bot " <> toText botname <> " deleted"]

catSet (View Default item) = do
  botid <- query
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  let selector = [BotSettingBotId ==. botid]
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingDisplayThinking . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DisplayThinking: " <> tshow mdt]
    DisplayToolMessage   _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingDisplayToolMessage . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DisplayToolMessage: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingDefaultModel . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingDefaultModelS . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemMessage . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemMessage: " <> toText mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemTemp . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemMaxToolDepth . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemAPIKeyOpenAI . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeyOpenAI: " <> redacted mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemAPIKeyDeepSeek . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeyDeepSeek: " <> redacted mdt]
    SystemAPIKeyOpenRouter _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemAPIKeyOpenRouter . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeyOpenRouter: " <> redacted mdt]
    SystemAPIKeySiliconFlow _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingSystemAPIKeySiliconFlow . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeySiliconFlow: " <> redacted mdt]
    ActiveChat           _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingActiveChat . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "ActiveChat: " <> tshow mdt]
    AtReply              _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingAtReply . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "AtReply: " <> tshow mdt]
    MentionReply         _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingMentionReply . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "MentionReply: " <> tshow mdt]
    ActiveProbability    _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingActiveProbability . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "ActiveProbability: " <> tshow mdt]
    MaxMessageInState    _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingMaxMessageInState . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "MaxMessageInState: " <> tshow mdt]
    CronTab        Nothing -> do
      cronJobs <- lift $ runMeowCoreDB $ selectList [BotCronJobBotId ==. botid] []
      let cronJobTexts = fmap cronTabDisplayText cronJobs
      return [baSendToChatId cid $ T.intercalate "\n\n" cronJobTexts]
    EnableNotes         _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingEnableNotes . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "EnableNotes: " <> tshow mdt]
    EnableCronTab       _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingEnableCronTab . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "EnableCronTab: " <> tshow mdt]
    _ -> do
      return [baSendToChatId cid $ "Not available"]

catSet (View PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (View (PerChatWithChatId cid) item)

catSet (View (PerChatWithChatId cid) item) = do
  botid <- query
  BotName botname <- query
  (_, cidOrigin, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  -- | If the cid to be set matches the cid of the current chat, we allow it
  -- otherwise you must be an Admin to set it
  _ <- MaybeT $ (<|> if cid == cidOrigin then Just () else Nothing) . void
          <$> runMeowCoreDB (selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] [])
  let selector = [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botid]
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatDisplayThinking . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "DisplayThinking: " <> tshow mdt]
    DisplayToolMessage   _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatDisplayToolMessage . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "DisplayToolMessage: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatDefaultModel . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatDefaultModelS . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemMessage . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemMessage: " <> toText mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemTemp . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemMaxToolDepth . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemAPIKeyOpenAI . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemAPIKeyOpenAI: " <> redacted mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemAPIKeyDeepSeek . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemAPIKeyDeepSeek: " <> redacted mdt]
    SystemAPIKeyOpenRouter _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemAPIKeyOpenRouter . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemAPIKeyOpenRouter: " <> redacted mdt]
    SystemAPIKeySiliconFlow _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatSystemAPIKeySiliconFlow . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "SystemAPIKeySiliconFlow: " <> redacted mdt]
    ActiveChat           _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatActiveChat . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "ActiveChat: " <> tshow mdt]
    AtReply              _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatAtReply . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "AtReply: " <> tshow mdt]
    MentionReply         _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatMentionReply . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "MentionReply: " <> tshow mdt]
    ActiveProbability    _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatActiveProbability . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "ActiveProbability: " <> tshow mdt]
    MaxMessageInState    _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatMaxMessageInState . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "MaxMessageInState: " <> tshow mdt]
    Note           Nothing -> do
      notes <- lift $ runMeowCoreDB $ selectList [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid] []
      let noteTexts = fmap (noteDisplayText . entityVal) notes
      return [baSendToChatId cidOrigin $ "Notes: " <> T.intercalate "\n\n" noteTexts]
    Note          (Just nid) -> do
      note <- lift $ runMeowCoreDB $ selectFirst [AssistantNoteNoteId ==. nid, AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid] []
      case note of
        Just n  -> return [baSendToChatId cidOrigin $ noteDisplayText (entityVal n)]
        Nothing -> return [baSendToChatId cidOrigin $ "No note with id " <> tshow nid <> " found."]
    CronTab         Nothing -> do
      cronTabs <- lift $ runMeowCoreDB $ selectList [BotCronJobBotId ==. botid] []
      let cronTabTexts = fmap (cronTabDisplayTextWithCid cid) cronTabs
      return [baSendToChatId cidOrigin $ T.intercalate "\n\n" $ catMaybes cronTabTexts]
    EnableNotes         _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatEnableNotes . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "EnableNotes: " <> tshow mdt]
    EnableCronTab       _ -> do
      mdt <- lift $ runMeowCoreDB $ fmap (botSettingPerChatEnableCronTab . entityVal) <$> selectFirst selector []
      return [baSendToChatId cidOrigin $ "EnableCronTab: " <> tshow mdt]
    _                       -> do
      return [baSendToChatId cidOrigin $ "Unavailable item & mode combination."]


catSet (Clear range) = do
  let newChatState = SM.empty ::SM.Map ChatId ChatState
  (_, original_cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  ecid <- case range of
    PerChat -> do
      (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
      return $ Right cid
    PerChatWithChatId c -> return $ Right c
    _ -> pure $ Left "Clear command can only be used in perchat mode"

  let updateChatState :: ChatId -> (ChatState -> ChatState) -> AllChatState -> AllChatState
      updateChatState cid f s =
        let state = SM.lookup cid s in
        case state of
          Just cs -> SM.insert cid (f cs) s
          Nothing -> SM.insert cid def s

      clear :: ChatState -> ChatState
      clear _ = def

  case ecid of
    Left err -> do
      return [baSendToChatId original_cid $ "Error: " <> T.pack err]
    Right cid -> do
      _ <- MaybeT $ (<|> if cid == original_cid then Just () else Nothing) . void
          <$> runMeowCoreDB (selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] [])
      allChatState <- lift $ updateChatState cid clear <$> getTypeWithDef newChatState
      -- ^ get the chat state and clear it
    
      lift . putType $ allChatState
      -- ^ update in the state
    
      return [baSendToChatId original_cid $ "Chat state for chat " <> tshow cid <> " cleared."]

class MaybeRedacted a where
  redacted :: a -> Text

instance MaybeRedacted Text where
  redacted = const "<redacted>"

instance MaybeRedacted a => MaybeRedacted (Maybe a) where
  redacted Nothing = "Nothing"
  redacted (Just x) = "Just (" <> redacted x <> ")"
