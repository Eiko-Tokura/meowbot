{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Command.Cat.CatSet where

import System.General
import Module.LogDatabase
import MeowBot
import qualified MeowBot.Parser as MP
import MeowBot.Parser
import qualified Data.Text as T
import qualified Data.Map.Strict as SM
import Data.Default
import Data.Additional.Default
import Data.HList
import Data.Proxy
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState
import Control.Monad
import Utils.RunDB
import Utils.Persist
import Data.PersistModel
import External.ChatAPI hiding (SystemMessage)

-- we will have to mantain a ChatState for each chat in the Chat command (not Cat command)
data ChatState = ChatState
  { chatStatus :: !ChatStatus
  , meowStatus :: !MeowStatus -- ^ avoids crafting too many messages simultaneously
  } deriving (Show, Eq, Typeable)

data MeowStatus = MeowIdle | MeowBusy deriving (Show, Eq, Typeable)

type AllChatState = SM.Map ChatId ChatState -- since we are keeping it as state, use strict map
instance IsAdditionalData AllChatState      -- use getTypeWithDef

modelsInUse :: CFList ChatAPI Proxy
  [ Local       DeepSeekR1_14B
  , Local       DeepSeekR1_32B
  , Local       Qwen2_5_32B
  , Local       Command_R_Latest
  , DeepSeek    DeepSeekChat
  , DeepSeek    DeepSeekReasoner
  , OpenAI      GPT4oMini
  , OpenAI      GPT4o
  , OpenRouter  DeepSeekR1_Free
  , SiliconFlow SF_DeepSeekV3
  ]
modelsInUse = def

modelsInUseText :: [T.Text]
modelsInUseText = cfListMap modelsInUse $ \(Proxy :: Proxy a) -> tshow (chatModel @a)

data CatSetCommand
  = Set   DefaultOrPerChat BotSettingItem
  | UnSet DefaultOrPerChat BotSettingItem
  | View  DefaultOrPerChat BotSettingItem
  | Clear
  deriving (Show)

data DefaultOrPerChat = Default | PerChat | PerChatWithChatId ChatId deriving (Show)

data BotSettingItem
  = DisplayThinking      (Maybe Bool)
  | DefaultModel         (Maybe ChatModel)
  | DefaultModelSuper    (Maybe ChatModel)
  | SystemMessage        (Maybe Text)
  | SystemTemp           (Maybe Double)
  | SystemMaxToolDepth   (Maybe Int)
  | SystemAPIKeyOpenAI   (Maybe Text)
  | SystemAPIKeyDeepSeek (Maybe Text)
  | SystemAPIKeyOpenRouter (Maybe Text)
  | SystemAPIKeySiliconFlow (Maybe Text)
  | ActiveChat           (Maybe Bool)
  | AtReply              (Maybe Bool)
  | ActiveProbability    (Maybe Double)
  deriving (Show)

catSetParser :: Parser T.Text Char CatSetCommand
catSetParser =
  (MP.headCommand "cat-" >> do
    action <- asum
      [ MP.string "set"   >> return Set
      , MP.string "unset" >> return UnSet
      , MP.string "view"  >> return View
      ]
    MP.spaces
    range <- asum
      [ MP.string "default" *> return Default  <* MP.spaces
      , MP.string "perchat" *> MP.spaces
            *> (PerChatWithChatId <$> chatIdP) <* MP.spaces
      , MP.string "perchat" *> return PerChat  <* MP.spaces
      , return PerChat
      ]
    return $ action range (DisplayThinking Nothing)
    asum
      [ MP.string "displayThinking"         >> fmap (action range) (DisplayThinking         <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "defaultModelSuper"       >> fmap (action range) (DefaultModelSuper       <$> MP.optMaybe (MP.spaces >> MP.parseByRead))
      , MP.string "defaultModel"            >> fmap (action range) (DefaultModel            <$> MP.optMaybe (MP.spaces >> MP.parseByRead))
      , MP.string "systemMessage"           >> fmap (action range) (SystemMessage           <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemTemp"              >> fmap (action range) (SystemTemp              <$> MP.optMaybe (MP.spaces >> MP.nFloat))
      , MP.string "systemMaxToolDepth"      >> fmap (action range) (SystemMaxToolDepth      <$> MP.optMaybe (MP.spaces >> MP.intRange 1 100))
      , MP.string "systemAPIKeyOpenAI"      >> fmap (action range) (SystemAPIKeyOpenAI      <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemAPIkeyDeepSeek"    >> fmap (action range) (SystemAPIKeyDeepSeek    <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemAPIKeyOpenRouter"  >> fmap (action range) (SystemAPIKeyOpenRouter  <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemAPIKeySiliconFlow" >> fmap (action range) (SystemAPIKeySiliconFlow <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "activeChat"              >> fmap (action range) (ActiveChat              <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "atReply"                 >> fmap (action range) (AtReply                 <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "activeProbability"       >> fmap (action range) (ActiveProbability       <$> MP.optMaybe (MP.spaces >> MP.nFloat))
      ]
    ) <|> (MP.headCommand "cat-clear" >> return Clear)
  where chatIdP = asum
          [ MP.string "user"  >> MP.spaces >> PrivateChat . UserId  <$> MP.int
          , MP.string "group" >> MP.spaces >> GroupChat   . GroupId <$> MP.int
          ]

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
  , "  only Admin can change 'default' and 'perchat <chatid>' settings"
  , ""
  , "* item is one of "
  , "    displayThinking :: Bool"
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
  , "    activeProbability :: Double"
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

----------------------------------- catSet -----------------------------------

catSet :: (LogDatabase `In` mods) => CatSetCommand -> MaybeT (MeowT r mods IO) [BotAction]
catSet (Set Default item) = do
  (_, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  _ <- MaybeT $ runDB $ selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] []
  botid <- query
  let selector = [BotSettingBotId ==. botid]
  case item of
    DisplayThinking      mdt -> do
      lift $ runDB (updateWhere selector [BotSettingDisplayThinking =. mdt])
      return [ baSendToChatId cid $ "DisplayThinking set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      lift $ runDB (updateWhere selector [BotSettingDefaultModel =. fmap PersistUseShow mdt])
      return [ baSendToChatId cid $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runDB (updateWhere selector [BotSettingDefaultModelS =. fmap PersistUseShow mdt])
      return [ baSendToChatId cid $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemMessage =. mdt])
      return [ baSendToChatId cid $ "SystemMessage set to " <> toText mdt ]
    SystemTemp           mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemTemp =. mdt])
      return [ baSendToChatId cid $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemMaxToolDepth =. mdt])
      return [ baSendToChatId cid $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemAPIKeyOpenAI =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyOpenAI set to " <> tshow mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemAPIKeyDeepSeek =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyDeepSeek set to " <> tshow mdt ]
    SystemAPIKeyOpenRouter mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemAPIKeyOpenRouter =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyOpenRouter set to " <> tshow mdt ]
    SystemAPIKeySiliconFlow mdt -> do
      lift $ runDB (updateWhere selector [BotSettingSystemAPIKeySiliconFlow =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeySiliconFlow set to " <> tshow mdt ]
    ActiveChat           mdt -> do
      lift $ runDB (updateWhere selector [BotSettingActiveChat =. mdt])
      return [ baSendToChatId cid $ "ActiveChat set to " <> tshow mdt ]
    AtReply              mdt -> do
      lift $ runDB (updateWhere selector [BotSettingAtReply =. mdt])
      return [ baSendToChatId cid $ "AtReply set to " <> tshow mdt ]
    ActiveProbability    mdt -> do
      lift $ runDB (updateWhere selector [BotSettingActiveProbability =. mdt])
      return [ baSendToChatId cid $ "ActiveProbability set to " <> tshow mdt ]

catSet (Set PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (Set (PerChatWithChatId cid) item)

catSet (Set (PerChatWithChatId cid) item) = do
  botid <- query
  (_, cid', uid, _, _) <- MaybeT $ getEssentialContent <$> query
  _ <- MaybeT $ (<|> if cid == cid' then Just () else Nothing) . void
       <$> runDB (selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] [])
  lift $ runDB $ exists [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botid] >>= \case
    True -> return ()
    False -> insert_ $ def
      { botSettingPerChatChatId = cid
      , botSettingPerChatBotId = botid
      }
  let selector = [BotSettingPerChatBotId ==. botid, BotSettingPerChatChatId ==. cid]
  case item of
    DisplayThinking      mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatDisplayThinking =. mdt]
      return [ baSendToChatId cid' $ "DisplayThinking set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatDefaultModel =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid' $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatDefaultModelS =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid' $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemMessage =. mdt]
      return [ baSendToChatId cid' $ "SystemMessage set to " <> toText mdt ]
    SystemTemp           mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemTemp =. mdt]
      return [ baSendToChatId cid' $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemMaxToolDepth =. mdt]
      return [ baSendToChatId cid' $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemAPIKeyOpenAI =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyOpenAI set to " <> redacted mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemAPIKeyDeepSeek =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyDeepSeek set to " <> redacted mdt ]
    SystemAPIKeyOpenRouter mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemAPIKeyOpenRouter =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyOpenRouter set to " <> redacted mdt ]
    SystemAPIKeySiliconFlow mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatSystemAPIKeySiliconFlow =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeySiliconFlow set to " <> redacted mdt ]
    ActiveChat           mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatActiveChat =. mdt]
      return [ baSendToChatId cid' $ "ActiveChat set to " <> tshow mdt ]
    AtReply              mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatAtReply =. mdt]
      return [ baSendToChatId cid' $ "AtReply set to " <> tshow mdt ]
    ActiveProbability    mdt -> do
      lift $ runDB $ updateWhere selector [BotSettingPerChatActiveProbability =. mdt]
      return [ baSendToChatId cid' $ "ActiveProbability set to " <> tshow mdt ]

catSet (UnSet range item) =
  case item of
    DisplayThinking         _ -> catSet (Set range $ DisplayThinking Nothing)
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
    ActiveProbability       _ -> catSet (Set range $ ActiveProbability Nothing)
catSet (View Default item) = do
  botid <- query
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  let selector = [BotSettingBotId ==. botid]
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runDB $ fmap (botSettingDisplayThinking . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DisplayThinking: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runDB $ fmap (botSettingDefaultModel . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingDefaultModelS . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemMessage . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemMessage: " <> toText mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemTemp . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemMaxToolDepth . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemAPIKeyOpenAI . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeyOpenAI: " <> redacted mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemAPIKeyDeepSeek . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeyDeepSeek: " <> redacted mdt]
    SystemAPIKeyOpenRouter _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemAPIKeyOpenRouter . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeyOpenRouter: " <> redacted mdt]
    SystemAPIKeySiliconFlow _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemAPIKeySiliconFlow . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "SystemAPIKeySiliconFlow: " <> redacted mdt]
    ActiveChat           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingActiveChat . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "ActiveChat: " <> tshow mdt]
    AtReply              _ -> do
      mdt <- lift $ runDB $ fmap (botSettingAtReply . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "AtReply: " <> tshow mdt]
    ActiveProbability    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingActiveProbability . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid $ "ActiveProbability: " <> tshow mdt]

catSet (View PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (View (PerChatWithChatId cid) item)

catSet (View (PerChatWithChatId cid) item) = do
  botid <- query
  (_, cid', _, _, _) <- MaybeT $ getEssentialContent <$> query
  let selector = [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botid]
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDisplayThinking . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "DisplayThinking: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDefaultModel . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDefaultModelS . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemMessage . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemMessage: " <> toText mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemTemp . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemMaxToolDepth . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyOpenAI . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemAPIKeyOpenAI: " <> redacted mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyDeepSeek . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemAPIKeyDeepSeek: " <> redacted mdt]
    SystemAPIKeyOpenRouter _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyOpenRouter . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemAPIKeyOpenRouter: " <> redacted mdt]
    SystemAPIKeySiliconFlow _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeySiliconFlow . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "SystemAPIKeySiliconFlow: " <> redacted mdt]
    ActiveChat           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatActiveChat . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "ActiveChat: " <> tshow mdt]
    AtReply              _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatAtReply . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "AtReply: " <> tshow mdt]
    ActiveProbability    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatActiveProbability . entityVal) <$> selectFirst selector []
      return [baSendToChatId cid' $ "ActiveProbability: " <> tshow mdt]

catSet Clear = do
  let newChatState = SM.empty ::SM.Map ChatId ChatState 

  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query

  let updateChatState :: ChatId -> (ChatState -> ChatState) -> AllChatState -> AllChatState
      updateChatState cid f s =
        let state = SM.lookup cid s in
        case state of
          Just cs -> SM.insert cid (f cs) s
          Nothing -> SM.insert cid (ChatState (ChatStatus 0 0 []) MeowIdle) s

      clear :: ChatState -> ChatState
      clear _ = ChatState (ChatStatus 0 0 []) MeowIdle

  allChatState <- lift $ updateChatState cid clear <$> getTypeWithDef newChatState
  -- ^ get the chat state and clear it

  lift . putType $ allChatState
  -- ^ update in the state

  return [baSendToChatId cid "Chat state cleared"]

class MaybeRedacted a where
  redacted :: a -> Text

instance MaybeRedacted Text where
  redacted = const "<redacted>"

instance MaybeRedacted a => MaybeRedacted (Maybe a) where
  redacted Nothing = "Nothing"
  redacted (Just x) = "Just (" <> redacted x <> ")"
