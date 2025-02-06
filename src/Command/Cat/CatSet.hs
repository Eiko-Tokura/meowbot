module Command.Cat.CatSet where

import System.General
import Module.LogDatabase
import MeowBot
import qualified MeowBot.Parser as MP
import MeowBot.Parser
import qualified Data.Text as T
import Data.Default
import Data.HList
import Data.Proxy
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad
import Utils.RunDB
import Utils.Persist
import Data.PersistModel
import External.ChatAPI hiding (SystemMessage)

modelsInUse :: CFList ChatAPI Proxy 
  [ Local    DeepSeekR1_14B
  , Local    DeepSeekR1_32B
  , DeepSeek DeepSeekChat
  , DeepSeek DeepSeekReasoner
  , OpenAI   GPT4oMini
  , OpenAI   GPT4o
  ]
modelsInUse = def

modelsInUseText :: [T.Text]
modelsInUseText = cfListMap modelsInUse $ \(Proxy :: Proxy a) -> tshow (chatModel @a)

data CatSetCommand
  = Set   DefaultOrPerChat BotSettingItem
  | UnSet DefaultOrPerChat BotSettingItem
  | View  DefaultOrPerChat BotSettingItem
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
  | ActiveChat           (Maybe Bool)
  | ActiveProbability    (Maybe Double)
  deriving (Show)

catSetParser :: Parser T.Text Char CatSetCommand
catSetParser = MP.headCommand "cat-" >> do
  action <- asum
    [ MP.string "set"   >> return Set
    , MP.string "unset" >> return UnSet
    , MP.string "view"  >> return View
    ]
  MP.spaces
  range <- asum 
    [ MP.string "default" *> return Default <* MP.spaces
    , MP.string "perchat" *> MP.spaces *> (PerChatWithChatId <$> chatIdP) <* MP.spaces
    , MP.string "perchat" *> return PerChat <* MP.spaces
    , return PerChat
    ]
  return $ action range (DisplayThinking Nothing)
  asum
    [ MP.string "displayThinking"      >> fmap (action range) (DisplayThinking      <$> MP.optMaybe (MP.spaces >> MP.bool))
    , MP.string "defaultModelSuper"    >> fmap (action range) (DefaultModelSuper    <$> MP.optMaybe (MP.spaces >> MP.parseByRead))
    , MP.string "defaultModel"         >> fmap (action range) (DefaultModel         <$> MP.optMaybe (MP.spaces >> MP.parseByRead))
    , MP.string "systemMessage"        >> fmap (action range) (SystemMessage        <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
    , MP.string "systemTemp"           >> fmap (action range) (SystemTemp           <$> MP.optMaybe (MP.spaces >> MP.nFloat))
    , MP.string "systemMaxToolDepth"   >> fmap (action range) (SystemMaxToolDepth   <$> MP.optMaybe (MP.spaces >> MP.intRange 1 100))
    , MP.string "systemAPIKeyOpenAI"   >> fmap (action range) (SystemAPIKeyOpenAI   <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
    , MP.string "systemAPIkeyDeepSeek" >> fmap (action range) (SystemAPIKeyDeepSeek <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
    , MP.string "activeChat"           >> fmap (action range) (ActiveChat           <$> MP.optMaybe (MP.spaces >> MP.bool))
    , MP.string "activeProbability"    >> fmap (action range) (ActiveProbability    <$> MP.optMaybe (MP.spaces >> MP.nFloat))
    ]
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
  , "    activeChat :: Bool"
  , "    activeProbability :: Double"
  , ""
  , "* ChatModel can be one of " <> T.intercalate ", " modelsInUseText
  , ""
  , "Example:"
  , ":cat-set default displayThinking true"
  , ":cat-set perchat defaultModel Local DeepSeekR1_32B"
  ]

----------------------------------- catSet -----------------------------------

catSet :: (LogDatabase `In` mods) => CatSetCommand -> MaybeT (MeowT r mods IO) [BotAction]
catSet (Set Default item) = do
  (_, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  _ <- MaybeT $ runDB $ selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] []
  botname <- query
  case item of
    DisplayThinking      mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingDisplayThinking =. mdt])
      return [ baSendToChatId cid $ "DisplayThinking set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingDefaultModel =. fmap PersistUseShow mdt])
      return [ baSendToChatId cid $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingDefaultModelS =. fmap PersistUseShow mdt])
      return [ baSendToChatId cid $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingSystemMessage =. mdt])
      return [ baSendToChatId cid $ "SystemMessage set to " <> tshow mdt ]
    SystemTemp           mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingSystemTemp =. mdt])
      return [ baSendToChatId cid $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingSystemMaxToolDepth =. mdt])
      return [ baSendToChatId cid $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingSystemAPIKeyOpenAI =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyOpenAI set to " <> tshow mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingSystemAPIKeyDeepSeek =. mdt])
      return [ baSendToChatId cid $ "SystemAPIKeyDeepSeek set to " <> tshow mdt ]
    ActiveChat           mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingActiveChat =. mdt])
      return [ baSendToChatId cid $ "ActiveChat set to " <> tshow mdt ]
    ActiveProbability    mdt -> do
      lift $ runDB (updateWhere [BotSettingBotName ==. maybeBotName botname] [BotSettingActiveProbability =. mdt])
      return [ baSendToChatId cid $ "ActiveProbability set to " <> tshow mdt ]

catSet (Set PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (Set (PerChatWithChatId cid) item)

catSet (Set (PerChatWithChatId cid) item) = do
  botname <- query
  (_, cid', uid, _, _) <- MaybeT $ getEssentialContent <$> query
  _ <- MaybeT $ (<|> if cid == cid' then Just () else Nothing) . void 
       <$> runDB (selectFirst [InUserGroupUserId ==. uid, InUserGroupUserGroup ==. Admin] [])
  lift $ runDB $ exists [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] >>= \case
    True -> return ()
    False -> insert_ $ def
      { botSettingPerChatChatId = cid
      , botSettingPerChatBotName = maybeBotName botname
      }
  case item of
    DisplayThinking      mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatDisplayThinking =. mdt]
      return [ baSendToChatId cid' $ "DisplayThinking set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatDefaultModel =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid' $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatDefaultModelS =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid' $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemMessage =. mdt]
      return [ baSendToChatId cid' $ "SystemMessage set to " <> tshow mdt ]
    SystemTemp           mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemTemp =. mdt]
      return [ baSendToChatId cid' $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemMaxToolDepth =. mdt]
      return [ baSendToChatId cid' $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemAPIKeyOpenAI =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyOpenAI set to " <> tshow mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemAPIKeyDeepSeek =. mdt]
      return [ baSendToChatId cid' $ "SystemAPIKeyDeepSeek set to " <> tshow mdt ]
    ActiveChat           mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatActiveChat =. mdt]
      return [ baSendToChatId cid' $ "ActiveChat set to " <> tshow mdt ]
    ActiveProbability    mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatActiveProbability =. mdt]
      return [ baSendToChatId cid' $ "ActiveProbability set to " <> tshow mdt ]

catSet (UnSet range item) = 
  case item of
    DisplayThinking      _ -> catSet (Set range $ DisplayThinking Nothing)
    DefaultModel         _ -> catSet (Set range $ DefaultModel Nothing)
    DefaultModelSuper    _ -> catSet (Set range $ DefaultModelSuper Nothing)
    SystemMessage        _ -> catSet (Set range $ SystemMessage Nothing)
    SystemTemp           _ -> catSet (Set range $ SystemTemp Nothing)
    SystemMaxToolDepth   _ -> catSet (Set range $ SystemMaxToolDepth Nothing)
    SystemAPIKeyOpenAI   _ -> catSet (Set range $ SystemAPIKeyOpenAI Nothing)
    SystemAPIKeyDeepSeek _ -> catSet (Set range $ SystemAPIKeyDeepSeek Nothing)
    ActiveChat           _ -> catSet (Set range $ ActiveChat Nothing)
catSet (View Default item) = do
  botname <- query
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runDB $ fmap (botSettingDisplayThinking . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "DisplayThinking: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runDB $ fmap (botSettingDefaultModel . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingDefaultModelS . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemMessage . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemMessage: " <> tshow mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemTemp . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemMaxToolDepth . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemAPIKeyOpenAI . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemAPIKeyOpenAI: " <> tshow mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runDB $ fmap (botSettingSystemAPIKeyDeepSeek . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemAPIKeyDeepSeek: " <> tshow mdt]
    ActiveChat           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingActiveChat . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "ActiveChat: " <> tshow mdt]
    ActiveProbability    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingActiveProbability . entityVal) <$> selectFirst [BotSettingBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "ActiveProbability: " <> tshow mdt]

catSet (View PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (View (PerChatWithChatId cid) item)

catSet (View (PerChatWithChatId cid) item) = do
  botname <- query
  (_, cid', _, _, _) <- MaybeT $ getEssentialContent <$> query
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDisplayThinking . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "DisplayThinking: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDefaultModel . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDefaultModelS . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemMessage . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "SystemMessage: " <> tshow mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemTemp . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemMaxToolDepth . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyOpenAI . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "SystemAPIKeyOpenAI: " <> tshow mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyDeepSeek . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "SystemAPIKeyDeepSeek: " <> tshow mdt]
    ActiveChat           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatActiveChat . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "ActiveChat: " <> tshow mdt]
    ActiveProbability    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatActiveProbability . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid' $ "ActiveProbability: " <> tshow mdt]


