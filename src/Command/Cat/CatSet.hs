module Command.Cat.CatSet where

import System.General
import Module.LogDatabase
import MeowBot
import qualified MeowBot.Parser as MP
import MeowBot.Parser
import qualified Data.Text as T
import Data.HList
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Utils.RunDB
import Utils.Persist
import Data.PersistModel
import External.ChatAPI (ChatModel(..))

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
    [ MP.string "displayThinking"      >> MP.spaces >> fmap (action range) (DisplayThinking      <$> MP.optMaybe MP.bool)
    , MP.string "defaultModel"         >> MP.spaces >> fmap (action range) (DefaultModel         <$> (Just <$> MP.parseByRead))
    , MP.string "defaultModelSuper"    >> MP.spaces >> fmap (action range) (DefaultModelSuper    <$> (Just <$> MP.parseByRead))
    , MP.string "systemMessage"        >> MP.spaces >> fmap (action range) (SystemMessage        <$> MP.optMaybe (MP.some' MP.item))
    , MP.string "systemTemp"           >> MP.spaces >> fmap (action range) (SystemTemp           <$> MP.optMaybe MP.nFloat)
    , MP.string "systemMaxToolDepth"   >> MP.spaces >> fmap (action range) (SystemMaxToolDepth   <$> MP.optMaybe (MP.intRange 1 100))
    , MP.string "systemAPIKeyOpenAI"   >> MP.spaces >> fmap (action range) (SystemAPIKeyOpenAI   <$> MP.optMaybe (MP.some' MP.item))
    , MP.string "systemAPIkeyDeepSeek" >> MP.spaces >> fmap (action range) (SystemAPIKeyDeepSeek <$> MP.optMaybe (MP.some' MP.item))
    ]
  where chatIdP = asum
          [ MP.string "user"  >> MP.spaces >> PrivateChat . UserId  <$> MP.int
          , MP.string "group" >> MP.spaces >> GroupChat   . GroupId <$> MP.int
          ]

testCatSetParser = MP.runParser catSetParser ":cat-set default displayThinking true"

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

catSet (Set PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (Set (PerChatWithChatId cid) item)

catSet (Set (PerChatWithChatId cid) item) = do
  botname <- query
  case item of
    DisplayThinking      mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatDisplayThinking =. mdt]
      return [ baSendToChatId cid $ "DisplayThinking set to " <> tshow mdt ]
    DefaultModel         mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatDefaultModel =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid $ "DefaultModel set to " <> tshow mdt ]
    DefaultModelSuper    mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatDefaultModelS =. fmap PersistUseShow mdt]
      return [ baSendToChatId cid $ "DefaultModelSuper set to " <> tshow mdt ]
    SystemMessage        mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemMessage =. mdt]
      return [ baSendToChatId cid $ "SystemMessage set to " <> tshow mdt ]
    SystemTemp           mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemTemp =. mdt]
      return [ baSendToChatId cid $ "SystemTemp set to " <> tshow mdt ]
    SystemMaxToolDepth   mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemMaxToolDepth =. mdt]
      return [ baSendToChatId cid $ "SystemMaxToolDepth set to " <> tshow mdt ]
    SystemAPIKeyOpenAI   mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemAPIKeyOpenAI =. mdt]
      return [ baSendToChatId cid $ "SystemAPIKeyOpenAI set to " <> tshow mdt ]
    SystemAPIKeyDeepSeek mdt -> do
      lift $ runDB $ updateWhere [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] [BotSettingPerChatSystemAPIKeyDeepSeek =. mdt]
      return [ baSendToChatId cid $ "SystemAPIKeyDeepSeek set to " <> tshow mdt ]

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

catSet (View PerChat item) = do
  (_, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  catSet (View (PerChatWithChatId cid) item)

catSet (View (PerChatWithChatId cid) item) = do
  botname <- query
  case item of
    DisplayThinking      _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDisplayThinking . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "DisplayThinking: " <> tshow mdt]
    DefaultModel         _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDefaultModel . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "DefaultModel: " <> tshow mdt]
    DefaultModelSuper    _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatDefaultModelS . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "DefaultModelSuper: " <> tshow mdt]
    SystemMessage        _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemMessage . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemMessage: " <> tshow mdt]
    SystemTemp           _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemTemp . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemTemp: " <> tshow mdt]
    SystemMaxToolDepth   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemMaxToolDepth . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemMaxToolDepth: " <> tshow mdt]
    SystemAPIKeyOpenAI   _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyOpenAI . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemAPIKeyOpenAI: " <> tshow mdt]
    SystemAPIKeyDeepSeek _ -> do
      mdt <- lift $ runDB $ fmap (botSettingPerChatSystemAPIKeyDeepSeek . entityVal) <$> selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
      return [baSendToChatId cid $ "SystemAPIKeyDeepSeek: " <> tshow mdt]


