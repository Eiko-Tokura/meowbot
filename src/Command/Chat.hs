{-# LANGUAGE TemplateHaskell #-}
module Command.Chat where

import Command
import Command.Cat (catParser)
import MeowBot
import System.General (MeowT)
import Data.Maybe (fromMaybe, catMaybes)
import External.ChatAPI
import External.ChatAPI as API
import External.ChatAPI.Tool
import qualified MeowBot.Parser as MP
import qualified Data.Map.Strict as SM
import qualified Data.Text as T

import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState
import Command.Cat.CatSet

import Data.PersistModel
import Data.Proxy
import Data.HList
import Data.Coerce
import Utils.RunDB
import Utils.Persist
import Data.Additional.Default

import Probability.Foundation

type MeowTools = '[] -- empty for now
type ModelChat = Local DeepSeekR1_14B

maxMessageInState :: Int
maxMessageInState = 24
-- we will have to mantain a ChatState for each chat
data ChatState = ChatState
  { chatStatus :: !ChatStatus
  , meowStatus :: !MeowStatus -- ^ avoids crafting too many messages simultaneously
  } deriving (Show, Eq, Typeable)

data MeowStatus = MeowIdle | MeowBusy deriving (Show, Eq, Typeable)

type AllChatState = SM.Map ChatId ChatState -- since we are keeping it as state, use strict map
instance IsAdditionalData AllChatState      -- use getTypeWithDef

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
  ess@(msg, cid, uid, mid, _) <- MaybeT $ getEssentialContent <$> query
  cqmsg <- queries getNewMsg
  other_data <- query
  whole_chat :: WholeChat <- query
  botmodules <- query
  botname    <- query
  botSetting        <- lift $ fmap (fmap entityVal) . runDB $ selectFirst [BotSettingBotName ==. maybeBotName botname] []
  botSettingPerChat <- lift $ fmap (fmap entityVal) . runDB $ selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotName ==. maybeBotName botname] []
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
      sd = savedData other_data
      activeChat = fromMaybe False $ asum
        [ botSettingPerChatActiveChat =<< botSettingPerChat
        , botSettingActiveChat =<< botSetting
        ] -- ^ whether to chat actively randomly
      modelCat = fromMaybe modelCat $ runPersistUseShow <$> asum
        [ botSettingPerChatDefaultModel =<< botSettingPerChat
        , botSettingDefaultModel =<< botSetting
        ]
      displayThinking = fromMaybe False $ asum
        [ botSettingPerChatDisplayThinking =<< botSettingPerChat
        , botSettingDisplayThinking =<< botSetting
        ]
      newChatState = SM.empty :: AllChatState
      toUserMessage :: EssentialContent -> Message
      toUserMessage (msg, _, _, _, sender) = UserMessage $ mconcat $ catMaybes $ 
        [ (\t -> "<role>" <> t <> "</role>") . roleToText <$> senderRole sender
        , fmap (\t -> "<nickname>" <> t <> "</nickname>") (senderNickname sender) <> Just ": "
        , Just msg
        ]
      updateChatState :: AllChatState -> AllChatState
      updateChatState s =
        let state = SM.lookup cid s in
        case state of
          Just cs -> SM.insert cid 
            cs 
              { chatStatus = (chatStatus cs) 
                { chatStatusMessages = strictTakeTail maxMessageInState $ chatStatusMessages (chatStatus cs) ++ [toUserMessage ess]
                }
              }
            s
          Nothing -> SM.insert cid (ChatState (ChatStatus 0 0 [toUserMessage ess]) MeowIdle) s

      params = ChatParams False msys :: ChatParams ModelChat MeowTools

  MaybeT $ if activeChat then pure (Just ()) else pure Nothing
  -- ^ only chat when set to active
  $(logDebug) "Chat command is active"

  allChatState <- lift $ updateChatState <$> getTypeWithDef newChatState 
  -- ^ get the updated chat state

  lift $ putType $ allChatState
  -- ^ update in the state

  chatState <- pureMaybe $ SM.lookup cid allChatState

  $(logDebug) "Determining if reply"
  determineIfReply cid msg botname msys chatState
  $(logDebug) "Replying"

  let ioeResponse =
        case (cfListPickElem modelsInUse (\(Proxy :: Proxy a) -> chatModel @a == modelCat)) of
          Nothing ->
            statusChatReadAPIKey @ModelChat @MeowTools (coerce params) $ chatStatus chatState
          Just proxyCont -> proxyCont $ \(Proxy :: Proxy a) -> 
            statusChatReadAPIKey @a @MeowTools (coerce params) $ chatStatus chatState

  asyncAction <- liftIO $ do
    async $ do
      (eNewMsg, newStatus) <- ioeResponse
      case eNewMsg of
        Left err -> do
          putTextLn $ "Error: " <> err
          return $ do
            markMeow cid MeowIdle -- ^ update status to idle
            pure []
        Right newMsgs -> do
          return $ do
            markMeow cid MeowIdle -- ^ update status to idle
            mergeChatStatus cid newMsgs newStatus
            meowSendToChatIdFull cid (Just mid) [] [MReplyTo mid, MMessage (last newMsgs)] (content $ last newMsgs)

  -- update busy status
  lift $ markMeow cid MeowBusy

  return [BAAsync asyncAction]

markMeow :: ChatId -> MeowStatus -> Meow ()
markMeow cid meowStat = do
  allChatState <- getTypeWithDef SM.empty
  let chatState = SM.lookup cid allChatState
  case chatState of
    Nothing -> pure ()
    Just chatState ->
      putType $ SM.insert cid chatState { meowStatus = meowStat } allChatState

mergeChatStatus :: ChatId -> [Message] -> ChatStatus -> Meow ()
mergeChatStatus cid newMsgs newStatus = do
  allChatState <- getTypeWithDef SM.empty
  let chatState = SM.lookup cid allChatState
  case chatState of
    Nothing -> pure ()
    Just chatState ->
      putType $ SM.insert cid 
        chatState -- adding new messages to newest state
          { chatStatus = newStatus
            { chatStatusMessages = strictTakeTail maxMessageInState $ chatStatusMessages (chatStatus chatState) ++ newMsgs
            }
          }
        allChatState

determineIfReply :: ChatId -> Text -> BotName -> ChatSetting -> ChatState -> MaybeT (MeowT MeowData Mods IO) ()
determineIfReply GroupChat{} msg bn cs ChatState {meowStatus = MeowIdle} = do
  chance <- getUniformR (0, 100 :: Int)
  liftIO $ putTextLn $ "Chance: " <> tshow chance
  let chanceReply = boolMaybe $ chance <= 10
  let parsed = void $ MP.runParser (catParser bn cs) msg
  pureMaybe $ chanceReply <|> parsed
determineIfReply PrivateChat{} msg _ _ ChatState {meowStatus = MeowIdle} = do
  chance <- getUniformR (0, 100 :: Int)
  liftIO $ putTextLn $ "Chance (not used): " <> tshow chance
  if T.isPrefixOf ":" msg
  then pureMaybe Nothing
  else pureMaybe $ Just ()
determineIfReply _ _ _ _ _ = empty

boolMaybe :: Bool -> Maybe ()
boolMaybe True  = Just ()
boolMaybe False = Nothing
