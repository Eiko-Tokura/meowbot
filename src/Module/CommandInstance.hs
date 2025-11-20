{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.CommandInstance where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Module.RS
import Module.RecvSentCQ
import Module.MeowConnection
import Control.System
import Control.Monad.Effect
import Data.Time
import Data.UpdateMaybe
import Data.Aeson (eitherDecode)
import Data.Maybe
import MeowBot.Handler.Requests
import MeowBot.BotStructure
import MeowBot.Update
import Network.WebSockets hiding (Response)
import System.Meow
import qualified Data.ByteString.Lazy as BL

import MeowBot.CommandRule
import Command
import Command.Balance
import Command.Cat
import Command.Chat
import Command.Md
import Command.Help
import Command.SetSysMessage
import Command.User
import Command.Aokana
import Command.Random (commandRandom)
import Command.Retract
import Command.Study
import Command.Poll
import Command.Hangman
import Command.Updater
import Command.Statistics
-- import Command.Haskell

import Module.Command
import Module.BotGlobal
import Module.Logging
import Module.Prometheus
import Module.Prometheus.Counter
import Module.Prometheus.Manager

import Utils.ByteString
import Utils.LabelKeys ()
import Utils.List

import qualified Data.HashMap.Strict as HM

allPrivateCommands :: [BotCommand]
allPrivateCommands = $(makeBotCommands $ filter (`notElem` [Retract]) [minBound .. maxBound :: CommandId])

allGroupCommands :: [BotCommand]
allGroupCommands   = $(makeBotCommands [minBound .. maxBound :: CommandId])

instance Module CommandModule where

  newtype ModuleRead  CommandModule = CommandModuleRead
    { tbqueueMsg :: TBQueue (Result '[ErrorText "recv_connection"] BL.ByteString)
    }
  data ModuleState CommandModule = CommandModuleState
  -- { msgAsync :: Async (Result '[ErrorText "recv_connection"] BL.ByteString) }

instance SystemModule CommandModule where
  data ModuleInitData CommandModule = CommandModuleInitData
  newtype ModuleEvent CommandModule = CommandModuleEvent { msgBS :: Result '[ErrorText "recv_connection"] BL.ByteString }

instance Dependency CommandModule
  [ MeowActionQueue, RecvSentCQ, MeowConnection
  , SModule WholeChat, SModule OtherData, SModule BotConfig
  , PrometheusMan, LoggingModule
  ] mods => Loadable FData CommandModule mods ies where
  withModule _ act = bracketEffT
    (do newTBQ <- liftIO $ newTBQueueIO 10
        let recvThread = foreverEffT $ do
              msg <- errorToResult recvLBSData
              liftIO . atomically $ writeTBQueue newTBQ msg
        tid <- forkEffT recvThread
        return (CommandModuleRead newTBQ, tid)
    )
    (\(_, tid) -> liftIO $ killThread tid)
    (\(newTBQ, _) -> do
      runEffTOuter_ newTBQ CommandModuleState act
    )

instance
  ( Dependency CommandModule
      [ MeowActionQueue, RecvSentCQ, MeowConnection
      , SModule WholeChat, SModule OtherData, SModule BotConfig, BotGlobal
      , PrometheusMan
      , LoggingModule
      ] mods
  , InList (ErrorText "recv_connection") es
  )
  => EventLoop FData CommandModule mods es where
  beforeEvent = do -- ^ clear the tvars in each loop
    tvarRCQmsg <- asksModule meowRecvCQ
    tvarSCQmsg <- asksModule meowSentCQ
    tvarBS     <- asksModule meowRawByteString
    liftIO . atomically $ writeTVar tvarRCQmsg Nothing
    liftIO . atomically $ writeTVar tvarSCQmsg Nothing
    liftIO . atomically $ writeTVar tvarBS     Nothing

    tmeow <- asksModule meowReadsAction
    friendList <- (fmap (HM.keys . unWithTime) . toMaybe . selfFriends =<<) <$> queries selfInfo
    groupList  <- (fmap (HM.keys . unWithTime) . toMaybe . selfInGroups =<<) <$> queries selfInfo
    glbMesChan <- asksModule globalMessageChannel
    -- fmap listToMaybe . lift . readTVarIO =<< 
    liftIO . atomically $ do
      glbMesL <- readTVar glbMesChan
      forM_ glbMesL $ \case
        m@(PrivateChat uid, msg) ->
          when (uid `elem` fromMaybe [] friendList) $ do
                modifyTVar' tmeow ((pure $ pure $ BASendPrivate uid msg):)
                modifyTVar' glbMesChan (filter (/= m))
        m@(GroupChat gid, msg) ->
          when (gid `elem` fromMaybe [] groupList) $ do
                modifyTVar' tmeow ((pure $ pure $ BASendGroup gid msg):)
                modifyTVar' glbMesChan (filter (/= m))

  moduleEvent = do
    CommandModuleRead tbQ <- askModule
    return $ CommandModuleEvent <$> readTBQueue tbQ

  handleEvent (CommandModuleEvent (RFailure (EHead recvErr))) = do
    $logError $ "Error in receiving data from connection: " <> pack (show recvErr)
    effThrow recvErr
    -- ^ rethrown to the top-level, which will restart the bot after a delay

  handleEvent (CommandModuleEvent (RSuccess msg)) = do
     -- | When received any raw message, put into the tvar so other modules can be notified of the message
     asksModule meowRawByteString >>= liftIO . atomically . (`writeTVar` Just msg)

     botMods <- getsS botModules

     botId <- query

     let gcmdids = canUseGroupCommands   botMods
         pcmdids = canUsePrivateCommands botMods
         pcmds   = filter ((`elem` pcmdids) . identifier) allPrivateCommands
         gcmds   = filter ((`elem` gcmdids) . identifier) allGroupCommands
         nameBot = fromMaybe "喵喵" $ maybeBotName $ nameOfBot botMods
         -- mods    = botMods
         eCQmsg  = eitherDecode msg :: Either String CQMessage

     when (DebugJson `elem` botMods.botInstance.botDebugFlags) $
       $(logDebug) $ pack nameBot <> " <- " <> fromLazyByteString msg

     --------- Queries ----------------------
     time <- liftIO getCurrentTime
     tvarQueryList <- asksModule meowReadsQueries
     liftIO . atomically $ removeOutdatedQuery time tvarQueryList
     queriesList <- liftIO $ readTVarIO tvarQueryList
     let firstQuery = listToMaybe $ mapMaybe (\(qid, WithTime _ parser) -> (qid, ) <$> parser msg) queriesList

     tmeow <- asksModule meowReadsAction
     case firstQuery of
       Just (qid, meowAction) -> do
         effAddLogCat' (LogCat @Text "Query") $ $logInfo $ pack nameBot <> " <- (query " <> pack (show qid) <> ") Received response."
         liftIO . atomically $ do
           modifyTVar' tvarQueryList (rseqList . filter ((/= qid) . fst))
           modifyTVar' tmeow (<> [meowAction])
       Nothing -> do
     ------------Command---------------------
        labels <- case eCQmsg of
          Left errMsg -> do
            $(logError) $ pack $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msg)
            return [Label @"message_type" UnknownMessage]
          Right cqmsg -> do
            asksModule meowRecvCQ >>= liftIO . atomically . (`writeTVar` (Just . ReceCQMessage $ cqmsg))
            $(logDebug) $ pack nameBot <> " <- " <> fromLazyByteString msg
            case eventType cqmsg of
              LifeCycle -> do
                $(logDebug) "LifeCycle event."
                updateSelfInfo cqmsg
                $(logDebug) "self info updated."
                return [Label @"message_type" LifeCycle]
              HeartBeat -> return [Label @"message_type" HeartBeat]
              Response -> do
                embedEffT $ updateWholeChatByMessage cqmsg
                updateSavedAdditionalData
                $(logInfo) $ pack nameBot <> " <- response."
                return [Label @"message_type" Response]
              PrivateMessage -> do
                embedEffT $ updateStates nameBot cqmsg
                liftIO $ atomically $ modifyTVar tmeow (<> [botCommandsWithIgnore cqmsg pcmds, botMessageCounter cqmsg])
                return [ Label @"message_type" PrivateMessage
                       , Label @"chat_id" (fromMaybe (PrivateChat 0) $ cqmsgToCid cqmsg)
                       ]
              GroupMessage -> do
                embedEffT $ updateStates nameBot cqmsg
                liftIO $ atomically $ modifyTVar tmeow (<> [botCommandsWithIgnore cqmsg gcmds, botMessageCounter cqmsg])
                return [ Label @"message_type" GroupMessage
                       , Label @"chat_id" (fromMaybe (GroupChat 0) $ cqmsgToCid cqmsg)
                       ]
              RequestEvent -> do
                liftIO $ atomically $ modifyTVar tmeow (<> [botHandleRequestEvent cqmsg nameBot])
                return [Label @"message_type" RequestEvent]
              UnknownMessage -> do
                $logWarn $ toText nameBot <> " <- Unknown message received (unhandled): " <> bsToText msg
                return [Label @"message_type" UnknownMessage]
              _ -> do
                $logInfo $ "Other event received (unhandled): " <> toText cqmsg
                return [Label @"message_type" cqmsg.eventType]
            where
              updateStates :: String -> CQMessage -> EffT '[SModule WholeChat, SModule OtherData, LoggingModule] NoError IO ()
              updateStates name cqm = do
                cqmsg' <- (\mid -> cqm {absoluteId = Just mid}) <$> embedMods increaseAbsoluteId
                embedEffT $ updateWholeChatByMessage cqmsg'
                -- modifyS (`using` rseqWholeChat)
                updateSavedAdditionalData
                $(logInfo) $ pack name <> " <- " <> pack (showCQ cqmsg')
        managedCounter "meowbot_received_cqmessages_total" (Label @"bot_id" botId : labels) IncCounter

removeOutdatedQuery :: UTCTime -> TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) )] -> STM ()
removeOutdatedQuery time tvar = do
  lst <- readTVar tvar
  let lst' = rseqList $ filter (\(_, WithTime t _) -> diffUTCTime time t < 60) lst
  writeTVar tvar lst'
