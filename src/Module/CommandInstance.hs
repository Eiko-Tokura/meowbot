{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.CommandInstance where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Module.RS
import Module.RecvSentCQ
import Module.MeowConnection
import Control.System
import Control.Monad.Effect
import Control.Parallel.Strategies
import Data.Time
import Data.UpdateMaybe
import Data.Aeson (eitherDecode)
import Data.Maybe
import MeowBot.Action
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
import Module.Logging

import Utils.ByteString


allPrivateCommands :: [BotCommand]
allPrivateCommands = $(makeBotCommands $ filter (`notElem` [Retract]) [minBound .. maxBound :: CommandId])

-- [commandCat, commandChat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook, commandPoll, commandHangman]--, commandHaskell]

allGroupCommands :: [BotCommand]
allGroupCommands   = $(makeBotCommands [minBound .. maxBound :: CommandId])

-- [commandCat, commandChat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook, commandPoll, commandHangman]--, commandHaskell]

instance Module CommandModule where

  data ModuleRead CommandModule  = CommandModuleRead
  data ModuleState CommandModule = CommandModuleState { msgAsync :: Async (Result '[ErrorText "recv_connection"] BL.ByteString) }

instance SystemModule CommandModule where
  data ModuleInitData CommandModule = CommandModuleInitData
  data ModuleEvent CommandModule = CommandModuleEvent { msgBS :: Result '[ErrorText "recv_connection"] BL.ByteString }

instance Dependency CommandModule 
  '[MeowActionQueue, RecvSentCQ, MeowConnection
  , SModule WholeChat, SModule OtherData, SModule BotConfig
  , LoggingModule] mods => Loadable FData CommandModule mods ies where
  withModule _ act = do
    asyncMessage <- embedError $ asyncEffT_ recvLBSData
    runEffTOuter_ CommandModuleRead (CommandModuleState asyncMessage) act

instance
  ( Dependency CommandModule 
    '[MeowActionQueue, RecvSentCQ, MeowConnection
     , SModule WholeChat, SModule OtherData, SModule BotConfig
     , LoggingModule] mods
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

  moduleEvent = do
    CommandModuleState asyncMessage <- getModule
    return $ CommandModuleEvent <$> waitSTM asyncMessage

  handleEvent (CommandModuleEvent (RFailure (EHead recvErr))) = do
    $logError $ "Error in receiving data from connection: " <> pack (show recvErr)
    effThrow recvErr
    -- ^ rethrown to the top-level, which will restart the bot after a delay

  handleEvent (CommandModuleEvent (RSuccess msg)) = do
     -- | When received any raw message, put into the tvar so other modules can be notified of the message
     asksModule meowRawByteString >>= liftIO . atomically . (`writeTVar` Just msg)

     botMods <- getsS botModules
     --mode <- gets (botConfig . snd)
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
           modifyTVar tvarQueryList (filter ((/= qid) . fst))
           modifyTVar tmeow (<> [meowAction])
       Nothing -> do
     ------------Command---------------------
         case eCQmsg of
           Left errMsg -> $(logError) $ pack $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msg)
           Right cqmsg -> do
             asksModule meowRecvCQ >>= liftIO . atomically . (`writeTVar` (Just . ReceCQMessage $ cqmsg))
             $(logDebug) $ pack nameBot <> " <- " <> fromLazyByteString msg
             case eventType cqmsg of
               LifeCycle -> do
                 $(logDebug) "LifeCycle event."
                 updateSelfInfo cqmsg
                 $(logDebug) "self info updated."
               HeartBeat -> return ()
               Response -> do
                 embedEffT $ updateWholeChatByMessage cqmsg
                 modifyS (`using` rseqWholeChat) 
                 updateSavedAdditionalData
                 $(logInfo) $ pack nameBot <> " <- response."
               PrivateMessage -> do
                 embedEffT $ updateStates nameBot cqmsg
                 tmeow   <- asksModule meowReadsAction
                 liftIO $ atomically $ modifyTVar tmeow (<> [botCommandsWithIgnore cqmsg pcmds, botMessageCounter cqmsg])
               GroupMessage -> do
                 embedEffT $ updateStates nameBot cqmsg
                 tmeow   <- asksModule meowReadsAction
                 liftIO $ atomically $ modifyTVar tmeow (<> [botCommandsWithIgnore cqmsg gcmds, botMessageCounter cqmsg])
               RequestEvent -> do
                 tmeow <- asksModule meowReadsAction
                 liftIO $ atomically $ modifyTVar tmeow (<> [botHandleRequestEvent cqmsg nameBot])
               _ -> return ()
             where
               updateStates :: MonadIO m => String -> CQMessage -> EffT '[SModule WholeChat, SModule OtherData, LoggingModule] NoError m ()
               updateStates name cqm = do
                 cqmsg' <- (\mid -> cqm {absoluteId = Just mid}) <$> embedMods increaseAbsoluteId
                 embedEffT $ updateWholeChatByMessage cqmsg'
                 modifyS (`using` rseqWholeChat)
                 updateSavedAdditionalData
                 $(logInfo) $ pack name <> " <- " <> pack (showCQ cqmsg')

     -------Next Async-------------------
     -- | creating a new async to receive the next message
     asyncNew <- embedNoError $ asyncEffT_ recvLBSData
     putModule (CommandModuleState asyncNew)

removeOutdatedQuery :: UTCTime -> TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) )] -> STM ()
removeOutdatedQuery time tvar = do
  lst <- readTVar tvar
  let lst' = filter (\(_, WithTime t _) -> diffUTCTime time t < 60) lst
  writeTVar tvar lst'
