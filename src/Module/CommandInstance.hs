{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.CommandInstance where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Module.RS
import Module.RecvSentCQ
import Module.MeowConnection
import Control.System
import Control.Monad.Effect
import Control.Monad.RS.Class
import Control.Parallel.Strategies
import Data.Time
import Data.UpdateMaybe
import Data.Aeson
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
  data ModuleState CommandModule = CommandModuleState { msgAsync :: Async BL.ByteString }

instance SystemModule CommandModule where
  data ModuleInitData CommandModule = CommandModuleInitData
  data ModuleEvent CommandModule = CommandModuleEvent { msgBS :: BL.ByteString }

  -- ( HasSystemRead (TVar [Meow [BotAction]]) r    -- ^ the channel to put meow actions
  -- , HasSystemRead (TVar (Maybe ReceCQMessage)) r -- ^ other modules can use
  -- , HasSystemRead (TVar (Maybe SentCQMessage)) r -- ^ other modules can use
  -- , HasSystemRead (TVar (Maybe BL.ByteString)) r -- ^ other modules can use
  -- , HasSystemRead (TVar [ (Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) ) ]) r -- ^ the channel to put query operations
  -- , HasSystemRead Connection r                   -- ^ the connection to the server
instance Dependency CommandModule 
  '[MeowActionQueue, RecvSentCQ, MeowConnection
  , SModule WholeChat, SModule OtherData, SModule BotConfig
  , LoggingModule] mods => Loadable FData CommandModule mods where
  initModule _ = do
    conn <- asksModule meowConnection
    asyncMessage <- liftIO $ async ( receiveData conn )
    return (CommandModuleRead, CommandModuleState asyncMessage)

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


  handleEvent (CommandModuleEvent msg) = do
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

     conn  <- asksModule meowConnection
     tmeow <- asksModule meowReadsAction
     case firstQuery of
       Just (qid, meowAction) -> do
         $logInfo $ pack nameBot <> " <- (query " <> pack (show qid) <> ") Received response."
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
     asyncNew <- liftIO $ async $ receiveData conn
     putModule (CommandModuleState asyncNew)
  -- data ModuleLocalState  CommandModule = CommandL { msgAsync :: Async BL.ByteString }
  -- data ModuleGlobalState CommandModule = CommandG
  -- data ModuleEvent CommandModule = CommandEvent { msgBS :: BL.ByteString }

  -- data ModuleInitDataG CommandModule = CommandInitDataG
  -- data ModuleInitDataL CommandModule = CommandInitDataL
  -- data ModuleEarlyLocalState CommandModule = CommandEarlyLocalState

  -- getInitDataG _ = (Just CommandInitDataG, empty)

  -- getInitDataL _ = (Just CommandInitDataL, empty)

  -- initModule _ _ = return CommandG

  -- initModuleLocal _ r _ _ _ = do
  --   let conn = readSystem r
  --   liftIO $ CommandL <$> async ( receiveData conn )

  -- initModuleEarlyLocal _ _ _ = return CommandEarlyLocalState

  -- beforeMeow _ = do -- ^ clear the tvars in each loop
  --   tvarRCQmsg <- askSystem @(TVar (Maybe ReceCQMessage))
  --   tvarSCQmsg <- askSystem @(TVar (Maybe SentCQMessage))
  --   tvarBS     <- askSystem @(TVar (Maybe BL.ByteString))
  --   liftIO . atomically $ writeTVar tvarRCQmsg Nothing
  --   liftIO . atomically $ writeTVar tvarSCQmsg Nothing
  --   liftIO . atomically $ writeTVar tvarBS     Nothing

  -- moduleEvent _ = do
  --   CommandL asyncMessage <- readModuleStateL (Proxy @CommandModule)
  --   return $ CommandEvent <$> waitSTM asyncMessage

  -- moduleEventHandler _ (CommandEvent msg) = do
  --   -- | When received any raw message, put into the tvar so other modules can be notified of the message
  --   askSystem @(TVar (Maybe BL.ByteString)) >>= liftIO . atomically . (`writeTVar` Just msg)

  --   botMods <- gets (botModules . botConfig . snd)
  --   --mode <- gets (botConfig . snd)
  --   let gcmdids = canUseGroupCommands   botMods
  --       pcmdids = canUsePrivateCommands botMods
  --       pcmds   = filter ((`elem` pcmdids) . identifier) allPrivateCommands
  --       gcmds   = filter ((`elem` gcmdids) . identifier) allGroupCommands
  --       nameBot = fromMaybe "喵喵" $ maybeBotName $ nameOfBot botMods
  --       -- mods    = botMods
  --       eCQmsg  = eitherDecode msg :: Either String CQMessage

  --   when (DebugJson `elem` botMods.botInstance.botDebugFlags) $
  --     $(logDebug) $ pack nameBot <> " <- " <> fromLazyByteString msg

  --   --------- Queries ----------------------
  --   time <- liftIO getCurrentTime
  --   tvarQueryList <- askSystem @(TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) )])
  --   liftIO . atomically $ removeOutdatedQuery time tvarQueryList
  --   queriesList <- liftIO $ readTVarIO tvarQueryList
  --   let firstQuery = listToMaybe $ mapMaybe (\(qid, WithTime _ parser) -> (qid, ) <$> parser msg) queriesList

  --   conn <- readSystem <$> asks snd
  --   tmeow <- readSystem <$> asks snd
  --   case firstQuery of
  --     Just (qid, meowAction) -> do
  --       $logInfo $ pack nameBot <> " <- (query " <> pack (show qid) <> ") Received response."
  --       liftIO . atomically $ do
  --         modifyTVar tvarQueryList (filter ((/= qid) . fst))
  --         modifyTVar tmeow (<> [meowAction])
  --     Nothing -> do
  --   ------------Command---------------------
  --       case eCQmsg of
  --         Left errMsg -> $(logError) $ pack $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msg)
  --         Right cqmsg -> do
  --           askSystem @(TVar (Maybe ReceCQMessage)) >>= liftIO . atomically . (`writeTVar` (Just . ReceCQMessage $ cqmsg))
  --           $(logDebug) $ pack nameBot <> " <- " <> fromLazyByteString msg
  --           case eventType cqmsg of
  --             LifeCycle -> do
  --               $(logDebug) "LifeCycle event."
  --               updateSelfInfo cqmsg
  --               $(logDebug) "self info updated."
  --             HeartBeat -> return ()
  --             Response -> do
  --               modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg
  --               updateSavedAdditionalData
  --               $(logInfo) $ pack nameBot <> " <- response."
  --             PrivateMessage -> do
  --               updateStates nameBot cqmsg
  --               tmeow   <- readSystem <$> asks snd
  --               liftIO $ atomically $ modifyTVar tmeow (<> [botCommandsWithIgnore cqmsg pcmds, botMessageCounter cqmsg])
  --             GroupMessage -> do
  --               updateStates nameBot cqmsg
  --               tmeow   <- readSystem <$> asks snd
  --               liftIO $ atomically $ modifyTVar tmeow (<> [botCommandsWithIgnore cqmsg gcmds, botMessageCounter cqmsg])
  --             RequestEvent -> do
  --               tmeow <- readSystem <$> asks snd
  --               liftIO $ atomically $ modifyTVar tmeow (<> [botHandleRequestEvent cqmsg nameBot])
  --             _ -> return ()
  --           where
  --             updateStates name cqm = do
  --               cqmsg' <- (\mid -> cqm {absoluteId = Just mid}) <$> increaseAbsoluteId
  --               modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg'
  --               updateSavedAdditionalData
  --               $(logInfo) $ pack name <> " <- " <> pack (showCQ cqmsg')

  --   -------Next Async-------------------
  --   -- | creating a new async to receive the next message
  --   asyncNew <- liftIO $ async $ receiveData conn
  --   modifyModuleState (Proxy @CommandModule) $ const $ CommandL asyncNew

removeOutdatedQuery :: UTCTime -> TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) )] -> STM ()
removeOutdatedQuery time tvar = do
  lst <- readTVar tvar
  let lst' = filter (\(_, WithTime t _) -> diffUTCTime time t < 60) lst
  writeTVar tvar lst'
