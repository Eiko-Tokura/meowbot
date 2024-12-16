{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.CommandInstance where

import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Applicative
import Control.Parallel.Strategies
import System.Meow
import MeowBot.BotStructure
import MeowBot.Update
import Data.Maybe
import Data.Aeson
import Network.WebSockets hiding (Response)
import qualified Data.ByteString.Lazy as BL

import Command
import Command.Cat
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

import Module.Command
import Module

import Utils.ByteString

allPrivateCommands :: [BotCommand]
allPrivateCommands = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook, commandPoll, commandHangman]

allGroupCommands :: [BotCommand]
allGroupCommands   = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook, commandPoll, commandHangman]

instance
  ( HasSystemRead (TVar [Meow [BotAction]]) r    -- ^ the channel to put meow actions
  , HasSystemRead (TVar (Maybe ReceCQMessage)) r     -- ^ other modules can use
  , HasSystemRead (TVar (Maybe SentCQMessage)) r     -- ^ other modules can use
  , HasSystemRead (TVar (Maybe BL.ByteString)) r -- ^ other modules can use
  , HasSystemRead Connection r                   -- ^ the connection to the server
  )
  => MeowModule r AllData CommandModule where

  data ModuleLocalState  CommandModule = CommandL { msgAsync :: Async BL.ByteString }
  data ModuleGlobalState CommandModule = CommandG
  data ModuleEvent CommandModule = CommandEvent { msgBS :: BL.ByteString }

  data ModuleInitDataG CommandModule = CommandInitDataG
  data ModuleInitDataL CommandModule = CommandInitDataL
  data ModuleEarlyLocalState CommandModule = CommandEarlyLocalState

  getInitDataG _ = (Just CommandInitDataG, empty)

  getInitDataL _ = (Just CommandInitDataL, empty)

  initModule _ _ = return CommandG

  initModuleLocal _ r _ _ _ = do
    let conn = readSystem r
    liftIO $ CommandL <$> async ( receiveData conn )

  initModuleEarlyLocal _ _ _ = return CommandEarlyLocalState

  beforeMeow _ = do -- ^ clear the tvars in each loop
    tvarRCQmsg <- askSystem @(TVar (Maybe ReceCQMessage))
    tvarSCQmsg <- askSystem @(TVar (Maybe SentCQMessage))
    tvarBS     <- askSystem @(TVar (Maybe BL.ByteString))
    liftIO . atomically $ writeTVar tvarRCQmsg Nothing
    liftIO . atomically $ writeTVar tvarSCQmsg Nothing
    liftIO . atomically $ writeTVar tvarBS     Nothing

  moduleEvent _ = do
    CommandL asyncMessage <- readModuleStateL (Proxy @CommandModule)
    return $ CommandEvent <$> waitSTM asyncMessage

  moduleEventHandler _ (CommandEvent msg) = do
    askSystem @(TVar (Maybe BL.ByteString)) >>= liftIO . atomically . (`writeTVar` (Just msg))
    botMods <- gets (botModules . botConfig . snd)
    --mode <- gets (botConfig . snd)
    let gcmdids = canUseGroupCommands   botMods
        pcmdids = canUsePrivateCommands botMods
        pcmds   = filter ((`elem` pcmdids) . identifier) allPrivateCommands
        gcmds   = filter ((`elem` gcmdids) . identifier) allGroupCommands
        nameBot = fromMaybe "喵喵" $ maybeBotName $ nameOfBot botMods
        -- mods    = botMods
        eCQmsg  = eitherDecode msg :: Either String CQMessage
    conn <- readSystem <$> asks snd
    --case traceModeWith DebugCQMessage mode (((nameBot ++ "debug: ") ++) . show) eCQmsg of
    case eCQmsg of
      Left errMsg -> $(logError) $ pack $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msg)
      Right cqmsg -> do
        askSystem @(TVar (Maybe ReceCQMessage)) >>= liftIO . atomically . (`writeTVar` (Just . ReceCQMessage $ cqmsg))
        $(logDebug) $ pack nameBot <> " <- " <> fromLazyByteString msg
        case eventType cqmsg of
          LifeCycle -> updateSelfInfo cqmsg
          HeartBeat -> return ()
          Response -> do
            change $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg
            updateSavedAdditionalData
            $(logInfo) $ pack nameBot <> " <- response."
          PrivateMessage -> do
            updateStates nameBot cqmsg
            tmeow <- readSystem <$> asks snd
            liftIO $ atomically $ modifyTVar tmeow $ (<> botCommandsToMeow pcmds)
          GroupMessage -> do
            updateStates nameBot cqmsg
            tmeow <- readSystem <$> asks snd
            liftIO $ atomically $ modifyTVar tmeow $ (<> botCommandsToMeow gcmds)
          _ -> return ()
        where
          updateStates nameBot cqmsg = do
            cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> increaseAbsoluteId
            change $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg'
            updateSavedAdditionalData
            $(logInfo) $ pack nameBot <> " <- " <> pack (showCQ cqmsg')
    -- | creating a new async to receive the next message
    asyncNew <- liftIO $ async $ receiveData conn
    modifyModuleState (Proxy @CommandModule) $ const $ CommandL asyncNew

