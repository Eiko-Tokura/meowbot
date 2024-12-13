{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia, TemplateHaskell, UndecidableInstances #-}
module Module.CommandInstance where

import Control.Monad.Trans.ReaderState
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

import Module.Command
import Module

import Utils.ByteString

allPrivateCommands :: [BotCommand]
allPrivateCommands = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook, commandPoll]

allGroupCommands :: [BotCommand]
allGroupCommands   = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook, commandPoll]

instance 
  ( HasSystemRead (TVar [Meow [BotAction]]) r
  , HasSystemRead Connection r
  ) 
  => MeowModule r AllData CommandModule where

  data ModuleLocalState  CommandModule = CommandL { msgAsync :: Async BL.ByteString }
  data ModuleGlobalState CommandModule = CommandG
  data ModuleEvent CommandModule = CommandEvent { msgBS :: BL.ByteString }

  data ModuleInitDataG CommandModule = CommandInitDataG
  data ModuleInitDataL CommandModule = CommandInitDataL

  getInitDataG _ = (Just CommandInitDataG, empty)

  getInitDataL _ = (Just CommandInitDataL, empty)

  initModule _ _ = return CommandG

  initModuleLocal _ r _ _ = do
    let conn = readSystem r
    liftIO $ CommandL <$> async (receiveData conn)

  moduleEvent _ = do
    CommandL asyncMessage <- readModuleStateL (Proxy @CommandModule)
    return $ CommandEvent <$> waitSTM asyncMessage

  moduleEventHandler _ (CommandEvent msg) = do
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
      Left errMsg -> liftIO $ putStrLn $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msg)
      Right cqmsg -> case eventType cqmsg of
        LifeCycle -> updateSelfInfo cqmsg
        HeartBeat -> return ()
        Response -> do
          change $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg
          updateSavedAdditionalData
          liftIO $ putStrLn $ nameBot ++ " <- response."
        PrivateMessage -> do
          updateStates nameBot cqmsg
          tmeow <- readSystem <$> asks snd
          liftIO $ atomically $ modifyTVar tmeow $ (<> extractBotActions pcmds)
        GroupMessage -> do
          updateStates nameBot cqmsg
          tmeow <- readSystem <$> asks snd
          liftIO $ atomically $ modifyTVar tmeow $ (<> extractBotActions gcmds)
        _ -> return ()
        where
          updateStates nameBot cqmsg = do
            cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> increaseAbsoluteId
            change $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg'
            updateSavedAdditionalData
            liftIO $ putStrLn $ nameBot ++ " <- " ++ showCQ cqmsg'
    asyncNew <- liftIO $ async $ receiveData conn
    modifyModuleState (Proxy @CommandModule) $ const $ CommandL asyncNew

