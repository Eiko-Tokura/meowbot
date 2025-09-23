{-# LANGUAGE TypeFamilies, TemplateHaskell, OverloadedStrings #-}
-- | This is a module that provides the function of logging messages into database.
module Module.LogDatabase where

import Control.Monad.Trans.ReaderState
import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad
import Database.Persist.Sqlite
import Data.PersistModel
import Data.Pool
import Data.Coerce
import MeowBot.BotStructure

import Module.RecvSentCQ
import Module.Database
import Module.Logging

import Control.Monad.Effect
import Control.System
import Module.RS.QQ
import Module.RS
import Module.MeowTypes

import Debug.Trace
import Parser.Run
import Parser.Except

[makeRModule|
LogDatabase
|]

instance Dependency' c LogDatabase '[SModule WholeChat, SModule BotConfig, SModule OtherData, RecvSentCQ, LoggingModule, MeowDatabase] mods
  => Loadable c LogDatabase mods where
  initModule _ = return (LogDatabaseRead, LogDatabaseState)

  afterEvent = do
    RecvSentCQRead {..} <- queryModule @RecvSentCQ
    mrcq <- liftIO $ readTVarIO meowRecvCQ
    mscq <- liftIO $ readTVarIO meowSentCQ
    let mcq = coerce mrcq <|> coerce mscq
    botname <- getsS (nameOfBot . botModules)
    botid   <- getsS (botId . botModules)
    mNewMessage <- getsS (cqMessageToChatMessage botid botname . getNewMsg)
    case (mcq, mNewMessage) of
      (Just cq, Just newMessage) -> when (eventType cq `elem` [PrivateMessage, GroupMessage, SelfMessage]) $ do
        runMeowDB (insert_ newMessage)
        $logDebug "Inserted a new message into the database."
      _    -> do
        return ()

-- | A tracing function that will only print the message when the flag is in the list.
traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a
  | flag `elem` ls = trace (f a) a
  | otherwise      = a

