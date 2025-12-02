{-# LANGUAGE TypeFamilies, TemplateHaskell, OverloadedStrings #-}
-- | This is a module that provides the function of logging messages into database.
module Module.LogDatabase where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Logger
import Control.System
import Data.Coerce
import Data.PersistModel.Data
import Database.Persist.Sql
import MeowBot.BotStructure
import Module.Logging
import Module.MeowTypes
import Module.RS
import Module.RS.QQ
import Module.RecvSentCQ


[makeRModule|
LogDatabase
|]

withLogDatabase :: (Monad m, ConsFDataList FData (LogDatabase : mods)) => EffT (LogDatabase : mods) es m a -> EffT mods es m a
withLogDatabase = runEffTOuter_ LogDatabaseRead LogDatabaseState

instance Dependency' c LogDatabase '[SModule WholeChat, SModule BotConfig, SModule OtherData, RecvSentCQ, LoggingModule, MeowDataDb] mods
  => Loadable c LogDatabase mods ies where
  withModule _ = runEffTOuter_ LogDatabaseRead LogDatabaseState

instance Dependency' c LogDatabase '[SModule WholeChat, SModule BotConfig, SModule OtherData, RecvSentCQ, LoggingModule, MeowDataDb] mods
  => EventLoop c LogDatabase mods es where
  afterEvent = do
    RecvSentCQRead {..} <- queryModule @RecvSentCQ
    mrcq <- liftIO $ readTVarIO meowRecvCQ
    mscq <- liftIO $ readTVarIO meowSentCQ
    let mcq = coerce mrcq <|> coerce mscq
    botname <- getsS (nameOfBot . botModules)
    botid   <- getsS (botId . botModules)
    mNewMessage <- getsS (cqMessageToChatMessage botid botname <=< getNewMsg)
    case (mcq, mNewMessage) of
      (Just cq, Just newMessage) -> when (eventType cq `elem` [PrivateMessage, GroupMessage, SelfMessage]) $ do
        $logDebug "Inserting a new message into the database."
        runMeowDataDB (insert_ newMessage) `effCatch` (\(ErrorText dbError :: ErrorText "meowDataDb") ->
          $logError $ "While trying to insert new message:" <> dbError
          )
      _    -> do
        return ()
