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
import Module
import Debug.Trace
import Parser.Run
import Parser.Except

--------------------------------------------------------------------------------------------------
data LogDatabase -- ^ The module that tries to log all CQMessage received into the database.

instance
  ( HasSystemRead (TVar (Maybe ReceCQMessage)) r
  , HasSystemRead (TVar (Maybe SentCQMessage)) r
  ) => MeowModule r AllData LogDatabase where
  data ModuleGlobalState LogDatabase = LogDatabaseGlobalState { databasePool :: Pool SqlBackend }
  data ModuleLocalState  LogDatabase = LogDatabaseLocalState
  data ModuleEvent       LogDatabase = LogDatabaseEvent
  data ModuleInitDataG   LogDatabase = LogDatabaseInitDataG { databasePath :: String }
  data ModuleInitDataL   LogDatabase = LogDatabaseInitDataL
  data ModuleEarlyLocalState LogDatabase = LogDatabaseEarlyLocalState

  getInitDataG _ = (Just (LogDatabaseInitDataG "meowbot.db"), liftR1 just "--database" >> withE "--database needs a path argument" (LogDatabaseInitDataG <$> nonFlagString))

  getInitDataL _ = (Just LogDatabaseInitDataL, empty)

  initModule _ (LogDatabaseInitDataG path) = do
    pool <- createSqlitePool (pack path) 1
    runMigration migrateAll `runSqlPool` pool
    -- ^ run the migration, which will create the table if not exists, and add the columns if not exists.
    return $ LogDatabaseGlobalState pool

  initModuleLocal _ _ _ _ _ = return LogDatabaseLocalState

  initModuleEarlyLocal _ _ _ = return LogDatabaseEarlyLocalState

  quitModule _ = do
    LogDatabaseGlobalState pool <- readModuleStateG (Proxy @LogDatabase)
    liftIO $ destroyAllResources pool

  afterMeow _ = do
    -- | Only update when there is new message, avoids multiple insertions.
    -- record both received and sent messages.
    mrcq        <- askSystem @(TVar (Maybe ReceCQMessage)) >>= liftIO . atomically . readTVar
    mscq        <- askSystem @(TVar (Maybe SentCQMessage)) >>= liftIO . atomically . readTVar
    let mcq = coerce mrcq <|> coerce mscq
    botname     <- gets (nameOfBot . botModules . botConfig . snd)
    mNewMessage <- gets (cqMessageToChatMessage botname . getNewMsg . wholechat . snd)
    case (mcq, mNewMessage) of
      (Just cq, Just newMessage) -> when (eventType cq `elem` [PrivateMessage, GroupMessage, SelfMessage]) $ do
        readModuleStateG (Proxy @LogDatabase) >>= lift . runSqlPool (insert_ newMessage) . databasePool
        $(logDebug) "Inserted a new message into the database."
      _    -> do
        return ()

--------------------------------------------------------------------------------------------------
-- useful logging functions
--
-- | A tracing function that will only print the message when the flag is in the list.
traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a
  | flag `elem` ls = trace (f a) a
  | otherwise      = a

