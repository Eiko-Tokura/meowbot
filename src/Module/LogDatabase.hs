{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | This is a module that provides the function of logging messages into database.
module Module.LogDatabase where

import Control.Monad.Trans.ReaderState
import Database.Persist.Sqlite
import Data.PersistModel
import Data.Pool
import MeowBot.BotStructure
import Module

data LogDatabase -- ^ The module that tries to log all CQMessage received into the database.

instance MeowModule r AllData LogDatabase where
  type ModuleGlobalState LogDatabase = Pool SqlBackend
  type ModuleLocalState LogDatabase = ()

  initModule _ _ = do
    pool <- createSqlitePool "meowbot.db" 2
    runMigration migrateAll `runSqlPool` pool
    -- ^ run the migration, which will create the table if not exists, and add the columns if not exists.
    return pool

  initModuleLocal _ _ = return ()

  quitModule _ = do
    pool <- readModuleStateG (Proxy @LogDatabase)
    liftIO $ destroyAllResources pool

  afterMeow _ = do
    botname     <- gets (nameOfBot . botModules . botConfig . snd)
    mNewMessage <- gets (cqMessageToChatMessage botname . getNewMsg . wholechat . snd)
    case mNewMessage of
      Nothing         -> return ()
      Just newMessage -> readModuleStateG (Proxy @LogDatabase) >>= lift . runSqlPool (insert_ newMessage)

