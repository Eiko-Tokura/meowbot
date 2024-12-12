{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | This is a module that provides the function of logging messages into database.
module Module.LogDatabase where

import Module
import MeowBot.BotStructure
import Control.Monad.Reader
import Control.Monad.State
import Data.PersistModel
import Data.Proxy
import Database.Persist.Sqlite
import Data.Pool

data LogDatabase -- ^ The module that tries to log all CQMessage received into the database.

instance MeowModule BotModules AllData LogDatabase where
  type ModuleState LogDatabase = Pool SqlBackend

  initModule _ r = do
    pool <- createSqlitePool "meowbot.db" 2
    flip runSqlPool pool $ runMigration migrateAll
    return pool
      -- ^ run the migration, which will create the table if not exists, and add the columns if not exists.

  quitModule _ = do
    pool <- gets fst
    liftIO $ destroyAllResources pool

  afterMeow _ = do
    botname <- gets (_ . snd)
    mNewMessage <- gets (cqMessageToChatMessage botname . getNewMsg . wholechat . snd)
    case mNewMessage of
      Nothing -> return ()
      Just newMessage -> do
        pool <- gets fst
        flip runSqlPool pool $ do
          insert_ newMessage
