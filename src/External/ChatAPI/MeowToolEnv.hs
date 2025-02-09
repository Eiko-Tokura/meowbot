module External.ChatAPI.MeowToolEnv where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.ReaderState
import Data.HList
import Database.Persist.Sql hiding (In)
import MeowBot.BotStructure
import Module.LogDatabase
import System
import System.Meow
import System.General

-- | This will be the monad the tools will run in, ReaderT is necessary to make UnliftIO work
newtype MeowToolEnv r mods a = MeowToolEnv
  { runMeowToolEnv
    :: ReaderT
        ( ( (WholeChat, BotConfig)
          , (AllModuleGlobalStates mods, r)
          )
        , ( AllModuleLocalStates mods
          , OtherData
          )
        )
        (LoggingT IO)
        a
  } deriving newtype
    ( Functor, Applicative, Monad, MonadUnliftIO, MonadIO
    , MonadReader
        ( ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r))
        , (AllModuleLocalStates mods, OtherData)
        )
    )

type MeowToolEnvDefault = MeowToolEnv MeowData Mods

getBotName :: MeowToolEnv r mods (Maybe String)
getBotName = asks (maybeBotName . nameOfBot . botModules . snd . fst . fst)
{-# INLINE getBotName #-}

getCid :: MeowToolEnv r mods (Maybe ChatId)
getCid = asks (cqmsgToCid . getNewMsg . fst . fst . fst)
{-# INLINE getCid #-}

embedMeowToolEnv :: MeowToolEnv r mods a -> MeowT r mods IO a
embedMeowToolEnv
  = MeowT
  . ReaderStateT
  . (\arr (wc_bc, allGlob_r) allLocl_other -> fmap (, allLocl_other) $ arr ((wc_bc, allGlob_r), allLocl_other)
    )
  . runReaderT
  . runMeowToolEnv
{-# INLINE embedMeowToolEnv #-}

runDBMeowTool :: (LogDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowToolEnv r mods b
runDBMeowTool acts = do
  pool <- databasePool <$> asks (getF @LogDatabase . fst . snd . fst)
  liftIO $ runSqlPool acts pool
