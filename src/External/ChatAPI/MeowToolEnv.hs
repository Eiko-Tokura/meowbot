module External.ChatAPI.MeowToolEnv where

import Control.Applicative
import Control.Monad
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
    ( Functor, Applicative, Monad, MonadUnliftIO, MonadIO, MonadLogger
    , MonadReader
        ( ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r))
        , (AllModuleLocalStates mods, OtherData)
        )
    )

type MeowToolEnvDefault = MeowToolEnv MeowData Mods

overrideMeowToolEnv :: OverrideSettings -> MeowToolEnv r mods a -> MeowToolEnv r mods a
overrideMeowToolEnv override = local
  ( \( (wc_bc, allGlob_r), localStates_otherdata) ->
      ( ((fst wc_bc, (snd wc_bc) { overrideSettings = Just override })
        , allGlob_r
        )
      , localStates_otherdata
      )
  )

getBotName :: MeowToolEnv r mods (Maybe String)
getBotName = asks (maybeBotName . nameOfBot . botModules . snd . fst . fst)
{-# INLINE getBotName #-}

getBotId :: MeowToolEnv r mods BotId
getBotId = asks (botId . botModules . snd . fst . fst)
{-# INLINE getBotId #-}

getCid :: MeowToolEnv r mods (Maybe ChatId)
getCid = asks
  (liftA2 (<|>)
    ((chatIdOverride <=< overrideSettings) . snd . fst . fst)
    (cqmsgToCid . getNewMsg . fst . fst . fst)
  )
{-# INLINE getCid #-}

getGid :: MeowToolEnv r mods (Maybe GroupId)
getGid = do
  cid <- getCid
  return $ case cid of
    Just (GroupChat gid) -> Just gid
    _                    -> Nothing
{-# INLINE getGid #-}

isGroupChat :: MeowToolEnv r mods Bool
isGroupChat = do
  cid <- getCid
  return $ case cid of
    Just (GroupChat _) -> True
    _                  -> False
{-# INLINE isGroupChat #-}

embedMeowToolEnv :: MeowToolEnv r mods a -> MeowT r mods IO a
embedMeowToolEnv
  = MeowT
  . ReaderStateT
  . (\arr (wc_bc, allGlob_r) allLocl_other -> (, allLocl_other) <$> arr ((wc_bc, allGlob_r), allLocl_other)
    )
  . runReaderT
  . runMeowToolEnv
{-# INLINE embedMeowToolEnv #-}

runDBMeowTool :: (LogDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowToolEnv r mods b
runDBMeowTool acts = do
  pool <- asks (databasePool . getF @LogDatabase . fst . snd . fst)
  liftIO $ runSqlPool acts pool
