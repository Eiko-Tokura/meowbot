module External.ChatAPI.MeowToolEnv where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Reader (ReaderT(..))
import Module.RS
import Data.TypeList
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Effect
import Control.Monad.RS.Class
import Data.Maybe
import Database.Persist.Sql hiding (In)
import MeowBot.BotStructure
import MeowBot.GetSelfInfo ( isSelfAdminInGroup )
import MeowBot.CostModel
import System.Meow
import Module.MeowTypes

type MeowToolEnvDefault = MeowToolEnv Mods
type MeowToolEnv mods = MeowT mods IO
-- | This will be the monad the tools will run in, ReaderT is necessary to make UnliftIO work
-- newtype MeowToolEnv a = MeowToolEnv
--   { runMeowToolEnv
--     -- :: ReaderT
--     --     ( ( (WholeChat, BotConfig)
--     --       , (AllModuleGlobalStates mods, r)
--     --       )
--     --     , ( AllModuleLocalStates mods
--     --       , OtherData
--     --       )
--     --     )
--     --     (LoggingT IO)
--       :: EffT
--           '[RModule WholeChat, RModule BotConfig]
--           a
--   } deriving newtype
--     ( Functor, Applicative, Monad, MonadUnliftIO, MonadIO, MonadLogger
--     , MonadReader
--         ( ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r))
--         , (AllModuleLocalStates mods, OtherData)
--         )
--     )

overrideMeowToolEnv :: MeowAllData mods => OverrideSettings -> MeowToolEnv mods a -> MeowToolEnv mods a
overrideMeowToolEnv override = local ( _overrideSettings ?~ override )
  -- ( \( (wc_bc, allGlob_r), localStates_otherdata) ->
  --     ( ((fst wc_bc, (snd wc_bc) { overrideSettings = Just override })
  --       , allGlob_r
  --       )
  --     , localStates_otherdata
  --     )
  -- )

meowGroupAdmin :: MeowAllData mods => MeowToolEnv mods Bool
meowGroupAdmin = fmap (fromMaybe False) . runMaybeT $ do
    gid  <- MaybeT getGid
    self <- MaybeT meowGetSelfInfo
    MaybeT $ pure $ isSelfAdminInGroup self gid

meowGetSelfInfo :: MeowAllData mods => MeowToolEnv mods (Maybe SelfInfo)
meowGetSelfInfo = getsS selfInfo

getBotName :: MeowAllData mods => MeowToolEnv mods (Maybe String)
getBotName = getsS (maybeBotName . nameOfBot . botModules)
{-# INLINE getBotName #-}

getBotId :: MeowAllData mods => MeowToolEnv mods BotId
getBotId = getsS (botId . botModules)
{-# INLINE getBotId #-}

getCid :: MeowAllData mods => MeowToolEnv mods (Maybe ChatId)
getCid =
  (liftA2 (<|>)
    (getsS $ chatIdOverride <=< overrideSettings)
    (getsS $ cqmsgToCid <=< getNewMsg)
  )
{-# INLINE getCid #-}

getGid :: MeowAllData mods => MeowToolEnv mods (Maybe GroupId)
getGid = do
  cid <- getCid
  return $ case cid of
    Just (GroupChat gid) -> Just gid
    _                    -> Nothing
{-# INLINE getGid #-}

isGroupChat :: MeowAllData mods => MeowToolEnv mods Bool
isGroupChat = do
  cid <- getCid
  return $ case cid of
    Just (GroupChat _) -> True
    _                  -> False
{-# INLINE isGroupChat #-}

hasCostModel :: (MeowDatabase `In` mods, MeowAllData mods) => MeowToolEnv mods Bool
hasCostModel = fmap (fromMaybe False) . runMaybeT $ do
  botid <- lift getBotId
  cid   <- MaybeT getCid
  srvCk <- lift $ runMeowDBMeowTool $ serviceBalanceCheck botid cid
  case srvCk of
    NoCostModelAssigned -> return False
    _                   -> return True

embedMeowToolEnv :: (SubList FData mods Mods, MeowAllData mods) => MeowToolEnv mods a -> Meow a
embedMeowToolEnv = embedEffT
{-# INLINE embedMeowToolEnv #-}

runMeowDBMeowTool :: (MeowDatabase `In` mods) => ReaderT SqlBackend IO b -> MeowToolEnv mods b
runMeowDBMeowTool = runMeowDB
