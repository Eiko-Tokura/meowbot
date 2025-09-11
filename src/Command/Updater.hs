-- | Updater is not a real command, but an executer for the update tasks.
module Command.Updater where

import Command
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Map.Strict (Map)
import Data.Time.Clock
import Data.UpdateMaybe
import MeowBot
import Module
import qualified Data.Map.Strict as Map

anHour :: NominalDiffTime
anHour = 3600

commandUpdater :: BotCommand
commandUpdater = BotCommand Updater $ botT $ do
  botid :: BotId <- query
  s@SelfInfo {selfId, selfInGroups} <- MaybeT $ queries selfInfo
  $logDebug $ "Bot ID: " <> toText botid <> ", SelfInfo: " <> toText s
  conn <- lift askSystem
  let updateLensSelfInGroups u = change $ \od -> od { selfInfo = Just s { selfInGroups = u } }

      updateGroupListInMapWith :: Ord g => [g] -> a -> Map g a -> Map g a
      updateGroupListInMapWith gids defVal mapG
        = Map.filterWithKey (\gid _ -> gid `elem` gids)
        $ Map.unionWith const mapG (Map.fromList [(gid, defVal) | gid <- gids])

      doNothing = MaybeT $ return Nothing

      fetchGroupList = do
        parseResponse <- queryAPI conn (GetGroupList False)
        let nextStep :: QueryAPIResponse 'QueryGroupList -> Meow [BotAction]
            nextStep (GetGroupListResponse ginfos) = botT $ do
              let gids = map groupBasicInfoGroupId ginfos
              $(logOther "UPDATER") $ toText botid <> " Fetched group list: " <> toText gids
              utcTime <- liftIO getCurrentTime
              s@SelfInfo {selfInGroups} <- MaybeT $ queries selfInfo
              let s' = s  { selfInGroups = updateUMaybeTime utcTime
                              (updateGroupListInMapWith gids NothingYet Map.empty)
                              (updateGroupListInMapWith gids NothingYet)
                              selfInGroups
                          }
              change $ \od -> od { selfInfo = Just s' }
              return []
        return $ pure $ BAQueryAPI [fmap nextStep . parseResponse]

  withExpireTimeDo anHour updateLensSelfInGroups selfInGroups fetchGroupList doNothing $ \groupMap -> do
    utcTime <- liftIO getCurrentTime

    let groupList = Map.toList groupMap
        updateNeeded = filter (needUpdate anHour utcTime . snd) groupList
        nextStepFor :: GroupId -> QueryAPIResponse 'QueryGroupMemberInfo -> Meow [BotAction]
        nextStepFor gid resp = botT $ do
          $(logOther "UPDATER") $ toText botid <> " Fetched group member info for group " <> toText gid <> ": " <> toText resp
          utcTime' <- liftIO getCurrentTime
          s@SelfInfo {selfInGroups} <- MaybeT $ queries selfInfo
          let s' = s { selfInGroups
                        = updateUMaybeTimeWithoutTime selfInGroups
                        $ flip Map.update gid
                        $ Just
                        . updateUMaybeTimeConst utcTime'
                            GroupInfo
                              { selfRole = resp.getGroupMemberInfoRole }
                     }
          change $ \od -> od { selfInfo = Just s' }
          return []

    -- | Mark all needed updates as Updating
    change $ \od -> od { selfInfo = Just s { selfInGroups
                                              = updateUMaybeTimeWithoutTime selfInGroups
                                              $ \m -> foldr (Map.update (Just . toUpdating) . fst) m updateNeeded
                                           } }

    parsers <- queryAPI conn `mapM` [GetGroupMemberInfo gid selfId False | gid <- fst <$> updateNeeded]
    return $ pure $ BAQueryAPI [ fmap (nextStepFor gid) . parser | gid <- fst <$> updateNeeded | parser <- parsers ]

withExpireTimeDo
  :: (MonadIO m)
  => NominalDiffTime        -- ^ expiration duration
  -> (UMaybeTime a -> m ()) -- ^ monadic lens to manage NothingYet -> Updating
  -> UMaybeTime a           -- ^ the UMaybeTime value to check
  -> m b                    -- ^ action to perform if expired or not present
  -> m b                    -- ^ action to perform if updating
  -> (a -> m b)             -- ^ action to perform if present and not expired
  -> m b
withExpireTimeDo expireDuration updateLens umaybe fetchAction waitAction action = do
  currentTime <- liftIO getCurrentTime
  case umaybe of
    NothingYet -> updateLens Updating >> fetchAction
    Updating   -> waitAction
    UpdatingOldValue (WithTime _ a) -> action a
    Updated (WithTime timeStamp a) ->
      if diffUTCTime currentTime timeStamp > expireDuration
        then updateLens Updating >> fetchAction
        else action a
{-# INLINE withExpireTimeDo #-}
