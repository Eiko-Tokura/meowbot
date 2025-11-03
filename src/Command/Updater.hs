-- | Updater is not a real command, but an executer for the update tasks.
module Command.Updater where

import Command
import Control.Lens
import Control.Monad.Effect
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Time.Clock
import Data.UpdateMaybe
import MeowBot
import Module.Logging
import Control.Monad.RS.Class
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import MeowBot.Data.CQHttp.Query (FriendBasicInfo(..))

anHour :: NominalDiffTime
anHour = 3600

aDay :: NominalDiffTime
aDay = 86400

commandUpdater :: BotCommand
commandUpdater = BotCommand Updater $ effAddLogCat' (LogCat @Text "OTHER:UPDATER") $ botT $ do
  botid :: BotId <- query
  s@SelfInfo {selfId, selfInGroups, selfFriends} <- MaybeT $ queries selfInfo
  $logDebug $ "Bot ID: " <> toText botid <> ", SelfInfo: " <> toText s
  let updateLensSelfInGroups u = modify $ _selfInfo ?~ s { selfInGroups = u }
      updateLensSelfFriends u = modify $ _selfInfo ?~ s { selfFriends = u }

      updateGroupListInMapWith :: (Hashable g) => [g] -> a -> HashMap g a -> HashMap g a
      updateGroupListInMapWith gids defVal mapG
        = Map.filterWithKey (\gid _ -> gid `elem` gids)
        $ Map.unionWith const mapG (Map.fromList [(gid, defVal) | gid <- gids])

      doNothing = MaybeT $ return Nothing

      fetchFriendList = do
        parseResponse <- lift $ queryAPI GetFriendList
        let nextStep :: QueryAPIResponse 'QueryFriendList -> Meow [BotAction]
            nextStep (GetFriendListResponse uinfos) = effAddLogCat' (LogCat @Text "FREIND") $ effAddLogCat' (LogCat @Text "OTHER:UPDATER") $ botT $ do
              let fset = Map.fromList [(uinfo.friendBasicInfoUserId, FriendInfo uinfo.friendBasicInfoNickname uinfo.friendBasicInfoRemark) | uinfo <- uinfos]
              $logInfo $ toText botid <> " Fetched friend list: " <> toText (Map.keys fset)
              utcTime <- liftIO getCurrentTime
              s <- MaybeT $ queries selfInfo
              let s' = s  & _selfFriends
                            .~ Updated (WithTime utcTime fset)
              modify $ _selfInfo ?~ s'
              return []
        return $ pure $ BARawQueryCallBack [fmap nextStep . parseResponse]

      fetchGroupList = do
        parseResponse <- lift $ queryAPI (GetGroupList False)
        let nextStep :: QueryAPIResponse 'QueryGroupList -> Meow [BotAction]
            nextStep (GetGroupListResponse ginfos) = effAddLogCat' (LogCat @Text "OTHER:UPDATER") $ botT $ do
              let gids = map groupBasicInfoGroupId ginfos
              $logInfo $ toText botid <> " Fetched group list: " <> toText gids
              utcTime <- liftIO getCurrentTime
              s <- MaybeT $ queries selfInfo
              let s' = s  & _selfInGroups
                            %~ updateUMaybeTime utcTime
                                (updateGroupListInMapWith gids NothingYet Map.empty)
                                (updateGroupListInMapWith gids NothingYet)
              modify $ _selfInfo ?~ s'
              return []
        return $ pure $ BARawQueryCallBack [fmap nextStep . parseResponse]

  l1 <- withExpireTimeDo aDay updateLensSelfFriends selfFriends fetchFriendList doNothing (const doNothing)

  fmap (l1 <>) <$> withExpireTimeDo anHour updateLensSelfInGroups selfInGroups fetchGroupList doNothing $ \groupMap -> do
    utcTime <- liftIO getCurrentTime

    let groupList = Map.toList groupMap
        updateNeeded = filter (needUpdate anHour utcTime . snd) groupList
        nextStepFor :: GroupId -> QueryAPIResponse 'QueryGroupMemberInfo -> Meow [BotAction]
        nextStepFor gid resp = effAddLogCat' (LogCat @Text "OTHER:UPDATER") $ botT $ do
          $logInfo $ toText botid <> " Fetched group member info for group " <> toText gid <> ": " <> toText resp
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
          modify $ _selfInfo ?~ s'
          return []

    -- | Mark all needed updates as Updating
    let !s_ = s { selfInGroups
                    = updateUMaybeTimeWithoutTime selfInGroups
                    $ \m -> foldr (Map.update (Just . toUpdating) . fst) m updateNeeded
                 }
    modify $ \od -> od { selfInfo = Just s_ }

    parsers <- lift $ queryAPI `mapM` [GetGroupMemberInfo gid selfId False | gid <- fst <$> updateNeeded]
    return $ pure $ BARawQueryCallBack [ fmap (nextStepFor gid) . parser | gid <- fst <$> updateNeeded | parser <- parsers ]

withExpireTimeDo
  :: (MonadIO m)
  => NominalDiffTime        -- ^ expiration duration
  -> (UMaybeTime a -> m ()) -- ^ monadic lens to manage NothingYet -> Updating
  -> UMaybeTime a           -- ^ the UMaybeTime value to check
  -> m b                    -- ^ action to perform if expired or not present
  -> m b                    -- ^ action to perform if updating
  -> (a -> m b)             -- ^ action to perform if present and not expired
  -> m b
withExpireTimeDo expireDuration updateLens umaybe fetchAction waitAction presentAction = do
  currentTime <- liftIO getCurrentTime
  case umaybe of
    NothingYet -> updateLens Updating >> fetchAction
    Updating   -> waitAction
    UpdatingOldValue (WithTime _ a) -> presentAction a
    Updated (WithTime timeStamp a) ->
      if diffUTCTime currentTime timeStamp > expireDuration
        then updateLens Updating >> fetchAction
        else presentAction a
{-# INLINE withExpireTimeDo #-}
