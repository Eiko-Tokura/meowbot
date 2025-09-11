module MeowBot.GetSelfInfo where

import MeowBot
import Data.UpdateMaybe
import qualified Data.Map as M

getSelfInfo :: Meow (Maybe SelfInfo)
getSelfInfo = queries selfInfo

getSelfRoleInGroup :: SelfInfo -> GroupId -> Maybe Role
getSelfRoleInGroup selfInfo gid = do
  groups    <- wToMaybe selfInfo.selfInGroups
  groupInfo <- M.lookup gid groups >>= wToMaybe
  return groupInfo.selfRole

isSelfAdminInGroup :: SelfInfo -> GroupId -> Maybe Bool
isSelfAdminInGroup gid = fmap (== RAdmin) <$> getSelfRoleInGroup gid
