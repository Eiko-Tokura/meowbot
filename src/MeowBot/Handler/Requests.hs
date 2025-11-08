module MeowBot.Handler.Requests where

import MeowBot.CostModel
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.PersistModel
import MeowBot.BotStructure
import System.Meow
import Utils.RunDB

shouldApproveGroupInvite :: BotRequestSetting -> GroupId -> DB Bool
shouldApproveGroupInvite reqSet gid = do
  hasCost <- (/= NoCostModelAssigned) <$> serviceBalanceCheck reqSet.botRequestSettingBotId (GroupChat gid)
  return  $  fromMaybe True  reqSet.botRequestSettingApproveGroupRequest  -- by default, approve
          || fromMaybe False reqSet.botRequestSettingApproveGroupWithCostModel && hasCost

shouldApproveFriendRequest :: BotRequestSetting -> UserId -> DB Bool
shouldApproveFriendRequest reqSet uid = do
  hasWallet <- isJust <$> selectFirst [ WalletOwnerId ==. OwnerId (PrivateChat uid) ] []
  return  $  fromMaybe True  reqSet.botRequestSettingApproveFriendRequest -- by default, approve
          || fromMaybe False reqSet.botRequestSettingApproveFriendWithWallet && hasWallet

botHandleRequestEvent :: CQMessage -> String -> Meow [BotAction]
botHandleRequestEvent cqmsg str = do
  $(logInfo) $ pack str <> " <- RequestEvent: " <> toText cqmsg
  botId      <- query
  case requestType cqmsg of
    Just (RequestFriend mcomment (Just flag)) -> fromMaybe (pure []) <=< runMaybeT $ do
      mBotReqSetting <- lift $ runMeowDB $ getBy $ UniqueBotRequestSettingBotId botId
      let mUid = cqmsg.userId

      case (mBotReqSetting, mUid) of
        (Just (Entity _ botReqSetting), Just uid) -> do
          approve <- lift $ runMeowDB $ shouldApproveFriendRequest botReqSetting uid
          guard approve
        _ -> return ()

      uid <- MaybeT $ return $ userId cqmsg
      $(logInfo) $ toText str <> " <- RequestFriend from " <> toText uid <> ", comment: " <> toText mcomment
      $(logInfo) $ " -> Approving the request."
      return . pure . pure $ BAActionAPI $ SetFriendAddRequest uid flag ""
    Just (RequestGroup RequestGroupInvite mcomment (Just flag)) -> fromMaybe (pure []) <=< runMaybeT $ do
      mBotReqSetting <- lift $ runMeowDB $ getBy $ UniqueBotRequestSettingBotId botId

      let mGid = cqmsg.groupId
      case (mBotReqSetting, mGid) of
        (Just (Entity _ botReqSetting), Just gid) -> do
          approve <- lift $ runMeowDB $ shouldApproveGroupInvite botReqSetting gid
          guard approve
        _ -> return ()

      gid <- MaybeT $ return $ groupId cqmsg
      uid <- MaybeT $ return $ userId cqmsg
      $(logInfo) $ toText str <> " <- RequestGroupInvite from " <> toText uid <> " in " <> toText gid <> ", comment: " <> toText mcomment
      $(logInfo) $ " -> Approving the request."
      return . pure . pure $ BAActionAPI $ SetGroupAddRequest flag RequestGroupInvite True Nothing
    _ -> return []
