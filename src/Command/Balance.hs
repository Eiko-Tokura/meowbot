module Command.Balance where

import Command
import Command.Cat.CatSet (insertBotSettingPerChatIfNotExists)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Maybe
import Data.PersistModel
import Data.Time.Clock
import MeowBot
import MeowBot.Data.Parser
import MeowBot.GetInfo
import Utils.RunDB
import qualified Data.Text as T

type Amount = Double

data BalanceCommand
  = Own        (Maybe OwnerId) ChatId        -- ^ a owner of a wallet decides to own a (bot, chat) pair
  | OwnBot     (Maybe OwnerId) (Maybe BotId) -- ^ a owner of a wallet decides to own a bot
  | AddOwnedBy Amount        OwnerId         -- ^ admin adds amount to the wallet owned by OwnerId
  | AddTo      Amount        WalletId        -- ^ admin adds amount to the wallet with WalletId
  | BalanceCheck (Maybe OwnerId)             -- ^ check balance of the wallet owned by OwnerId, if None, try to find the ownerId by chatId
  | TotalBalance (Maybe BotId) -- for admin only

data BalanceAction
  = AOwn    OwnerId BotId ChatId
  | AOwnBot OwnerId BotId
  | AAddOwnedBy UserId Amount OwnerId
  | AAddTo      UserId Amount WalletId
  | ABalanceCheck OwnerId
  | ATotalBalance (Maybe BotId)

type NewWalletCreated = Bool
withCreateWalletIfNotExists :: OwnerId -> (NewWalletCreated -> WalletId -> Meow a) -> Meow a
withCreateWalletIfNotExists oid action = do
  mWallet <- runDB $ getBy $ UniqueOwnerId oid
  case mWallet of
    Just (Entity wid _) -> action False wid
    Nothing -> do
      wid <- runDB $ insert $ Wallet oid 0 Nothing
      action True wid

walletOwns :: WalletId -> Meow ([BotId], [(BotId, ChatId)])
walletOwns wid = do
  ownsBot <- runDB $ selectList [BotSettingBillingWalletId ==. Just wid] []
  ownsBotChat <- runDB $ selectList [BotSettingPerChatBillingWalletId ==. Just wid] []
  let botIds = map (botSettingBotId . entityVal) ownsBot
      botChatIds = map (\b -> (botSettingPerChatBotId (entityVal b), botSettingPerChatChatId (entityVal b))) ownsBotChat
  return (botIds, botChatIds)

balanceAction :: ChatId -> BalanceAction -> Meow (Maybe [BotAction])
balanceAction scid (AOwn oid bid cid) = do
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    runMaybeT $ insertBotSettingPerChatIfNotExists bid cid
    runDB $ updateWhere [BotSettingPerChatBotId ==. bid, BotSettingPerChatChatId ==. cid] [BotSettingPerChatBillingWalletId =. Just wid]
    let msg = T.unwords $
                [tshow oid, "with walletId", tshow wid, "now owns bot", tshow bid, "in chat", tshow cid]
                <> [ "(An new empty wallet is created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]
balanceAction scid (AOwnBot oid bid) = do
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    runDB $ updateWhere [BotSettingBotId ==. bid] [BotSettingBillingWalletId =. Just wid]
    let msg = T.unwords $
                [ tshow oid, "with walletId", tshow wid, "now owns bot", tshow bid]
                <> [ "(An new empty wallet is created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]
balanceAction scid (AAddOwnedBy uid amt oid) = do
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    utcTime <- liftIO getCurrentTime
    runDB $ do -- runDB is atomic
      insert $ Transaction wid amt utcTime (Just uid) Nothing
      updateWhere [WalletId ==. wid] [WalletBalance +=. amt]
    let msg = T.unwords $ [tshow amt, "is added to the wallet owned by", tshow oid, "with walletId", tshow wid]
                        <> [ "(An new empty wallet is created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]
balanceAction scid (AAddTo uid amt wid) = do
  mWallet <- runDB $ get wid
  case mWallet of
    Just (Wallet oid _ _) -> do
      utcTime <- liftIO getCurrentTime
      runDB $ do -- ^ runDB is atomic
        insert $ Transaction wid amt utcTime (Just uid) Nothing
        updateWhere [WalletId ==. wid] [WalletBalance +=. amt]
      let msg = T.unwords [tshow amt, "is added to the wallet owned by", tshow oid, "with walletId", tshow wid]
      return $ Just [baSendToChatId scid msg]
    Nothing -> return $ Just [baSendToChatId scid $ "Wallet with id " <> tshow wid <> " does not exist."]
balanceAction scid (ABalanceCheck oid) = do
  mWallet <- runDB $ getBy $ UniqueOwnerId oid
  case mWallet of
    Just (Entity wid (Wallet _ bal _)) -> do
      owns <- walletOwns wid
      let balanceMsg = T.unwords ["Wallet with id", tshow wid, "owned by", tshow oid, "has balance:", tshow bal]
          ownsMsg = T.intercalate "\n" $ ["Associated with"]
                    <> [ "bot" <> tshow bid                            | bid        <- fst owns ]
                    <> [ "bot" <> tshow bid <> " in chat" <> tshow cid | (bid, cid) <- snd owns ]
      return $ Just [baSendToChatId scid $ T.intercalate "\n---\n" [balanceMsg, ownsMsg]]
    Nothing -> return $ Just [baSendToChatId scid $ "No wallet found for owner " <> tshow oid]
balanceAction scid _ = return $ Just [baSendToChatId scid "This feature is not implemented yet."]

balanceCommandToAction :: BotId -> (ChatId, UserId) -> BalanceCommand -> BalanceAction
balanceCommandToAction bid (oid, _) (Own Nothing cid)         = AOwn (OwnerId oid) bid cid
balanceCommandToAction _ (oid, _) (OwnBot Nothing (Just bid)) = AOwnBot (OwnerId oid) bid
balanceCommandToAction bid _ (Own (Just oid) cid)             = AOwn oid bid cid
balanceCommandToAction bid _ (OwnBot (Just oid) mbid)         = AOwnBot oid (fromMaybe bid mbid)
balanceCommandToAction bid (oid, _) (OwnBot Nothing Nothing)  = AOwnBot (OwnerId oid) bid
balanceCommandToAction _ (_, uid) (AddOwnedBy amt oid)        = AAddOwnedBy uid amt oid
balanceCommandToAction _ (_, uid) (AddTo amt wid)             = AAddTo uid amt wid
balanceCommandToAction _ (oid, _) (BalanceCheck oid')         = ABalanceCheck (fromMaybe (OwnerId oid) oid')
balanceCommandToAction _ _ (TotalBalance mbid)                = ATotalBalance mbid

-- balance [ownerId]

---- | wallet owner can choose to own multiple bots or (bot, chat) pairs
---- [ownerId] own <chatId> -- this will give (bot, chat) pair wallet
---- [ownerId] own bot [botId] -- this will own all chats of the bot
---- 
---- (only admin can issue [ownerId] own ...)
--Wallet
--  ownerId     ChatId    Maybe
--  balance     Double
--  description Text      Maybe
--
---- | Transactions are issued by admin users
----
---- add <amount> owned by <chatId>
---- add <amount> to <walletId>
----
--Transaction
--  walletId    WalletId
--  amount      Double
--  time        UTCTime
--  handlerId   UserId    Maybe
--  description Text      Maybe

checkPrivilegeBalance :: IsAdmin -> (ChatId, UserId) -> BalanceCommand -> Bool
checkPrivilegeBalance True  _  _                                 = True
checkPrivilegeBalance False _ (Own Nothing _)                    = True
checkPrivilegeBalance False _ (OwnBot Nothing _)                 = True
checkPrivilegeBalance False (cid, uid) (BalanceCheck (Just oid)) = cid == coerce oid || PrivateChat uid == coerce oid
checkPrivilegeBalance _ _ _                                      = False

balanceParser :: Parser T.Text Char BalanceCommand
balanceParser = headCommand "" >> asum
  [ string "own" >> spaces >> Own Nothing <$> chatIdP
  , string "own" >> spaces >> string "bot" >> OwnBot Nothing <$> optMaybe (spaces >> botIdP)
  , string "add" >> spaces >>
    (   AddOwnedBy <$> (positiveFloat <* spaces <* string "owned by" <* spaces) <*> fmap OwnerId chatIdP
    <|> AddTo      <$> (positiveFloat <* spaces <* string "to" <* spaces) <*> walletIdP
    )
  , string "balance" >> spaces >> BalanceCheck <$> optMaybe (fmap OwnerId chatIdP)
  ]

commandBalance :: BotCommand
commandBalance = BotCommand Balance $ botT $ do
  (msg, cid, uid, _mid, _sender) <- MaybeT $ getEssentialContent <$> query
  botid         <- query
  catSetCommand <- MaybeT $ (`runParser` msg) <$> commandParserTransformByBotName balanceParser
  isadmin       <- lift $ isAdmin uid
  let privilege = checkPrivilegeBalance isadmin (cid, uid) catSetCommand
      action    = balanceCommandToAction botid (cid, uid) catSetCommand
  if privilege
    then MaybeT $ balanceAction cid action
    else return [baSendToChatId cid "Operation not permitted."]

