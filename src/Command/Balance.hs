module Command.Balance where

import Command
import Command.Cat.CatSet (insertBotSettingPerChatIfNotExists)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Coerce
import Data.PersistModel
import MeowBot
import MeowBot.GetInfo
import MeowBot.Data.Parser
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

balanceAction :: ChatId -> BalanceAction -> Meow (Maybe [BotAction])
balanceAction scid (AOwn oid bid cid) = do
  mWallet <- runDB $ getBy $ UniqueOwnerId oid
  case mWallet of
    Nothing -> do
      _ <- runDB $ insert $ Wallet oid 0 Nothing
      return $ Just [baSendToChatId scid ("Empty wallet created for " <> tshow oid)]
    Just (Entity wid _) -> do
      insertBotSettingPerChatIfNotExists bid cid
balanceAction _ _ = undefined

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
  if privilege then MaybeT $ balanceAction action else MaybeT $ pure Nothing

