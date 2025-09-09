{-# LANGUAGE ParallelListComp #-}
module Command.Balance where

import Command
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Default
import Data.Maybe
import Data.PersistModel
import Data.Time.Clock
import MeowBot
import MeowBot.Data.Parser
import MeowBot.GetInfo
import MeowBot.CostModel.Types
import Utils.RunDB
import qualified Data.Text as T

type Amount = Double

data BalanceCommand
  = Own        (Maybe OwnerId) ChatId        (Maybe CostModel) -- ^ a owner of a wallet decides to own a (bot, chat) pair
  | OwnBot     (Maybe OwnerId) (Maybe BotId) (Maybe CostModel) -- ^ a owner of a wallet decides to own a bot
  -- | OwnBotChat OwnerId (Maybe CostModel)      BotId ChatId  -- ^ admin makes OwnerId own a (bot, chat) pair
  | AddOwnedBy Amount        OwnerId                           -- ^ admin adds amount to the wallet owned by OwnerId (can be negative)
  | AddTo      Amount        WalletId                          -- ^ admin adds amount to the wallet with WalletId (can be negative)
  | BalanceCheck (Maybe OwnerId)                               -- ^ check balance of the wallet owned by OwnerId, if None, try to find the ownerId by chatId
  | TotalBalance (Maybe BotId) -- for admin only
  deriving (Show, Eq)

data BalanceAction
  = AOwn    OwnerId (Maybe CostModel) BotId ChatId
  | AOwnBot OwnerId (Maybe CostModel) BotId
  | AAddOwnedBy UserId Amount OwnerId
  | AAddTo      UserId Amount WalletId
  | ABalanceCheck OwnerId
  | ATotalBalance (Maybe BotId)

newEmptyWallet :: UTCTime -> OwnerId -> Wallet
newEmptyWallet utcTime oid = Wallet oid 0 Nothing Nothing Nothing Nothing utcTime

type NewWalletCreated = Bool
withCreateWalletIfNotExists :: OwnerId -> (NewWalletCreated -> WalletId -> Meow a) -> Meow a
withCreateWalletIfNotExists oid action = do
  mWallet <- runDB $ getBy $ UniqueOwnerId oid
  utcTime <- liftIO getCurrentTime
  case mWallet of
    Just (Entity wid _) -> action False wid
    Nothing -> do
      wid <- runDB $ insert $ newEmptyWallet utcTime oid
      action True wid

-- | Determine which bots and (bot, chat) pairs are owned by the wallet
--
-- the convention is the same as bot_settings: chat-specific ownership overrides bot-wide ownership
walletOwns :: WalletId -> DB ([BotId], [(BotId, ChatId)])
walletOwns wid = do
  ownsBot     <- selectList [BotCostModelWalletId ==. Just wid] []
  ownsBotChat <- selectList [BotCostModelPerChatWalletId ==. Just wid] []
  return
    ( map (botCostModelBotId . entityVal) ownsBot
    , [ (botCostModelPerChatBotId, botCostModelPerChatChatId)
      | Entity _ BotCostModelPerChat { botCostModelPerChatBotId, botCostModelPerChatChatId } <- ownsBotChat
      ]
    )

-- | This function handles differently, global ownership first, overriding chat-specific ownership
-- walletOwnsGlobalFirst :: WalletId -> Meow ([BotId], [(BotId, ChatId)])
-- walletOwnsGlobalFirst wid = runDB $ do
--   ownsBot     <- selectList [BotCostModelWalletId ==. Just wid] []
--   ownsBotChat <- selectList [BotCostModelPerChatWalletId ==. Just wid] []
--   let chadIdsOfOwnedBotChat = botCostModelPerChatBotId . entityVal <$> ownsBotChat
--   ofWhichOwned <- map (botCostModelBotId . entityVal) <$> selectList
--     [ BotCostModelBotId    <-. chadIdsOfOwnedBotChat
--     , BotCostModelWalletId !=. Nothing
--     ]
--     []
--   return
--     ( map (botCostModelBotId . entityVal) ownsBot
--     , [ (botCostModelPerChatBotId, botCostModelPerChatChatId)
--       | Entity _ BotCostModelPerChat { botCostModelPerChatBotId, botCostModelPerChatChatId } <- ownsBotChat
--       , botCostModelPerChatBotId `notElem` ofWhichOwned
--       ] -- exclude those botChat pairs whose bot is owned
--     )

-- | For a specific (bot, chat) pair, find the associated cost model and wallet
-- first finds chat-specific ownership, then global ownership, otherwise Nothing
--
-- can be wrapped in runDB as atomic if needed
findWalletAssociatedToBotChat :: BotId -> ChatId -> DB (Maybe (CostModel, Maybe WalletId))
findWalletAssociatedToBotChat bid cid = do
  mBotChatRecord <- selectFirst [BotCostModelPerChatBotId ==. bid, BotCostModelPerChatChatId ==. cid] [Desc BotCostModelPerChatInserted]
  case mBotChatRecord of
    Just (Entity _ BotCostModelPerChat { botCostModelPerChatCostModel, botCostModelPerChatWalletId }) ->
      return $ Just (botCostModelPerChatCostModel, botCostModelPerChatWalletId)
    Nothing -> do
      mBotRecord <- selectFirst [BotCostModelBotId ==. bid] [Desc BotCostModelInserted]
      case mBotRecord of
        Just (Entity _ BotCostModel { botCostModelCostModel, botCostModelWalletId }) ->
          return $ Just (botCostModelCostModel, botCostModelWalletId)
        Nothing ->
          return Nothing

balanceAction
  :: Maybe String  -- ^ bot name for logging purpose, optional
  -> ChatId        -- ^ source chat id to send the result message
  -> BalanceAction -- ^ action to perform
  -> Meow (Maybe [BotAction])
balanceAction mBotName scid (AOwn oid mcm bid cid) = do
  utcTime <- liftIO getCurrentTime
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    runDB $ do
      mLastRecord <- selectFirst [BotCostModelPerChatBotId ==. bid, BotCostModelPerChatChatId ==. cid] [Desc BotCostModelPerChatInserted]
      case (mLastRecord, mcm) of
        (Just (Entity _ bcm), Just cm) -> insert_ bcm
          { botCostModelPerChatCostModel = cm
          , botCostModelPerChatWalletId  = Just wid
          , botCostModelPerChatInserted  = utcTime
          } -- specified new cost model
        (Just (Entity _ bcm), Nothing) -> insert_ bcm
          { botCostModelPerChatWalletId  = Just wid
          , botCostModelPerChatInserted  = utcTime
          } -- keep the old cost model
        (Nothing, Just cm) -> do -- insert new record with specified cost model
          insert_ $ BotCostModelPerChat mBotName bid cid cm (Just wid) utcTime
        (Nothing, Nothing) -> do -- insert new record with default cost model
          insert_ $ BotCostModelPerChat mBotName bid cid def (Just wid) utcTime
    let msg = T.unwords $
                [tshow oid, "with walletId", tshow wid, "now owns bot", tshow bid, "in chat", tshow cid]
                <> [ "(An new empty wallet is created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]

balanceAction mBotName scid (AOwnBot oid mcm bid) = do
  utcTime <- liftIO getCurrentTime
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    runDB $ do
      mLastRecord <- selectFirst [BotCostModelBotId ==. bid] [Desc BotCostModelInserted]
      case (mLastRecord, mcm) of
        (Just (Entity _ bcm), Just cm) -> insert_ bcm
          { botCostModelCostModel = cm
          , botCostModelWalletId  = Just wid
          , botCostModelInserted  = utcTime
          } -- specified new cost model
        (Just (Entity _ bcm), Nothing) -> insert_ bcm
          { botCostModelWalletId  = Just wid
          , botCostModelInserted  = utcTime
          } -- keep the old cost model
        (Nothing, Just cm) -> do -- insert new record with specified cost model
          insert_ $ BotCostModel mBotName bid cm (Just wid) utcTime
        (Nothing, Nothing) -> do -- insert new record with default cost model
          insert_ $ BotCostModel mBotName bid def (Just wid) utcTime
    let msg = T.unwords $
                [ tshow oid, "with walletId", tshow wid, "now owns bot", tshow bid]
                <> [ "(An new empty wallet is created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]

balanceAction _ scid (AAddOwnedBy uid amt oid) = do
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    utcTime <- liftIO getCurrentTime
    runDB $ do -- runDB is atomic
      insert $ Transaction wid amt utcTime (Just uid) Nothing
      updateWhere [WalletId ==. wid] [WalletBalance +=. amt]
    let msg = T.unwords $ [tshow amt, "is added to the wallet owned by", tshow oid, "with walletId", tshow wid]
                        <> [ "(An new empty wallet is created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]

balanceAction _ scid (AAddTo uid amt wid) = do
  mWallet <- runDB $ get wid
  case mWallet of
    Just Wallet { walletOwnerId } -> do
      utcTime <- liftIO getCurrentTime
      runDB $ do -- ^ runDB is atomic
        insert $ Transaction wid amt utcTime (Just uid) Nothing
        updateWhere [WalletId ==. wid] [WalletBalance +=. amt]
      let msg = T.unwords [tshow amt, "is added to the wallet owned by", tshow walletOwnerId, "with walletId", tshow wid]
      return $ Just [baSendToChatId scid msg]
    Nothing -> return $ Just [baSendToChatId scid $ "Wallet with id " <> tshow wid <> " does not exist."]

balanceAction _ scid (ABalanceCheck oid) = do
  mWallet <- runDB $ getBy $ UniqueOwnerId oid
  case mWallet of
    Just (Entity wid Wallet {walletBalance}) -> do
      owns <- runDB $ walletOwns wid
      let balanceMsg = T.unwords ["Wallet with id", tshow wid, "owned by", tshow oid, "has balance:", tshow walletBalance]
          ownsMsg = T.intercalate "\n" $ ["Associated with"]
                    <> [ "bot" <> tshow bid                            | bid        <- fst owns ]
                    <> [ "bot" <> tshow bid <> " in chat" <> tshow cid | (bid, cid) <- snd owns ]
      return $ Just [baSendToChatId scid $ T.intercalate "\n---\n" [balanceMsg, ownsMsg]]
    Nothing -> return $ Just [baSendToChatId scid $ "No wallet found for owner " <> tshow oid]

balanceAction _ scid _ = return $ Just [baSendToChatId scid "This feature is not implemented yet."]

balanceCommandToAction :: BotId -> (ChatId, UserId) -> BalanceCommand -> Maybe BalanceAction
balanceCommandToAction bid (oid, _) (Own Nothing cid Nothing)           = Just $ AOwn (OwnerId oid) Nothing bid cid
balanceCommandToAction _   (oid, _) (OwnBot Nothing (Just bid) Nothing) = Just $ AOwnBot (OwnerId oid) Nothing bid
balanceCommandToAction _   (_, _)   (Own Nothing _ (Just _))            = Nothing
balanceCommandToAction _   (_, _)   (OwnBot Nothing (Just _) (Just _))  = Nothing
balanceCommandToAction bid _        (Own (Just oid) cid mcm)            = Just $ AOwn oid mcm bid cid
balanceCommandToAction bid _        (OwnBot (Just oid) mbid mcm)        = Just $ AOwnBot oid mcm (fromMaybe bid mbid)
balanceCommandToAction bid (oid, _) (OwnBot Nothing Nothing mcm)        = Just $ AOwnBot (OwnerId oid) mcm bid
balanceCommandToAction _   (_, uid) (AddOwnedBy amt oid)                = Just $ AAddOwnedBy uid amt oid
balanceCommandToAction _   (_, uid) (AddTo amt wid)                     = Just $ AAddTo uid amt wid
balanceCommandToAction _   (oid, _) (BalanceCheck oid')                 = Just $ ABalanceCheck (fromMaybe (OwnerId oid) oid')
balanceCommandToAction _   _        (TotalBalance mbid)                 = Just $ ATotalBalance mbid

checkPrivilegeBalance :: IsAdmin -> (ChatId, UserId) -> BalanceCommand -> Bool
checkPrivilegeBalance True  _  _                                 = True
checkPrivilegeBalance False _ (Own Nothing _ Nothing)            = True
checkPrivilegeBalance False _ (OwnBot Nothing _ Nothing)         = True
checkPrivilegeBalance False (cid, uid) (BalanceCheck (Just oid)) = cid == coerce oid || PrivateChat uid == coerce oid
checkPrivilegeBalance _ _ _                                      = False

ownerIdP = fmap OwnerId chatIdP

costModelP :: Parser T.Text Char CostModel
costModelP = asum
  [ (asum . map string) ["unlimited", "Unlimited"]       >> return Unlimited
  , (asum . map string) ["subscription", "Subscription"] >> return Subscription
  , (asum . map string) ["payasyougo", "PayAsYouGo"]     >> return PayAsYouGo
  ]

balanceParser :: Parser T.Text Char BalanceCommand
balanceParser = headCommand "" >> asum
  [ string "own" >> spaces >> Own Nothing <$> chatIdP <*> pure Nothing
  , string "own" >> spaces >> string "bot" >> OwnBot Nothing <$> optMaybe (spaces >> botIdP) <*> pure Nothing
  , Own    <$> (Just <$> ownerIdP <* spaces <* string "own" <* spaces) <*> chatIdP <*> optMaybe (spaces >> string "using" >> spaces >> costModelP)
  , OwnBot <$> (Just <$> ownerIdP <* spaces <* string "own" <* spaces0 <* string "bot" <* spaces0) <*> optMaybe botIdP <*> optMaybe (spaces >> string "using" >> spaces >> costModelP)
  , string "add" >> spaces >>
    (   AddOwnedBy <$> (float <* spaces <* string "owned by" <* spaces) <*> fmap OwnerId chatIdP
    <|> AddTo      <$> (float <* spaces <* string "to" <* spaces) <*> walletIdP
    )
  , string "balance check" >> spaces >> BalanceCheck <$> optMaybe (fmap OwnerId chatIdP)
  ]
  <* many spaceOrEnter
  <* end

unitTestsBalanceParser :: [(Maybe BalanceCommand, Bool)]
unitTestsBalanceParser =
  let testCases =
        [ (":own group 12345", Just $ Own Nothing (GroupChat 12345) Nothing)
        , (":own bot", Just $ OwnBot Nothing Nothing Nothing)
        , (":own bot 67890", Just $ OwnBot Nothing (Just 67890) Nothing)
        , (":user 123 own group 456 using subscription", Just $ Own (Just $ OwnerId (PrivateChat 123)) (GroupChat 456) (Just Subscription))
        , (":user 123 own group 456", Just $ Own (Just $ OwnerId (PrivateChat 123)) (GroupChat 456) Nothing)
        , (":group 123 own bot 789 using unlimited", Just $ OwnBot (Just $ OwnerId (GroupChat 123)) (Just 789) (Just Unlimited))
        ]
      result = runParser balanceParser . fst <$> testCases
  in  [ (mParsed, mParsed == expected)
      | mParsed <- result
      | (_, expected) <- testCases
      ]

commandBalance :: BotCommand
commandBalance = BotCommand Balance $ botT $ do
  (msg, cid, uid, _mid, _sender) <- MaybeT $ getEssentialContent <$> query
  botid           <- query
  BotName botName <- query
  catSetCommand   <- MaybeT $ (`runParser` msg) <$> commandParserTransformByBotName balanceParser
  isadmin         <- lift $ isAdmin uid
  let privilege = checkPrivilegeBalance isadmin (cid, uid) catSetCommand
      mAction   = balanceCommandToAction botid (cid, uid)  catSetCommand
  case (mAction, privilege) of
    (Nothing, _)        -> return [baSendToChatId cid "Invalid command or parameters."]
    (Just action, True) -> MaybeT $ balanceAction botName cid action
    (Just _, False)     -> return [baSendToChatId cid "Operation not permitted."]

data ServiceBalanceCheck
  = NoWalletAssociated
  | WalletBalanceGood    (Maybe OverdueBehavior) Amount
  | WalletBalanceLow     (Maybe OverdueBehavior) Amount
  | WalletBalanceOverdue (Maybe OverdueBehavior) Amount
  | WalletUnlimited      (Maybe Amount)
  deriving (Show, Eq)

determineOverdue :: CostModel -> Maybe OverdueBehavior -> Amount -> ServiceBalanceCheck
determineOverdue cm ob amt
  | Unlimited <- cm = WalletUnlimited (Just amt)
  | amt >= 1        = WalletBalanceGood ob amt
  | amt < 0         = WalletBalanceOverdue ob amt
  | otherwise       = WalletBalanceLow ob amt

-- | For internal service to check balance, not for end users
serviceBalanceCheck :: BotId -> ChatId -> DB ServiceBalanceCheck
serviceBalanceCheck bid cid = do
  findWalletAssociatedToBotChat bid cid >>= \case
    Just (cm, Just wid) -> do
      mWalletRecord <- get wid
      case mWalletRecord of
        Just w@(walletOverdueBehavior -> Just ob) -> return $ determineOverdue cm (Just ob) w.walletBalance
        Just w                                    -> do
          defOb <- selectFirst [] []
          return $ determineOverdue cm ((walletOverdueBehavior . entityVal =<< defOb) <|> Just def) w.walletBalance
        Nothing -> return NoWalletAssociated -- impossible
    Just (Unlimited, Nothing) -> return $ WalletUnlimited Nothing
    _                         -> return NoWalletAssociated
