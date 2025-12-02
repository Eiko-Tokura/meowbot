{-# LANGUAGE ParallelListComp, TransformListComp #-}
module Command.Balance where

import Command
import Data.PersistModel
import Data.Time.Clock
import GHC.Exts
import MeowBot
import MeowBot.BotStatistics (ChatStatistics(..), chatStatistics)
import MeowBot.Prelude
import MeowBot.CostModel
import MeowBot.Data.Parser
import MeowBot.GetInfo
import MeowBot.GetSelfInfo
import Utils.ListComp
import Utils.RunDB
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

data FlagAll = FlagAll deriving (Show, Eq)
data ItSelf = ItSelf deriving (Show, Eq)
data BalanceCommand
  = Own        (Maybe OwnerId) ChatId        (Maybe CostModel) -- ^ a owner of a wallet decides to own a (bot, chat) pair
  | OwnBot     (Maybe OwnerId) (Maybe BotId) (Maybe CostModel) -- ^ a owner of a wallet decides to own a bot
  | OwnBotChat OwnerId BotId   (Either ItSelf ChatId) (Maybe CostModel) -- ^ admin makes OwnerId own a (bot, chat) pair with optional cost model
  -- | UnOwn      BotId (Maybe ChatId)                   -- ^ admin removes ownership of a bot or (bot, chat) pair
  | AddOwnedBy Amount        OwnerId   (Maybe Text)      -- ^ admin adds amount to the wallet owned by OwnerId (can be negative)
  | AddTo      Amount        WalletId  (Maybe Text)      -- ^ admin adds amount to the wallet with WalletId (can be negative)
  | CostTo     Amount        WalletId  (Maybe Text)      -- ^ admin adds one-off cost to the wallet with WalletId, positive means cost incurred
  | CostOwnedBy Amount        OwnerId   (Maybe Text)     -- ^ admin adds one-off cost to the wallet owned by OwnerId, positive means cost incurred
  | BalanceCheck       (Maybe OwnerId) (Maybe FlagAll)   -- ^ check balance of the wallet owned by OwnerId, if None, try to find the ownerId by chatId
  | BalanceCheckInChat (Maybe ChatId)  (Maybe FlagAll)   -- ^ a different semantics, finds back the ownerId and check balance, for normal user
  | ChangeOwner  WalletId OwnerId               -- ^ admin modifys the owner of a wallet
  | TotalBalance (Maybe BotId) -- for admin only
  deriving (Show, Eq)

data BalanceAction
  = AOwn    OwnerId (Maybe CostModel) BotId ChatId
  | AOwnBot OwnerId (Maybe CostModel) BotId
  | AAddOwnedBy UserId Amount OwnerId  (Maybe Text)
  | AAddTo      UserId Amount WalletId (Maybe Text)
  | ACostTo     UserId Amount WalletId (Maybe Text)
  | ACostOwnedBy UserId Amount OwnerId (Maybe Text)
  | ABalanceCheck OwnerId (Maybe FlagAll)
  | ABalanceCheckInChat ChatId (Maybe FlagAll)
  | AChangeOwner WalletId OwnerId
  | ATotalBalance (Maybe BotId)

type NewWalletCreated = Bool
withCreateWalletIfNotExists :: OwnerId -> (NewWalletCreated -> WalletId -> Meow a) -> Meow a
withCreateWalletIfNotExists oid action = do
  mWallet <- runMeowCoreDB $ getBy $ UniqueOwnerId oid
  utcTime <- liftIO getCurrentTime
  case mWallet of
    Just (Entity wid _) -> action False wid
    Nothing -> do
      wid <- runMeowCoreDB $ insert $ newEmptyWallet utcTime oid
      action True wid

-- | Determine which bots and (bot, chat) pairs are owned by the wallet
--
-- the convention is the same as bot_settings: chat-specific ownership overrides bot-wide ownership
walletOwns :: WalletId -> DB ([(BotId, CostModel)], [(BotId, ChatId, CostModel)])
walletOwns wid = do
  ownsBot     <- selectList [BotCostModelWalletId        ==. wid] []
  ownsBotChat <- selectList [BotCostModelPerChatWalletId ==. wid] []
  return
    ( [ ( botCostModelBotId     $ head' botCM
        , botCostModelCostModel $ head' botCM
        )
      | botCM <- entityVal <$> ownsBot
      , then sortWith by Down $ botCostModelInserted botCM
      , then group by botCostModelBotId botCM using groupWith
        -- ^ stable sort to keep the latest inserted record first in each group
      ]
    ,
      [ ( botCostModelPerChatBotId      $ head' bcm
        , botCostModelPerChatChatId     $ head' bcm
        , botCostModelPerChatCostModel  $ head' bcm
        )
      | bcm <- entityVal <$> ownsBotChat
      , then sortWith by Down $ botCostModelPerChatInserted bcm
      , then group by (botCostModelPerChatBotId bcm, botCostModelPerChatChatId bcm) using groupWith
        -- ^ stable sort to keep the latest inserted record first in each group
      ]
    )

warningIfNotInGroup :: BotId -> ChatId -> Meow (Maybe Text)
warningIfNotInGroup _   (PrivateChat _) = return Nothing
warningIfNotInGroup bid (GroupChat gid) = runMaybeT $ do
  myBid <- query
  MaybeT $ pure $ guard (bid == myBid)
  selfInfo  <- MaybeT getSelfInfo
  isInGroup <- MaybeT $ return $ isSelfInGroup gid selfInfo
  MaybeT $ pure $ guard isInGroup
  return $ "Warning: I'm not in " <> toText gid <> "."

filterInactive
    :: Maybe FlagAll
       -> [(BotId, ChatId, CostModel)]
       -> DB  ( [(BotId, ChatId, CostModel)]
              , Maybe Int -- ^ number of inactive chats filtered out
              )
filterInactive (Just FlagAll) owns = pure (owns, Nothing)
filterInactive Nothing owns = do
  chatStatsList <- mapM (\(bid, cid, cm) -> (bid, cid, cm, ) <$> chatStatistics 7 bid cid) owns
  let isActiveChat ChatStatistics { recvMessagesPerDay, sentMessagesPerDay } =
        V.sum recvMessagesPerDay + V.sum sentMessagesPerDay > 0
      active =
        [ (bid, cid, cm)
        | (bid, cid, cm, stats) <- chatStatsList
        , isActiveChat stats
        ]
      inactiveCount = length owns - length active
  return (active, if inactiveCount > 0 then Just inactiveCount else Nothing)

balanceAction
  :: Maybe String  -- ^ bot name for logging purpose, optional
  -> ChatId        -- ^ source chat id to send the result message
  -> BalanceAction -- ^ action to perform
  -> Meow (Maybe [BotAction])
balanceAction mBotName scid (AOwn oid mcm bid cid) = do
  utcTime <- liftIO getCurrentTime
  mWarnCid <- warningIfNotInGroup bid cid
  mWarnOid <- warningIfNotInGroup bid (ownerChatId oid)
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    runMeowCoreDB $ do
      mLastRecord <- selectFirst [BotCostModelPerChatBotId ==. bid, BotCostModelPerChatChatId ==. cid] [Desc BotCostModelPerChatInserted]
      case (mLastRecord, mcm) of
        (Just (Entity _ bcm), Just cm) -> insert_ bcm
          { botCostModelPerChatCostModel = cm
          , botCostModelPerChatWalletId  = wid
          , botCostModelPerChatInserted  = utcTime
          } -- specified new cost model
        (Just (Entity _ bcm), Nothing) -> insert_ bcm
          { botCostModelPerChatWalletId  = wid
          , botCostModelPerChatInserted  = utcTime
          } -- keep the old cost model
        (Nothing, Just cm) -> do -- insert new record with specified cost model
          insert_ $ BotCostModelPerChat mBotName bid cid cm wid utcTime
        (Nothing, Nothing) -> do -- insert new record with default cost model
          insert_ $ BotCostModelPerChat mBotName bid cid def wid utcTime
    let msg = T.unwords $
                [toText oid, "with walletId", toText wid, "now owns bot", toText bid, "in", toText cid]
                <> [ "(empty wallet created)" | newWallet ]
                <> maybe [] (\w -> ["\n" <> w]) mWarnCid
                <> maybe [] (\w -> ["\n" <> w]) mWarnOid
    return $ Just [baSendToChatId scid msg]

balanceAction mBotName scid (AOwnBot oid mcm bid) = do
  utcTime <- liftIO getCurrentTime
  mWarnOid <- warningIfNotInGroup bid (ownerChatId oid)
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    runMeowCoreDB $ do
      mLastRecord <- selectFirst [BotCostModelBotId ==. bid] [Desc BotCostModelInserted]
      case (mLastRecord, mcm) of
        (Just (Entity _ bcm), Just cm) -> insert_ bcm
          { botCostModelCostModel = cm
          , botCostModelWalletId  = wid
          , botCostModelInserted  = utcTime
          } -- specified new cost model
        (Just (Entity _ bcm), Nothing) -> insert_ bcm
          { botCostModelWalletId  = wid
          , botCostModelInserted  = utcTime
          } -- keep the old cost model
        (Nothing, Just cm) -> do -- insert new record with specified cost model
          insert_ $ BotCostModel mBotName bid cm wid utcTime
        (Nothing, Nothing) -> do -- insert new record with default cost model
          insert_ $ BotCostModel mBotName bid def wid utcTime
    let msg = T.unwords $
                [ toText oid, "with walletId", toText wid, "now owns bot", toText bid]
                <> [ "(empty wallet created)" | newWallet ]
                <> maybe [] (\w -> ["\n" <> w]) mWarnOid
    return $ Just [baSendToChatId scid msg]

balanceAction _ scid (AAddOwnedBy uid amt oid mDesc) = do
  withCreateWalletIfNotExists oid $ \newWallet wid -> do
    utcTime <- liftIO getCurrentTime
    runMeowCoreDB $ do -- runMeowCoreDB is atomic
      insert $ Transaction wid amt utcTime (Just uid) mDesc
      update wid [WalletBalance +=. amt, WalletOverdueNotified =. Nothing]
    let msg = T.unwords $ [toText amt, "is added to the wallet owned by", toText oid, "with walletId", toText wid]
                        <> [ "(empty wallet created)" | newWallet ]
    return $ Just [baSendToChatId scid msg]

balanceAction _ scid (AAddTo uid amt wid mDesc) = do
  mWallet <- runMeowCoreDB $ get wid
  case mWallet of
    Just Wallet { walletOwnerId } -> do
      utcTime <- liftIO getCurrentTime
      runMeowCoreDB $ do -- ^ runMeowCoreDB is atomic
        insert $ Transaction wid amt utcTime (Just uid) mDesc
        update wid [WalletBalance +=. amt, WalletOverdueNotified =. Nothing]
      let msg = T.unwords [toText amt, "is added to the wallet owned by", toText walletOwnerId, "with walletId", toText wid]
      return $ Just [baSendToChatId scid msg]
    Nothing -> return $ Just [baSendToChatId scid $ "Wallet with id " <> toText wid <> " does not exist."]

balanceAction _ scid (ACostTo uid amt wid mDesc) = do
  mWallet <- runMeowCoreDB $ get wid
  case mWallet of
    Just Wallet { walletOwnerId } -> do
      utcTime <- liftIO getCurrentTime
      runMeowCoreDB $ do -- ^ runMeowCoreDB is atomic
        insert $ OneOffCostRecord wid amt Nothing utcTime (Just uid) mDesc
        update wid [WalletBalance -=. amt, WalletOverdueNotified =. Nothing]
      let msg = T.unwords [toText amt, " one-off cost is deducted from the wallet owned by", toText walletOwnerId, "with walletId", toText wid]
      return $ Just [baSendToChatId scid msg]
    Nothing -> return $ Just [baSendToChatId scid $ "Wallet with id " <> toText wid <> " does not exist."]

balanceAction bn scid (ACostOwnedBy uid amt oid mDesc) = do
  mWallet <- runMeowCoreDB $ getBy $ UniqueOwnerId oid
  case mWallet of
    Just (Entity wid Wallet {}) -> do
      balanceAction bn scid (ACostTo uid amt wid mDesc)
    Nothing -> return $ Just [baSendToChatId scid $ "No wallet found for owner " <> toText oid]

balanceAction _ scid (ABalanceCheck oid flagAll) = do
  mWallet <- runMeowCoreDB $ getBy $ UniqueOwnerId oid
  case mWallet of
    Just (Entity wid Wallet {walletBalance}) -> do
      owns'  <- runMeowCoreDB $ walletOwns wid
      (ownBot, (ownChats, filtered)) <- runMeowCoreDB $ (fst owns',) <$> filterInactive flagAll (snd owns')
      let owns = (ownBot, ownChats)
      let balanceMsg = T.unwords ["Wallet with id", toText wid, "owned by", toText oid, "has balance:", toText walletBalance]
          ownsMsg = T.intercalate "\n" $ ["Associated with"]
                    <>  [ "bot" <> toText bid <> " " <> toText cm
                        | (bid, cm)      <- fst owns ]
                    <>  [ "bot" <> toText bid <> " in " <> toText cid <> " " <> toText cm
                        | (bid, cid, cm) <- snd owns ]
                    <>  maybe [] (\n -> ["(" <> toText n <> " inactive chats omitted, use --all to see)"]) filtered
      return $ Just [baSendToChatId scid $ T.intercalate "\n" [balanceMsg, ownsMsg]]
    Nothing -> return $ Just [baSendToChatId scid $ "No wallet found for owner " <> toText oid]

balanceAction _ scid (ABalanceCheckInChat cid flagAll) = do
  botid <- query
  mWarnCid <- warningIfNotInGroup botid cid
  mCmWid <- runMeowCoreDB $ findWalletAssociatedToBotChat botid cid
  case mCmWid of
    Just (_, wid) -> do
      mWallet <- runMeowCoreDB $ get wid
      case mWallet of
        Just Wallet { walletOwnerId, walletBalance } -> do
          owns' <- runMeowCoreDB $ walletOwns wid
          (ownBot, (ownChats, filtered)) <- runMeowCoreDB $ (fst owns',) <$> filterInactive flagAll (snd owns')
          let owns = (ownBot, ownChats)
          let balanceMsg = T.unwords ["Wallet with id", toText wid, "owned by", toText walletOwnerId, "has balance:", toText walletBalance]
              ownsMsg = T.intercalate "\n" $ ["Associated with"]
                        <>  [ "bot" <> toText bid <> " " <> toText cm
                            | (bid, cm)      <- fst owns ]
                        <>  [ "bot" <> toText bid <> " in " <> toText cid' <> " " <> toText cm
                            | (bid, cid', cm) <- snd owns ]
                        <>  maybe [] (\n -> ["(" <> toText n <> " inactive chats omitted, use --all to see)"]) filtered
          return $ Just [baSendToChatId scid $ T.intercalate "\n" $ [balanceMsg, ownsMsg] <> maybe [] pure mWarnCid]
        Nothing -> return $ Just [baSendToChatId scid $ "Something wrong: " <> toText cid <> " links to wallet with id " <> toText wid <> ", but it is not found."]
    Nothing -> return $ Just [baSendToChatId scid $ "No cost/wallet is associated with " <> toText cid <> "."]

balanceAction _ scid (AChangeOwner wid oid) = do
  mWallet <- runMeowCoreDB $ get wid
  case mWallet of
    Just _ -> do
      runMeowCoreDB $ update wid [WalletOwnerId =. oid, WalletOverdueNotified =. Nothing]
      return $ Just [baSendToChatId scid $ T.unwords ["Wallet with id", toText wid, "is now owned by", toText oid]]
    Nothing -> return $ Just [baSendToChatId scid $ "Wallet with id " <> toText wid <> " does not exist."]

balanceAction _ scid (ATotalBalance _) = return $ Just [baSendToChatId scid "This feature is not implemented yet."]

balanceCommandToAction :: BotId -> (ChatId, UserId) -> BalanceCommand -> Maybe BalanceAction
balanceCommandToAction _   _        (OwnBotChat oid bid (Right cid) mcm)   = Just $ AOwn oid mcm bid cid
balanceCommandToAction _   _        (OwnBotChat oid bid (Left ItSelf) mcm) = Just $ AOwn oid mcm bid (ownerChatId oid)
balanceCommandToAction bid (oid, _) (Own Nothing cid Nothing)              = Just $ AOwn (OwnerId oid) Nothing bid cid
balanceCommandToAction _   (oid, _) (OwnBot Nothing (Just bid) Nothing)    = Just $ AOwnBot (OwnerId oid) Nothing bid
balanceCommandToAction _   (_, _)   (Own Nothing _ (Just _))               = Nothing
balanceCommandToAction _   (_, _)   (OwnBot Nothing (Just _) (Just _))     = Nothing
balanceCommandToAction bid _        (Own (Just oid) cid mcm)               = Just $ AOwn oid mcm bid cid
balanceCommandToAction bid _        (OwnBot (Just oid) mbid mcm)           = Just $ AOwnBot oid mcm (fromMaybe bid mbid)
balanceCommandToAction bid (oid, _) (OwnBot Nothing Nothing mcm)           = Just $ AOwnBot (OwnerId oid) mcm bid
balanceCommandToAction _   (_, uid) (AddOwnedBy amt oid mdesc)             = Just $ AAddOwnedBy uid amt oid mdesc
balanceCommandToAction _   (_, uid) (AddTo amt wid mdesc)                  = Just $ AAddTo uid amt wid mdesc
balanceCommandToAction _   (_, uid) (CostTo amt wid mdesc)                 = Just $ ACostTo uid amt wid mdesc
balanceCommandToAction _   (_, uid) (CostOwnedBy amt oid mdesc)            = Just $ ACostOwnedBy uid amt oid mdesc
balanceCommandToAction _   (oid, _) (BalanceCheck oid' fl)                 = Just $ ABalanceCheck (fromMaybe (OwnerId oid) oid') fl
balanceCommandToAction _   (cid, _) (BalanceCheckInChat Nothing fl)        = Just $ ABalanceCheckInChat cid fl
balanceCommandToAction _   (_, _)   (BalanceCheckInChat (Just cid) fl)     = Just $ ABalanceCheckInChat cid fl
balanceCommandToAction _   _        (ChangeOwner wid oid)                  = Just $ AChangeOwner wid oid
balanceCommandToAction _   _        (TotalBalance mbid)                    = Just $ ATotalBalance mbid

checkPrivilegeBalance :: IsSuperUser -> (ChatId, UserId) -> BalanceCommand -> Bool
checkPrivilegeBalance (IsSuperUser True ) _  _                                        = True
checkPrivilegeBalance (IsSuperUser False) _ (Own Nothing _ Nothing)                   = True
checkPrivilegeBalance (IsSuperUser False) _ (OwnBot Nothing _ Nothing)                = True
checkPrivilegeBalance (IsSuperUser False) _ (BalanceCheck Nothing _)                  = True
checkPrivilegeBalance (IsSuperUser False) (_, _) (BalanceCheckInChat Nothing _)       = True
checkPrivilegeBalance (IsSuperUser False) (cid, uid) (BalanceCheckInChat (Just cid') _) = cid == cid'       || PrivateChat uid == cid'
checkPrivilegeBalance (IsSuperUser False) (cid, uid) (BalanceCheck (Just oid) _)        = cid == coerce oid || PrivateChat uid == coerce oid
checkPrivilegeBalance _ _ _                                                           = False

ownerIdP = fmap OwnerId chatIdP
selfP = ItSelf <$ (asum . map string) ["self", "itself", "ItSelf"]

feeRateP :: Parser T.Text Char PayAsYouGoFeeRate
feeRateP = PayAsYouGoFeeRate <$> (string "rate=" >> float)

dailyCostP :: Parser T.Text Char OtherCost
dailyCostP = asum
  [ DailyCostCapped <$> (string "daily=" >> DailyBasicCost <$> nFloat) <*> (spaces >> string "cap=" >> positiveInt)
  , DailyCost . DailyBasicCost <$> (string "daily=" >> nFloat)
  ]

costModelP :: Parser T.Text Char CostModel
costModelP = asum
  [ (asum . map string) ["unlimited", "Unlimited"]       >> return  Unlimited 

  , (asum . map string) ["subscription", "Subscription"] >> return ( Subscription (monthlySubscriptionFee def) )

  , (asum . map string) ["payasyougo", "PayAsYouGo"]     >> PayAsYouGo <$> fmap (fromMaybe def) (optMaybe (spaces >> feeRateP)) <*> optMaybe (spaces >> dailyCostP)
  , (asum . map string) ["payasyougo", "PayAsYouGo"]     >> return ( PayAsYouGo def Nothing )

  , (asum . map string) ["chino", "Chino"]               >> return  chinoCostModel

  , (asum . map string) ["costonly", "CostOnly"]         >> CostOnly . Just <$> (spaces >> dailyCostP)
  , (asum . map string) ["costonly", "CostOnly"]         >> return ( CostOnly Nothing )
  ]

balanceParser :: Parser T.Text Char (NonEmpty BalanceCommand)
balanceParser = innerParserToBatchParser innerBalanceParser
  where
    innerBalanceParser = asum
      [ string "own" >> spaces >> Own Nothing <$> chatIdP <*> pure Nothing
      , string "own" >> spaces >> string "bot" >> OwnBot Nothing <$> optMaybe (spaces >> botIdP) <*> pure Nothing
      , Own    <$> (Just <$> ownerIdP <* spaces <* string "own" <* spaces) <*> chatIdP <*> optMaybe (spaces >> string "using" >> spaces >> costModelP)
      , OwnBot <$> (Just <$> ownerIdP <* spaces <* string "own" <* spaces0 <* string "bot" <* spaces0) <*> optMaybe botIdP <*> optMaybe (spaces >> string "using" >> spaces >> costModelP)
      , OwnBotChat <$> ownerIdP <* spaces <* string "own" <* spaces0 <* string "bot" <* spaces0 <*> botIdP <*> (spaces >> string "in" >> spaces >> (Right <$> chatIdP <|> Left <$> selfP)) <*> optMaybe (spaces >> string "using" >> spaces >> costModelP)
      , string "add" >> spaces >>
        (   AddOwnedBy <$> (Amount <$> float <* spaces <* string "owned by" <* spaces) <*> fmap OwnerId chatIdP <*> optMaybe (spaces >> just '"' >> manyTill' (just '"') getItem <* just '"')
        <|> AddTo      <$> (Amount <$> float <* spaces <* string "to" <* spaces) <*> walletIdP <*> optMaybe (spaces >> just '"' >> manyTill' (just '"') getItem <* just '"')
        )
      , string "cost" >> spaces >>
        (   CostOwnedBy <$> (Amount <$> float <* spaces <* string "owned by" <* spaces) <*> fmap OwnerId chatIdP <*> optMaybe (spaces >> just '"' >> manyTill' (just '"') getItem <* just '"')
        <|> CostTo      <$> (Amount <$> float <* spaces <* string "to" <* spaces) <*> walletIdP <*> optMaybe (spaces >> just '"' >> manyTill' (just '"') getItem <* just '"')
        )
      , string "balance check" >> BalanceCheck <$> optMaybe (spaces >> ownerIdP) <*> optMaybe (spaces >> string "--all" >> return FlagAll)
      , string "balance check in chat" >> BalanceCheckInChat <$> optMaybe (spaces >> chatIdP) <*> optMaybe (spaces >> string "--all" >> return FlagAll)
      , string "modify owner of wallet" >> spaces >> ChangeOwner <$> walletIdP <*> (spaces >> string "to" >> spaces >> ownerIdP)
      ]

unitTestsBalanceParser :: [(Maybe (NonEmpty BalanceCommand), Bool)]
unitTestsBalanceParser =
  let testCases =
        [ (":own group 12345", Just $ pure $ Own Nothing (GroupChat 12345) Nothing)
        , (":own bot", Just $ pure $ OwnBot Nothing Nothing Nothing)
        , (":own bot 67890", Just $ pure $ OwnBot Nothing (Just 67890) Nothing)
        , (":user 123 own group 456 using subscription", Just $ pure $ Own (Just $ OwnerId (PrivateChat 123)) (GroupChat 456) (Just $ Subscription def))
        , (":user 123 own group 456", Just $ pure $ Own (Just $ OwnerId (PrivateChat 123)) (GroupChat 456) Nothing)
        , (":group 123 own bot 789 using unlimited", Just $ pure $ OwnBot (Just $ OwnerId (GroupChat 123)) (Just 789) (Just Unlimited))
        , (":group 123 own bot 1 in group 123 using payasyougo", Just $ pure $ OwnBotChat (OwnerId (GroupChat 123)) 1 (Right $ GroupChat 123) (Just $ PayAsYouGo def def))
        , (":group 123 own bot 1 in itself using payasyougo", Just $ pure $ OwnBotChat (OwnerId (GroupChat 123)) 1 (Left ItSelf) (Just $ PayAsYouGo def def))

        , (":add 0 owned by user 123", Just $ pure $ AddOwnedBy (Amount 0) (OwnerId (PrivateChat 123)) Nothing)
        , (":add 0 owned by group 12345 \"for testing\"", Just $ pure $ AddOwnedBy (Amount 0) (OwnerId (GroupChat 12345)) (Just "for testing"))
        , (":{own bot; user 123 own group 12345 using subscription;}"
          , Just $ OwnBot Nothing Nothing Nothing :| [Own (Just $ OwnerId (PrivateChat 123)) (GroupChat 12345) (Just $ Subscription def)]
          )
        , (":{own bot; own bot; own bot; own bot; own bot; own bot; own bot; own bot; own bot; own bot;}"
          , Just $ NE.fromList $ replicate 10 (OwnBot Nothing Nothing Nothing)
          )
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
  parsedCommands  <- MaybeT $ (`runParser` msg) <$> commandParserTransformByBotName balanceParser
  isSuper         <- lift $ isSuperUser uid
  let privilege = all (checkPrivilegeBalance isSuper (cid, uid)) parsedCommands
      mActions  = balanceCommandToAction botid (cid, uid) `mapM` parsedCommands
  case (mActions, privilege) of
    (Nothing, _)                -> return [baSendToChatId cid "Invalid command or parameters."]
    (Just (action :| []), True) -> MaybeT $ balanceAction botName cid action
    (Just actions, True)        -> MaybeT $ concatOutput (Just "\n---\n") $ balanceAction botName cid `mapM` actions
    (Just _, False)             -> return [baSendToChatId cid "Operation not permitted."]
