module MeowBot.CronTab.PeriodicCost where

import Command
import Cron.Match
import Cron.Schedule
import Control.Monad.Logger
import Data.Coerce
import Data.PersistModel
import MeowBot
import MeowBot.CostModel
import MeowBot.CronTab
import MeowBot.Prelude
import Utils.RunDB

periodicCostHandleCronTabTick :: CronTabTick -> Meow [BotAction]
periodicCostHandleCronTabTick (CronTabTick now) = botT $ do
  MaybeT $ pure $ guard $ timeMatchesCron now hourlySchedule
  botId :: BotId <- query
  lift $ checkAndUpdatePeriodicCost botId now
  return []

checkAndUpdatePeriodicCost :: BotId -> UTCTime -> Meow ()
checkAndUpdatePeriodicCost botId now = do
  mBotCostModel        <- runDB $ getBotPeriodicCostModel        botId
  lBotCostModelPerChat <- runDB $ getBotPerChatPeriodicCostModel botId
  let listCosts = [ (p, Left  bcm)  | Just (p, bcm) <- [mBotCostModel] ] ++
                  [ (p, Right bcmc) | (p, bcmc) <- lBotCostModelPerChat ]
  _inserted <- forM listCosts $ \(p, bcm) -> do
    let bcm'  = either (Left . entityKey) (Right . entityKey) bcm
    let bcm'' = either (Left . entityVal) (Right . entityVal) bcm
    mLastRecord <- runDB $ findLastCostRecord bcm'
    receivedMessageToday <- runDB $ receivedMessageToday bcm'' (utctDay now)
    let needAction =  receivedMessageToday > 0
                   && periodicLogic Daily now (fmap periodicCostRecordTime mLastRecord)
    when needAction $ do
      res <- runDB $ insertNewCostRecord p bcm now
      case res of
        Just () -> $logInfo  $ "Inserted daily basic cost record for " <> toText bcm
        Nothing -> $logError $ "Failed to insert daily basic cost record for " <> toText bcm
  return ()

insertNewCostRecord :: DailyBasicCost -> Either (Entity BotCostModel) (Entity BotCostModelPerChat) -> UTCTime -> DB (Maybe ())
insertNewCostRecord dailyCost (Left enBCM) now = runMaybeT $ do
  let costRecord = PeriodicCostRecord
        { periodicCostRecordTime                      = now
        , periodicCostRecordReferToCostModelId        = Just enBCM.entityKey
        , periodicCostRecordReferToCostModelPerChatId = Nothing
        , periodicCostRecordWalletId                  = enBCM.entityVal.botCostModelWalletId
        , periodicCostRecordDescription               = Just "Daily basic cost"
        , periodicCostRecordCost                      = coerce dailyCost
        }
  MaybeT $ pure $ guard $ hasDailyCost enBCM.entityVal.botCostModelCostModel
  lift $ do
    update enBCM.entityVal.botCostModelWalletId [WalletBalance -=. coerce dailyCost]
    insert_ costRecord
insertNewCostRecord dailyCost (Right enBCMP) now = runMaybeT $ do
  let costRecord = PeriodicCostRecord
        { periodicCostRecordTime                      = now
        , periodicCostRecordReferToCostModelId        = Nothing
        , periodicCostRecordReferToCostModelPerChatId = Just enBCMP.entityKey
        , periodicCostRecordWalletId                  = enBCMP.entityVal.botCostModelPerChatWalletId
        , periodicCostRecordDescription               = Just "Daily basic cost"
        , periodicCostRecordCost                      = coerce dailyCost
        }
  MaybeT $ pure $ guard $ hasDailyCost enBCMP.entityVal.botCostModelPerChatCostModel
  lift $ do
    update enBCMP.entityVal.botCostModelPerChatWalletId [WalletBalance -=. coerce dailyCost]
    insert_ costRecord

findLastCostRecord :: Either BotCostModelId BotCostModelPerChatId -> DB (Maybe PeriodicCostRecord)
findLastCostRecord (Left botCostModelId)         = fmap entityVal <$>
  selectFirst [PeriodicCostRecordReferToCostModelId ==. Just botCostModelId] [Desc PeriodicCostRecordTime]
findLastCostRecord (Right botCostModelPerChatId) = fmap entityVal <$>
  selectFirst [PeriodicCostRecordReferToCostModelPerChatId ==. Just botCostModelPerChatId] [Desc PeriodicCostRecordTime]

getBotPeriodicCostModel :: BotId -> DB (Maybe (DailyBasicCost, Entity BotCostModel))
getBotPeriodicCostModel botId = runMaybeT $ do
  enCM      <- MaybeT $ getBotCostModel botId
  basicCost <- MaybeT $ pure $ getDailyCost enCM.entityVal.botCostModelCostModel
  return ( basicCost, enCM )

getBotPerChatPeriodicCostModel :: BotId -> DB [(DailyBasicCost, Entity BotCostModelPerChat)]
getBotPerChatPeriodicCostModel botId = do
  list <- getBotPerChatCostModel botId
  return [ (p, en)
         | en@(Entity _ modelPerChat) <- list
         , let mP = getDailyCost modelPerChat.botCostModelPerChatCostModel
         , Just p <- [mP]
         ]

data Period = Daily

type LastCost = UTCTime
type NeedAction = Bool

periodicLogic :: Period -> UTCTime -> Maybe LastCost -> NeedAction
periodicLogic Daily _ Nothing = True
periodicLogic Daily now (Just lastTime) = utctDay lastTime /= utctDay now

receivedMessageToday :: Either BotCostModel BotCostModelPerChat -> Day -> DB Int
receivedMessageToday (Right bcmpc) day = do
  mBotStat <- getBy $ UniqueBotStatisticsPerApiKeyPerChatPerDay
    "STATISTICS"
    bcmpc.botCostModelPerChatBotId
    bcmpc.botCostModelPerChatChatId day
  return $ maybe 0 (botStatisticsPerApiKeyPerChatPerDayTotalMessageRecv . entityVal) mBotStat
receivedMessageToday (Left bcm) day = do
  botStats <- selectList
    [ BotStatisticsPerApiKeyPerChatPerDayBotId ==. bcm.botCostModelBotId
    , BotStatisticsPerApiKeyPerChatPerDayDay   ==. day
    ] []
  return $ sum $ map (botStatisticsPerApiKeyPerChatPerDayTotalMessageRecv . entityVal) botStats
