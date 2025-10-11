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
import Utils.LabelKeys ()
import Module.Prometheus.Manager

periodicCostHandleCronTabTick :: CronTabTick -> Meow [BotAction]
periodicCostHandleCronTabTick (CronTabTick now) = botT $ do
  MaybeT $ pure $ guard $ timeMatchesCron now hourlySchedule
  botId :: BotId <- query
  lift $ checkAndUpdatePeriodicCost botId now
  return []

checkAndUpdatePeriodicCost :: BotId -> UTCTime -> Meow ()
checkAndUpdatePeriodicCost botId now = do
  mBotCostModel        <- runMeowDB $ getBotPeriodicCostModel        botId
  lBotCostModelPerChat <- runMeowDB $ getBotPerChatPeriodicCostModel botId
  let listCosts = [ (p, Left  bcm)  | Just (p, bcm) <- [mBotCostModel] ] ++
                  [ (p, Right bcmc) | (p, bcmc) <- lBotCostModelPerChat ]
  _inserted <- forM listCosts $ \(p, bcm) -> do
    let bcm'  = either (Left . entityKey) (Right . entityKey) bcm
        bcm'' = either (Left . entityVal) (Right . entityVal) bcm
        mCap  = hasCap $ either botCostModelCostModel botCostModelPerChatCostModel bcm''
    mLastRecord <- runMeowDB $ findLastCostRecord bcm'
    countRecords <- runMeowDB $ countCostRecords bcm'
    receivedMessageToday <- runMeowDB $ receivedMessageToday bcm'' (utctDay now)

    let notCapped = maybe True (countRecords <) mCap
        needAction =  receivedMessageToday > 0
                   && notCapped
                   && periodicLogic Daily now (fmap periodicCostRecordTime mLastRecord)

    when needAction $ do
      res <- runMeowDB $ insertNewCostRecord (dailyCost p) bcm now
      case res of
        Just meow -> do
          $logInfo  $ "Inserted daily basic cost record for " <> toText bcm
          meow
        Nothing -> $logError $ "Failed to insert daily basic cost record for " <> toText bcm
  return ()

insertNewCostRecord :: DailyBasicCost -> Either (Entity BotCostModel) (Entity BotCostModelPerChat) -> UTCTime -> DB (Maybe (Meow ()))
insertNewCostRecord dailyCost (Left enBCM) now = runMaybeT $ do
  let costRecord = PeriodicCostRecord
        { periodicCostRecordTime                      = now
        , periodicCostRecordReferToCostModelId        = Just enBCM.entityKey
        , periodicCostRecordReferToCostModelPerChatId = Nothing
        , periodicCostRecordWalletId                  = enBCM.entityVal.botCostModelWalletId
        , periodicCostRecordDescription               = Just "Daily basic cost"
        , periodicCostRecordCost                      = coerce dailyCost
        }
  MaybeT $ pure $ guard $ hasOtherCost enBCM.entityVal.botCostModelCostModel
  lift $ do
    update enBCM.entityVal.botCostModelWalletId [WalletBalance -=. coerce dailyCost]
    insert_ costRecord
  let labels = [ Label @"bot_id" enBCM.entityVal.botCostModelBotId
               , Label @"periodic_cost_reason" "DailyBasicCost"
               ]
  return $ managedGauge "meowbot_periodic_cost_total" labels (GaugeAdd $ coerce dailyCost)
insertNewCostRecord dailyCost (Right enBCMP) now = runMaybeT $ do
  let costRecord = PeriodicCostRecord
        { periodicCostRecordTime                      = now
        , periodicCostRecordReferToCostModelId        = Nothing
        , periodicCostRecordReferToCostModelPerChatId = Just enBCMP.entityKey
        , periodicCostRecordWalletId                  = enBCMP.entityVal.botCostModelPerChatWalletId
        , periodicCostRecordDescription               = Just "Daily basic cost"
        , periodicCostRecordCost                      = coerce dailyCost
        }
  MaybeT $ pure $ guard $ hasOtherCost enBCMP.entityVal.botCostModelPerChatCostModel
  lift $ do
    update enBCMP.entityVal.botCostModelPerChatWalletId [WalletBalance -=. coerce dailyCost]
    insert_ costRecord
  let labels = [ Label @"bot_id" enBCMP.entityVal.botCostModelPerChatBotId
               , Label @"chat_id" (coerce enBCMP.entityVal.botCostModelPerChatChatId)
               , Label @"periodic_cost_reason" "DailyBasicCost"
               ]
  return $ managedGauge "meowbot_periodic_cost_total" labels (GaugeAdd $ coerce dailyCost)

findLastCostRecord :: Either BotCostModelId BotCostModelPerChatId -> DB (Maybe PeriodicCostRecord)
findLastCostRecord (Left botCostModelId)         = fmap entityVal <$>
  selectFirst [PeriodicCostRecordReferToCostModelId ==. Just botCostModelId] [Desc PeriodicCostRecordTime]
findLastCostRecord (Right botCostModelPerChatId) = fmap entityVal <$>
  selectFirst [PeriodicCostRecordReferToCostModelPerChatId ==. Just botCostModelPerChatId] [Desc PeriodicCostRecordTime]

countCostRecords :: Either BotCostModelId BotCostModelPerChatId -> DB Int
countCostRecords (Left botCostModelId)         =
  count [PeriodicCostRecordReferToCostModelId ==. Just botCostModelId]
countCostRecords (Right botCostModelPerChatId) =
  count [PeriodicCostRecordReferToCostModelPerChatId ==. Just botCostModelPerChatId]

getBotPeriodicCostModel :: BotId -> DB (Maybe (OtherCost, Entity BotCostModel))
getBotPeriodicCostModel botId = runMaybeT $ do
  enCM      <- MaybeT $ getBotCostModel botId
  basicCost <- MaybeT $ pure $ getOtherCost enCM.entityVal.botCostModelCostModel
  return ( basicCost, enCM )

getBotPerChatPeriodicCostModel :: BotId -> DB [(OtherCost, Entity BotCostModelPerChat)]
getBotPerChatPeriodicCostModel botId = do
  list <- getBotPerChatCostModel botId
  return [ (p, en)
         | en@(Entity _ modelPerChat) <- list
         , let mP = getOtherCost modelPerChat.botCostModelPerChatCostModel
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
