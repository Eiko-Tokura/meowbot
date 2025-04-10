module MeowBot.CronTab where

import Command.Cat.CatSet
import Command.Chat
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Readable
import Cron.Match
import Cron.Parser
import Data.Additional.Default
import Data.Default
import Data.PersistModel
import Data.Time
import External.ChatAPI
import MeowBot.CronTab.CronMeowAction
import System.Meow
import Utils.RunDB
import Utils.Text
import qualified Data.Map.Strict as SM

data CronTabTick = CronTabTick UTCTime

meowHandleCronTabTick :: CronTabTick -> Meow [BotAction]
meowHandleCronTabTick (CronTabTick time) = do
  triggeredJobs <- filter ((\(en,s,_) -> timeMatchesCron time s && hasChanceToRun (botCronJobCronRepeatFinite $ entityVal en))) <$> getCronSchedulesDb
  reduceRepeatCount $ (\(en,_,_) -> en) <$> triggeredJobs
  fmap concat . sequence $ map (performCronMeowAction . (\(_,_,m) -> m)) triggeredJobs
  where hasChanceToRun Nothing = True
        hasChanceToRun (Just n) = n > 0
        reduceRepeatCount :: [Entity BotCronJob] -> Meow ()
        reduceRepeatCount list = forM_ list $ \(Entity key job) -> do
          $(logInfo) $ "CronJob triggered: " <> toText job
          let newTimes = case botCronJobCronRepeatFinite job of
                Nothing -> Nothing
                Just n -> if n > 1 then Just (n - 1) else Just 0
              newJob = job { botCronJobCronRepeatFinite = newTimes }
          if newTimes == Just 0
            then do
              $(logInfo) $ "CronJob reapeat finished, removing from database : " <> toText job
              runDB $ delete key
            else do
              $(logInfo) $ "CronJob repeat count reduced: " <> toText newJob
              runDB $ replace key newJob
  
getCronSchedulesDb :: Meow [(Entity BotCronJob, CronSchedule, CronMeowAction)]
getCronSchedulesDb = do
  botId <- query
  botCronJobs <- runDB $ selectList [BotCronJobBotId ==. botId] []
  return $ (\en@(Entity _ job) -> (en, cronTextToCronSchedule $ botCronJobCronSchedule job, botCronJobCronMeowAction job)) <$> botCronJobs

performCronMeowAction :: CronMeowAction -> Meow [BotAction]
performCronMeowAction (CronMeowChatBack chatId message) = do
  time <- liftIO getCurrentTime
  -- need to modify ChatState
  -- then trigger the command Chat
  let newMsg = ToolMessage ("Time reached: " <> toText time <> "\n" <> message) def

      cid = chatId

      newChatState = SM.empty :: AllChatState

      updateChatState :: AllChatState -> AllChatState
      updateChatState s =
        let mstate = SM.lookup cid s in
        case mstate of
          Just cs -> SM.insert cid
            cs
              { chatStatus = (chatStatus cs)
                { chatStatusMessages = chatStatusMessages (chatStatus cs) <> [newMsg]
                , chatStatusToolDepth = 0 -- ^ reset tool depth
                }
              , activeTriggerOneOff = True
              }
            s
          Nothing -> SM.insert cid
            def
              { chatStatus =  (ChatStatus 0 0 [newMsg])
              , activeTriggerOneOff = True
              } s


  asyncChat <- liftIO $ async $ return $ do
    allChatState <- updateChatState <$> getTypeWithDef newChatState
    putType $ allChatState
    command commandChat
  return [BAAsync asyncChat]
