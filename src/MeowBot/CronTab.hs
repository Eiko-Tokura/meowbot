module MeowBot.CronTab where

import Command.Chat
import Command.Cat.CatSet
import Control.Monad.Readable
import Cron.Match
import Cron.Parser
import Data.Default
import Data.PersistModel
import Data.Time
import External.ChatAPI
import MeowBot.CronTab.CronMeowAction
import System.Meow
import Utils.Text
import Utils.RunDB
import qualified Data.Map.Strict as SM
import Data.Additional.Default
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async

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
          let newTimes = case botCronJobCronRepeatFinite job of
                Nothing -> Nothing
                Just n -> if n > 1 then Just (n - 1) else Just 0
              newJob = job { botCronJobCronRepeatFinite = newTimes }
          if newTimes == Just 0
            then runDB $ delete key
            else runDB $ replace key newJob
  
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

  allChatState <- updateChatState <$> getTypeWithDef newChatState
  putType $ allChatState

  asyncChat <- liftIO $ async $ return $ command commandChat
  return [BAAsync asyncChat]
