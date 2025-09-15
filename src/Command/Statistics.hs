{-# LANGUAGE TransformListComp #-}
-- | This command generates interesting statistics about the current chat.
module Command.Statistics where

import Control.Monad.Logger
import Command
import Data.PersistModel
import Data.List (sortOn)
import Data.Ord (Down(..))
import MeowBot
import MeowBot.GetInfo
import MeowBot.Parser
import MeowBot.Prelude
import Utils.Diagrams
import Utils.Diagrams.Render
import Utils.Esqueleto
import Utils.RunDB
import Utils.TH
import Utils.Persist
import qualified Database.Esqueleto.Experimental as SQL

makeNewInts ["NUsers", "NDays"]

data StatisticsCommand
  = StatActivity     (Maybe NDays) (Maybe NUsers)
  | StatActivityUser (Maybe NDays) (Maybe UserId)

data StatisticsAction
  = AStatActivity     BotId ChatId NDays NUsers -- ^ Show activity statistics for the past N days and top N users in the chat
  | AStatActivityUser BotId ChatId NDays UserId -- ^ Show activity statistics for the past N days and top N users in the chat

statisticsCommandToAction :: BotId -> (ChatId, UserId) -> StatisticsCommand -> Maybe StatisticsAction
statisticsCommandToAction bid (cid, _)   (StatActivity mDays mUsers)
  | maybe True (<=180) mDays && maybe True (<= 50) mUsers = Just $ AStatActivity bid cid (fromMaybe 7 mDays) (fromMaybe 15 mUsers)
  | otherwise = Nothing
statisticsCommandToAction bid (cid, uid) (StatActivityUser mDays mUser)
  | maybe True (<=180) mDays = Just $ AStatActivityUser bid cid (fromMaybe 30 mDays) (fromMaybe uid mUser)
  | otherwise = Nothing

statisticsAction :: Maybe String -> ChatId -> StatisticsAction -> Meow (Maybe [BotAction])
statisticsAction mName scid = \case
  AStatActivity bid cid days users -> do
    now <- liftIO getCurrentTime
    let startDay = addUTCTime (negate $ fromIntegral (days.unNDays * 86400)) now
    userActivity <- fmap toUnValue $ runDB $ SQL.select $ do
      p <- SQL.from $ SQL.table @ChatMessage
      SQL.where_
        (         p SQL.^. ChatMessageBotId   SQL.==. SQL.val bid
        SQL.&&.   p SQL.^. ChatMessageChatId  SQL.==. SQL.val cid
        SQL.&&.   SQL.not_ (SQL.isNothing (p SQL.^. ChatMessageUserId)) -- is not null
        SQL.&&.   p SQL.^. ChatMessageTime    SQL.>=. SQL.val startDay
        )
      SQL.groupBy (p SQL.^. ChatMessageUserId)
      let countMessages = SQL.countRows :: SQL.SqlExpr (SQL.Value Int)
      SQL.orderBy [SQL.desc countMessages]
      SQL.limit $ fromIntegral users
      pure (p SQL.^. ChatMessageUserId, countMessages)
    selfActivity <- fmap toUnValue $ runDB $ SQL.select $ do
      p <- SQL.from $ SQL.table @ChatMessage
      SQL.where_
        (         p SQL.^. ChatMessageBotId     SQL.==. SQL.val bid
        SQL.&&.   p SQL.^. ChatMessageChatId    SQL.==. SQL.val cid
        SQL.&&.   p SQL.^. ChatMessageEventType SQL.==. SQL.val (PersistUseInt64 SelfMessage)
        SQL.&&.   p SQL.^. ChatMessageTime      SQL.>=. SQL.val startDay
        )
      let countMessages = SQL.countRows :: SQL.SqlExpr (SQL.Value Int)
      pure countMessages
    let allActivity =  take (fromIntegral users)
                    $  sortOn (Down . snd)
                    $  [(name u, fromIntegral a) | (u, a) <- userActivity]
                    <> maybeToList ((fromMaybe ">w<" mName, ) . fromIntegral <$> listToMaybe selfActivity)
        plot = SimpleNamedBarPlot
                 { snbpTitle =  "Top " ++ show (length allActivity)
                             ++ " active users in the past "
                             ++ show days ++ " days (number of messages)"
                 , snbpData  = reverse allActivity
                 }
        name (Just (UserId uid)) = show uid
        name Nothing             = "owo?"
        lbs = axisToPngLbs (mkSizeSpec2D (Just 1200) Nothing) $ simpleNamedBarPlot plot
    $logInfo $ "userActivity: " <> toText allActivity
    $logInfo $ "selfActivity: " <> toText selfActivity
    return $ Just [ baSendImageLbs scid lbs ]
    
  _ -> return Nothing

checkPrivilegeStatistics :: IsSuperUser -> (ChatId, UserId) -> StatisticsCommand -> Bool
checkPrivilegeStatistics _ (GroupChat{}, _)    StatActivity{}                      = True
checkPrivilegeStatistics _ (GroupChat{}, _)    StatActivityUser {}                 = True
checkPrivilegeStatistics _ (PrivateChat{}, _) (StatActivity{}; StatActivityUser{}) = False

statisticsParser :: Parser Text Char (NonEmpty StatisticsCommand)
statisticsParser = innerParserToBatchParser innerStatisticsParser
  where innerStatisticsParser = asum
          [ statP >> StatActivity <$> optMaybe (spaces >> ndaysP) <*> optMaybe (spaces >> nUsersP) ]
        statP   = asum [ string "stat", string "statistics" ]
        ndaysP  = nint <* (do spaces0; string "d" <|> string "days")
        nUsersP = nint <* (do spaces0; string "u" <|> string "users")

commandStatistics :: BotCommand
commandStatistics = BotCommand Statistics $ botT $ do
  (msg, cid, uid, _mid, _sender) <- MaybeT $ getEssentialContent <$> query
  botid           <- query
  BotName botName <- query
  parsedCommands  <- MaybeT $ (`runParser` msg) <$> commandParserTransformByBotName statisticsParser
  isSuper         <- lift $ isSuperUser uid
  let privilege = all (checkPrivilegeStatistics isSuper (cid, uid)) parsedCommands
      mActions  = statisticsCommandToAction botid (cid, uid) `mapM` parsedCommands
  case (mActions, privilege) of
    (Nothing, _)                -> return [baSendToChatId cid "Invalid command or parameters."]
    (Just (action :| []), True) -> MaybeT $ statisticsAction botName cid action
    (Just actions, True)        -> MaybeT $ concatOutput (Just "") $ statisticsAction botName cid `mapM` actions
    (Just _, False)             -> return [baSendToChatId cid "Operation not permitted."]
