{-# LANGUAGE QuasiQuotes, TransformListComp #-}
-- | This command generates interesting statistics about the current chat.
module Command.Statistics where

import Command
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.PersistModel
import MeowBot
import MeowBot.GetInfo
import MeowBot.Parser
import MeowBot.Prelude
import Text.Shakespeare.Text (st)
import Utils.Diagrams
import Utils.Diagrams.Render
import Utils.Diagrams.Scatter
import Utils.Esqueleto
import Utils.List
import Utils.Persist
import Utils.RunEsql
import Utils.TH
import qualified Data.Map.Strict as M
import qualified Database.Persist.Sql as Sql
import qualified Diagrams.Prelude as D
import qualified Numeric.LinearAlgebra as LA

makeNewInts ["NUsers", "NDays"]

data StatisticsCommand
  = StatActivity     (Maybe NDays) (Maybe NUsers)
  | StatActivityUser (Maybe NDays) (Maybe UserId)
  | StatProximity    (Maybe NDays) (Maybe NUsers)

data StatisticsAction
  = AStatActivity     BotId ChatId NDays NUsers
    -- ^ Show activity statistics for the past N days and top N users in the chat
  | AStatActivityUser BotId ChatId NDays UserId
    -- ^ Show activity statistics for the past N days and top N users in the chat
  | AStatProximity    BotId GroupId NDays NUsers

statisticsCommandToAction :: BotId -> (ChatId, UserId) -> StatisticsCommand -> Maybe StatisticsAction
statisticsCommandToAction bid (cid, _)   (StatActivity mDays mUsers)
  | maybe True (<=180) mDays && maybe True (<= 50) mUsers = Just $ AStatActivity bid cid (fromMaybe 7 mDays) (fromMaybe 15 mUsers)
  | otherwise = Nothing
statisticsCommandToAction bid (cid, uid) (StatActivityUser mDays mUser)
  | maybe True (<=180) mDays = Just $ AStatActivityUser bid cid (fromMaybe 30 mDays) (fromMaybe uid mUser)
  | otherwise = Nothing
statisticsCommandToAction bid (GroupChat gid, _)   (StatProximity mDays mUsers)
  | maybe True (<=180) mDays && maybe True (<= 100) mUsers = Just $ AStatProximity bid gid (fromMaybe 30 mDays) (fromMaybe 30 mUsers)
  | otherwise = Nothing
statisticsCommandToAction _ (PrivateChat {}, _)   (StatProximity {}) = Nothing

statisticsAction :: Maybe String -> ChatId -> StatisticsAction -> Meow (Maybe [BotAction])
statisticsAction mName scid = \case
  AStatActivity bid cid days users -> do
    pool <- askDB
    now <- liftIO getCurrentTime
    let startDay = addUTCTime (negate $ fromIntegral (days.unNDays * 86400)) now
    fetchStat <- liftIO . async $ do -- ^ doing the database fetch asynchronously, avoid blocking the main thread
      userActivity <- fmap toUnValue $ flip runSqlPool pool $ select $ do
        p <- from $ table @ChatMessage
        where_
          (   p ^. ChatMessageBotId  ==. val bid
          &&. p ^. ChatMessageChatId ==. val cid
          &&. not_ (isNothing_ (p ^. ChatMessageUserId)) -- is not null
          &&. p ^. ChatMessageTime   >=. val startDay
          )
        groupBy (p ^. ChatMessageUserId)
        let countMessages = countRows :: SqlExpr (Value Int)
        orderBy [desc countMessages]
        limit $ fromIntegral users
        pure (p ^. ChatMessageUserId, countMessages)
      selfActivity <- fmap toUnValue $ flip runSqlPool pool $ select $ do
        p <- from $ table @ChatMessage
        where_
          (   p ^. ChatMessageBotId     ==. val bid
          &&. p ^. ChatMessageChatId    ==. val cid
          &&. p ^. ChatMessageEventType ==. val (PersistUseInt64 SelfMessage)
          &&. p ^. ChatMessageTime      >=. val startDay
          )
        let countMessages = countRows :: SqlExpr (Value Int)
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
          !lbs = axisToPngLbs (mkSizeSpec2D (Just 1080) Nothing) $ simpleNamedBarPlot plot
      return [ baSendImageLbs scid lbs ]
    return $ Just [BAPureAsync fetchStat]

  AStatProximity bid cid days users -> do
    pool <- askDB
    fetchStat <- liftIO . async $ flip runSqlPool pool $ do
      result <- statProximity bid cid days users
      let toPairs = weightPairsToPoints
                    [ ( (r.sprUserId1, unpack $ fromMaybe (toText $ unUserId $ sprUserId1 r) $ sprName1 r)
                      , (r.sprUserId2, unpack $ fromMaybe (toText $ unUserId $ sprUserId2 r) $ sprName2 r)
                      , sprWeight r
                      )
                    | r <- result
                    ]
          userCount = M.fromList [ (r.sprUserId1, r.sprN1) | r <- result ]
          avgCount  = fromIntegral (sum (M.elems userCount)) / max 1 (fromIntegral $ M.size userCount)
          userSize  = M.map (\n -> max 0.1 $ (fromIntegral n / avgCount) ** (1/2)) userCount
          toPairsSized =  [ ( name1, userSize M.! uid1, pos )
                          | ( (uid1, name1), pos ) <- toPairs
                          , M.member uid1 userSize
                          ]
          plot = scatterSized toPairsSized
          title = "群友宇宙owo (in the past " ++ show days ++ " days)"
          diagram = D.vcat [ plot, D.text title & D.fontSizeL 0.018 & D.fcA (D.black `D.withOpacity` 0.7) ]
          !lbs = defaultToPngLbs (mkSizeSpec2D (Just 1600) Nothing) diagram
      return [ baSendImageLbs scid lbs ]
    return $ Just [baSendToChatId scid "正在计算中，等我几分钟>wo (请不要重复发送命令>.<!)", BAPureAsync fetchStat]
  _ -> return Nothing

checkPrivilegeStatistics :: IsSuperUser -> (ChatId, UserId) -> StatisticsCommand -> Bool
checkPrivilegeStatistics _ (GroupChat{}, _)    StatActivity{}      = True
checkPrivilegeStatistics _ (GroupChat{}, _)    StatActivityUser {} = True
checkPrivilegeStatistics _ (GroupChat{}, _)    StatProximity {}    = True
checkPrivilegeStatistics _ (PrivateChat{}, _) (StatActivity{}; StatActivityUser{}; StatProximity{}) = False

statisticsParser :: Parser Text Char (NonEmpty StatisticsCommand)
statisticsParser = innerParserToBatchParser innerStatisticsParser
  where innerStatisticsParser = asum
          [ statP >> StatActivity <$> optMaybe (spaces >> ndaysP) <*> optMaybe (spaces >> nUsersP)
          , statP >> spaces >> proxP >> StatProximity <$> optMaybe (spaces >> ndaysP) <*> optMaybe (spaces >> nUsersP)
          ]
        statP   = asum [ string "stat", string "statistics" ]
        proxP   = string "proximity" <|> string "prox"
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

data StatProximityRow = StatProximityRow
  { sprUserId1 :: UserId
  , sprName1   :: Maybe Text
  , sprN1      :: Int
  , sprUserId2 :: UserId
  , sprName2   :: Maybe Text
  , sprN2      :: Int
  , sprWeight  :: Double
  }

statProximity :: BotId -> GroupId -> NDays -> NUsers -> DB [StatProximityRow]
statProximity bid (GroupId gid) ndays nUsers = do
  let wReply   = toText (5.0 :: Double) :: Text
      wAdj     = toText (1.0 :: Double) :: Text
      kReplySec = 1200 :: Int -- 20 minutes
      kAdjSec   = 300  :: Int  -- 5 minutes
      adjSecMax = 1200 :: Int -- 20 minutes
  let alphaEdge = toText (2.0 :: Double) :: Text  -- edge reliability shrink
      alphaDeg  = toText (1.0 :: Double) :: Text  -- degree smoothing (units of weight)
  rows :: [ ( Single UserId, Single Int, Single (Maybe Text), Single (Maybe Text), Single UserId, Single Int, Single (Maybe Text), Single (Maybe Text), Single Double ) ]
    <- (`Sql.rawSql` [])
      [st|
WITH m AS (
  SELECT id, user_id, time, message_id, sender_card, sender_nickname, reply_to
  FROM chat_message
  WHERE bot_id = #{bid.unBotId}
    AND chat_id = #{gid}
    AND user_id IS NOT NULL
    AND time >= datetime('now', '-' || #{ndays.unNDays} || ' days')
),

-- per-user message counts in-window
user_msgs AS (
  SELECT user_id, COUNT(*) AS n_msgs
  FROM m
  GROUP BY user_id
),

-- top-N users by activity
top_users AS (
  SELECT user_id, n_msgs
  FROM user_msgs
  ORDER BY n_msgs DESC, user_id
  LIMIT #{nUsers.unNUsers}
),

-- replies: only keep pairs where BOTH endpoints are in top_users
edges_reply AS (
  SELECT
    CASE WHEN a.user_id < r.user_id THEN a.user_id ELSE r.user_id END AS u1,
    CASE WHEN a.user_id < r.user_id THEN r.user_id ELSE a.user_id END AS u2,
    ABS(strftime('%s', a.time) - strftime('%s', r.time)) AS dt_sec
  FROM m AS a
  JOIN chat_message AS r
    ON r.bot_id     = #{bid.unBotId}
   AND r.chat_id    = #{gid}
   AND r.message_id = a.reply_to
  JOIN top_users t1 ON t1.user_id = a.user_id
  JOIN top_users t2 ON t2.user_id = r.user_id
  WHERE a.reply_to IS NOT NULL
    AND r.user_id IS NOT NULL
    AND a.user_id <> r.user_id
),
edges_reply_w AS (
  SELECT u1, u2,
         #{wReply} * (1.0 / (1.0 + (dt_sec * 1.0) / #{kReplySec})) AS w
  FROM edges_reply
),

-- adjacent alternating turns within #{adjSecMax}, restricted to top_users
seq AS (
  SELECT
    id, user_id, time,
    LAG(user_id) OVER (ORDER BY time) AS prev_uid,
    LAG(time)    OVER (ORDER BY time) AS prev_time
  FROM m
  WHERE user_id IN (SELECT user_id FROM top_users)
),
edges_adjacent AS (
  SELECT
    CASE WHEN s.user_id < s.prev_uid THEN s.user_id ELSE s.prev_uid END AS u1,
    CASE WHEN s.user_id < s.prev_uid THEN s.prev_uid ELSE s.user_id END AS u2,
    (strftime('%s', s.time) - strftime('%s', s.prev_time)) AS dt_sec
  FROM seq AS s
  WHERE s.prev_uid IS NOT NULL
    AND s.user_id <> s.prev_uid
    AND (strftime('%s', s.time) - strftime('%s', s.prev_time)) BETWEEN 0 AND #{adjSecMax}
),
edges_adjacent_w AS (
  SELECT u1, u2,
         #{wAdj} * (1.0 / (1.0 + (dt_sec * 1.0) / #{kAdjSec})) AS w
  FROM edges_adjacent
),

-- combine, aggregate per pair, and apply reliability shrink by number of events
edges AS (
  SELECT u1, u2,
         SUM(w)   AS w_sum,
         COUNT(*) AS n_events
  FROM (
    SELECT u1,u2,w FROM edges_reply_w
    UNION ALL
    SELECT u1,u2,w FROM edges_adjacent_w
  )
  GROUP BY u1,u2
),

-- OPTIONAL: require a minimum number of interactions to keep an edge
--edges_filtered AS (
--  SELECT * FROM edges WHERE n_events >= 2
--),
-- use edges_filtered instead of edges below if you enable the threshold

edges_shrunk AS (
  SELECT
    u1, u2,
    -- reliability shrink: n / (n + alphaEdge)
    w_sum * ((n_events * 1.0) / (n_events + #{alphaEdge})) AS w_eff
  FROM edges
),

-- degrees on the induced top-users subgraph using shrunk weights
deg AS (
  SELECT u AS uid, SUM(w_eff) AS d FROM (
    SELECT u1 AS u, w_eff FROM edges_shrunk
    UNION ALL
    SELECT u2 AS u, w_eff FROM edges_shrunk
  ) GROUP BY u
),

-- normalization: arithmetic mean with degree smoothing (no sqrt needed)
edges_norm AS (
  SELECT
    e.u1, e.u2,
    e.w_eff /  sqrt((COALESCE(d1.d,0)+#{alphaDeg}) * (COALESCE(d2.d,0)+#{alphaDeg})) AS w_norm
  FROM edges_shrunk e
  LEFT JOIN deg d1 ON d1.uid = e.u1
  LEFT JOIN deg d2 ON d2.uid = e.u2
),

-- recent display fields restricted to top users (cheap)
recent_card AS (
  SELECT user_id, sender_card AS card
  FROM (
    SELECT user_id, sender_card,
           ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY time DESC) AS rn
    FROM m
    WHERE user_id IN (SELECT user_id FROM top_users)
      AND sender_card IS NOT NULL AND sender_card <> ''
  ) t WHERE rn = 1
),
recent_nick AS (
  SELECT user_id, sender_nickname AS nickname
  FROM (
    SELECT user_id, sender_nickname,
           ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY time DESC) AS rn
    FROM m
    WHERE user_id IN (SELECT user_id FROM top_users)
      AND sender_nickname IS NOT NULL AND sender_nickname <> ''
  ) t WHERE rn = 1
)

SELECT
  en.u1,
  tu1.n_msgs               AS n1,
  rc1.card                 AS card_u1,
  rn1.nickname             AS nickname_u1,
  en.u2,
  tu2.n_msgs               AS n2,
  rc2.card                 AS card_u2,
  rn2.nickname             AS nickname_u2,
  en.w_norm
FROM edges_norm en
JOIN top_users  tu1 ON tu1.user_id = en.u1
JOIN top_users  tu2 ON tu2.user_id = en.u2
LEFT JOIN recent_card rc1 ON rc1.user_id = en.u1
LEFT JOIN recent_nick rn1 ON rn1.user_id = en.u1
LEFT JOIN recent_card rc2 ON rc2.user_id = en.u2
LEFT JOIN recent_nick rn2 ON rn2.user_id = en.u2
ORDER BY en.w_norm DESC;
      |]
  return $ [ StatProximityRow
              { sprUserId1 = u1
              , sprN1      = n1
              , sprName1   = mcard1 <|> mname1
              , sprUserId2 = u2
              , sprN2      = n2
              , sprName2   = mcard2 <|> mname2
              , sprWeight  = w_norm
              }
           | ( Single u1, Single n1, Single mcard1, Single mname1
             , Single u2, Single n2, Single mcard2, Single mname2
             , Single w_norm ) <- rows
           ]

weightPairsToPoints :: (Ord a) => [(a, a, Double)] -> [(a, (Double, Double))]
weightPairsToPoints ws =
  let nameL = nubList $ [ x | (x,_,_) <- ws ] ++ [ y | (_,y,_) <- ws ]
      nameIdMap = M.fromList $ zip nameL [0..]
      idNameMap = M.fromList $ zip [0..] nameL
      n = M.size nameIdMap
      wMat = LA.trustSym $ LA.assoc (n,n) 0.0 $
        [ ((nameIdMap M.! x, nameIdMap M.! y), v)
        | (x,y,v) <- ws
        ]
        ++
        [ ((nameIdMap M.! y, nameIdMap M.! x), v)
        | (x,y,v) <- ws
        ]
        ++
        [ ((i,i), 1.0)
        | i <- [0..n-1]
        ]
      (vx, vy) = innerTo2D (LA.konst 1.0 n) wMat
  in [ ( idNameMap M.! i
       , (vx LA.! i, vy LA.! i)
       )
     | i <- [0..n-1]
     ]

-- | input symmetric {w_{ij}} matrix representing inner products
-- output two vectors (x_i), (y_i) in R^2 st. w_{ij} ~ <x_i, y_j> approx
-- using eigen-decomposition
innerTo2D :: LA.Vector Double -> LA.Herm Double -> (LA.Vector Double, LA.Vector Double)
innerTo2D d w =
  let diagDSqrtInv = LA.diag (LA.cmap sqrt d)
      -- dimension = LA.size d
      -- identity = LA.ident dimension
      l_sym = LA.sym $ diagDSqrtInv LA.<> LA.unSym w LA.<> diagDSqrtInv
      --- identity
      (_eigenVals, eigenVects) = LA.eigSH l_sym
      eiVs = LA.toColumns eigenVects
  in ( eiVs !! 0
     , eiVs !! 1
     )
