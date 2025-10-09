{-# LANGUAGE OverloadedStrings #-}
module Command.Hangman.Ranking where

import Command.Hangman.Model
import Control.Monad
import Data.List (tails, inits, maximumBy)
import Data.Ord
import Data.Maybe
import Data.PersistModel
import qualified Data.Set as S
import MeowBot.Data
import System.Meow
import Utils.RunDB

data ViewRanking = ViewPersonalRanking | ViewGlobalRanking | UpdateAllRanking | UpdateAllScores deriving (Show, Eq)

type AccPair = (Int, Int) -- miss count and total guess count
type PlayCount = (Int, Int) -- pass count and total play count

updateTotalPP
  :: UserId             -- ^ user id
  -> Maybe Text         -- ^ user nickname
  -> Meow (Double, Int, AccPair, PlayCount) -- ^ returns total pp and rank
updateTotalPP uid mnick = do
  listScores <- runMeowDB $ selectList [HangmanRecordUserId ==. uid] [Desc HangmanRecordId]
  let totalPP = computePP (fromMaybe 0 . hangmanRecordScore . entityVal <$> listScores)
  rank <- fmap (+1) . runMeowDB $ count [HangmanRankingTotalPP >. totalPP, HangmanRankingUserId !=. uid]
  let pc = length listScores
      pass = length $ filter (completedPlay . hangmanRecordToState . entityVal) listScores
      accPairs@(totalMiss, totalGuess)
        = foldl' (\(x,y) (x',y') -> (x+x', y+y')) (0, 0)
        $ accuracyPair . hangmanRecordToState . entityVal <$> listScores
  runMeowDB $ upsert (HangmanRanking uid (fromMaybe "" mnick) totalPP rank totalMiss totalGuess pass pc)
          ( [ HangmanRankingTotalPP    =. totalPP
            , HangmanRankingRank       =. rank
            , HangmanRankingTotalMiss  =. totalMiss
            , HangmanRankingTotalGuess =. totalGuess
            , HangmanRankingPassCount  =. pass
            , HangmanRankingPlaycount  =. pc
            ] <>
            [ HangmanRankingUserNickName =. nick | Just nick <- [mnick] ]
          )
  return (totalPP, rank, accPairs, (pass, pc))

recalculateAllScores :: Meow ()
recalculateAllScores = do
  allScores <- runMeowDB $ selectList [] [Desc HangmanRecordId]
  let newAllScores = (\score ->
        let scoreRec = entityVal $ score
            state = hangmanRecordToState scoreRec
        in (scoreRec { hangmanRecordScore = Just $ hangmanScoring state }, entityKey score)
        ) <$> allScores
  runMeowDB $ forM_ newAllScores $ \(score, key) -> replace key score

computePP :: [Double] -> Double
computePP = (c *) . weightedSum lambda . map fst . greedyGrouping g
  where c      = 100 * (1 - lambda) / fromIntegral g
        lambda = 0.66
        g      = 10

weightedSum :: Double -> [Double] -> Double
weightedSum lambda = sum . zipWith (*) (map (lambda^) [0::Int ..])

greedyGrouping :: Int -> [Double] -> [(Double, [Double])]
greedyGrouping _ [] = []
greedyGrouping groupSize l | length l <= groupSize = [(sum l, l)]
greedyGrouping groupSize l =
  let (restLeft, maxSumGroup, restRight) = getMaxSumGroup groupSize l
  in maxSumGroup : mergeBySum (greedyGrouping groupSize restLeft) (greedyGrouping groupSize restRight)
  where mergeBySum [] r = r
        mergeBySum l [] = l
        mergeBySum (x:xs) (y:ys) | fst x > fst y = x : mergeBySum xs (y:ys)
                                 | otherwise     = y : mergeBySum (x:xs) ys

getMaxSumGroup :: Int -> [Double] -> ([Double], (Double, [Double]), [Double])
getMaxSumGroup groupSize l = maximumBy (comparing (\(_,(s,_),_) -> s)) $
  [ (left, (sum group, group), right) | (left, rest) <- zip (inits l) (tails l)
                         , let group = take groupSize rest
                               right = drop groupSize rest
                         ]

-- | get the ranking of the hangman game
getRanking :: Meow [HangmanRanking]
getRanking = fmap (fmap entityVal) . runMeowDB $ selectList [] [Desc HangmanRankingTotalPP]

-- | get the ranking of a user, no update performed
getUserRank :: UserId -> Meow (Maybe (Double, Int))
getUserRank uid = fmap (fmap $ (\h -> (hangmanRankingTotalPP h, hangmanRankingRank h)) . entityVal) . runMeowDB $ getBy (UniqueUserId uid)

updateAllRanking :: Meow ()
updateAllRanking = do
  allScores <- runMeowDB $ selectList [] [Desc HangmanRecordId]
  let allUsers = S.toList . S.fromList $ map (hangmanRecordUserId . entityVal) allScores
  forM_ allUsers $ \uid -> do
    updateTotalPP uid Nothing
