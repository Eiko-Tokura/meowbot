{-# LANGUAGE OverloadedStrings #-}
module Command.Hangman.Ranking where

import Utils.RunDB
import Data.PersistModel
import System.Meow
import Data.Maybe
import Data.List (tails)
import Control.Monad
import qualified Data.Set as S
import MeowBot.Data
import Command.Hangman.Model

data ViewRanking = ViewPersonalRanking | ViewGlobalRanking | UpdateAllRanking

type AccPair = (Int, Int) -- miss count and total guess count
type PlayCount = (Int, Int) -- pass count and total play count

updateTotalPP
  :: UserId             -- ^ user id
  -> Maybe Text               -- ^ user nickname
  -> Meow (Double, Int, AccPair, PlayCount) -- ^ returns total pp and rank
updateTotalPP uid mnick = do
  listScores <- runDB $ selectList [HangmanRecordUserId ==. uid] [Desc HangmanRecordId]
  let totalPP = computePP (fromMaybe 0 . hangmanRecordScore . entityVal <$> listScores)
  rank <- fmap (+1) . runDB $ count [HangmanRankingTotalPP >. totalPP, HangmanRankingUserId !=. uid]
  let pc = length listScores
      pass = length $ filter (completedPlay . hangmanRecordToState . entityVal) listScores
      accPairs@(totalMiss, totalGuess) 
        = foldl' (\(x,y) (x',y') -> (x+x', y+y')) (0, 0) 
        $ accuracyPair . hangmanRecordToState . entityVal <$> listScores
  runDB $ upsert (HangmanRanking uid (fromMaybe "" mnick) totalPP rank (totalMiss) (totalGuess) (pass) (pc)) 
          ( [ HangmanRankingTotalPP =. totalPP
            , HangmanRankingRank =. rank
            , HangmanRankingTotalMiss =. totalMiss
            , HangmanRankingTotalGuess =. totalGuess
            , HangmanRankingPassCount =. pass
            , HangmanRankingPlaycount =. pc
            ] <>
            [ HangmanRankingUserNickName =. nick | Just nick <- [mnick] ]
          )
  return (totalPP, rank, accPairs, (pass, pc))

-- the total pp it the maximal 50 consecutive weighted sum of the scores
computePP :: [Double] -> Double
computePP = maximum . map (sum . zipWith (*) [lambda^n | n<- [0..l]] . take l) . tails
  where lambda = 0.98
        l = 50

-- | get the ranking of the hangman game
getRanking :: Meow [HangmanRanking]
getRanking = fmap (fmap entityVal) . runDB $ selectList [] [Desc HangmanRankingTotalPP]

-- | get the ranking of a user, no update performed
getUserRank :: UserId -> Meow (Maybe (Double, Int))
getUserRank uid = fmap (fmap $ (\h -> (hangmanRankingTotalPP h, hangmanRankingRank h)) . entityVal) . runDB $ getBy (UniqueUserId uid)

updateAllRanking :: Meow ()
updateAllRanking = do
  allScores <- runDB $ selectList [] [Desc HangmanRecordId]
  let allUsers = S.toList . S.fromList $ map (hangmanRecordUserId . entityVal) allScores
  forM_ allUsers $ \uid -> do
    updateTotalPP uid Nothing
