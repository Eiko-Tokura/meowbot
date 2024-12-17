module Command.Hangman.Ranking where

import Utils.RunDB
import Data.PersistModel
import System.Meow
import Data.Maybe
import Data.List (tails)
import MeowBot.Data

data ViewRanking = ViewPersonalRanking | ViewGlobalRanking

updateTotalPP
  :: UserId             -- ^ user id
  -> Text               -- ^ user nickname
  -> Meow (Double, Int) -- ^ returns total pp and rank
updateTotalPP uid nick = do
  listScores <- runDB $ selectList [HangmanRecordUserId ==. uid] [Desc HangmanRecordId]
  let totalPP = computePP (fromMaybe 0 . hangmanRecordScore . entityVal <$> listScores)
  rank <- fmap (+1) . runDB $ count [HangmanRankingTotalPP >. totalPP, HangmanRankingUserId !=. uid]
  -- insert or update the ranking
  -- if the user is not in the ranking, insert it
  -- if the user is in the ranking, update it
  runDB $ upsert (HangmanRanking uid nick totalPP rank) [HangmanRankingTotalPP =. totalPP, HangmanRankingRank =. rank]
  return (totalPP, rank)

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
