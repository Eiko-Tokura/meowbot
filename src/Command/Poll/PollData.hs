{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeApplications, DerivingVia, DeriveAnyClass #-}
module Command.Poll.PollData where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Typeable
import Data.Additional
import MeowBot.Data

newtype PollId = PollId Int deriving (Show, Read, Num, Real, Enum, Integral, Eq, Ord, Typeable) via Int

instance IsAdditionalData PollId -- ^ this data need to be attached to a message

data PollData = PollData
  { pollId      :: PollId
  , pollEnv     :: Maybe ChatId -- ^ ChatId if the poll is in a group, Nothing if the poll is global
  , pollTitle   :: Text
  , pollOptions :: M.Map Int Text
  , pollVotes   :: M.Map UserId (S.Set Int)
  } deriving (Show, Read, Eq, Typeable, IsAdditionalData)

pollStatistics :: PollData -> [(Int, Text, Int)]
pollStatistics poll =
  let optionVotes = M.fromListWith (+)
        [ (optionId, 1)
        | optionId <- concatMap S.toList $ M.elems $ pollVotes poll ]
  in [ (optionId, option, M.findWithDefault 0 optionId optionVotes)
     | (optionId, option) <- M.toList $ pollOptions poll ]

instance IsAdditionalData (M.Map PollId PollData) -- ^ data is stored as a map

