module MeowBot.MeowMsg.ChatId where

import Data.TypeList
import MeowBot.MeowMsg

type family MGroupIds ps where
  MGroupIds '[]       = '[]
  MGroupIds (p ': ps) = MGroupId p ': MGroupIds ps

type family MUserIds ps where
  MUserIds '[]       = '[]
  MUserIds (p ': ps) = MUserId p ': MUserIds ps

-- | A group id is a sum type of all group id types of the platforms in the list.
type MeowGroupId ps = EList (MGroupIds ps)
-- | A user id is a sum type of all user id types of the platforms in the list.
type MeowUserId  ps = EList (MUserIds ps)
