module MeowBot.MeowMsg.MeowBot where

import MeowBot.Prelude
import MeowBot.MeowMsg
import MeowBot.MeowMsg.CQHttp
import MeowBot.MeowMsg.ChatId
import MeowBot.MeowMsg.Discord
import Data.TypeList

data MeowBot
type MeowPlatforms = '[CQHttp, Discord]

instance Platform MeowBot where
  type MUserId    MeowBot = MeowUserIds  MeowPlatforms
  type MGroupId   MeowBot = MeowGroupIds MeowPlatforms
