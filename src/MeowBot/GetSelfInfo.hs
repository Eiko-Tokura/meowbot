module MeowBot.GetSelfInfo where

import MeowBot

getSelfInfo :: Meow (Maybe SelfInfo)
getSelfInfo = queries selfInfo
