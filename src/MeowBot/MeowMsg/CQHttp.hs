module MeowBot.MeowMsg.CQHttp where

import MeowBot.Prelude
import MeowBot.MeowMsg
import MeowBot.Data.ChatId
import MeowBot.MetaMessage
import MeowBot.Data.CQHttp.CQCode

data CQHttp
newtype QQ a = QQ { unQQ :: a }
  deriving newtype (Eq, Ord, Generic, NFData)
  deriving Functor

instance Platform CQHttp where
  type MChat   CQHttp = QQ ChatId
  type MUserId CQHttp = QQ UserId
  type MMsgId  CQHttp = CQMessageId

  type MMsgInline CQHttp = Either Text CQCode
  type MAt        CQHttp = QQ UserId
  type MFace      CQHttp = Int
