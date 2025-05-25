module MeowBot.CronTab.CronMeowAction where

import MeowBot.BotStructure
import Utils.Persist

-- | This should be a storable data that describes what to do
-- you cannot put any monadic value here
data CronMeowAction
  = CronMeowChatBack
    { cronMeowActionChatId  :: ChatId
    , cronMeowActionMessage :: Text
    }
  deriving (Eq, Show, Read)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CronMeowAction)

instance ToText CronMeowAction Text where
  toText (CronMeowChatBack chatId message) =
    "CronMeowChatBack { chatId = " <> toText chatId <> ", message = " <> message <> " }"
