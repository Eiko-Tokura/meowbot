module MeowBot.IgnoreMatch where

import Control.Monad.Readable
import Data.Maybe
import Data.PersistModel
import Database.Persist
import MeowBot.CQCode
import MeowBot.Data
import MeowBot.Data.IgnoreMatchType
import System.Meow
import Utils.RunDB
import qualified Data.Text as T

matchIgnoredMessage :: CQMessage -> BotIgnore -> Bool
matchIgnoredMessage cqmsg bot_ignore@BotIgnore{ botIgnoreMatchType = IgnoreExact } =
  let hasAt = fromMaybe False $ (\l -> not . null $ [() | Left (CQAt {}) <- l]) . mixedMessage <$> metaMessage cqmsg
      pureMsg = fromMaybe ""  $ (\l -> T.concat [t | Right t <- l])             . mixedMessage <$> metaMessage cqmsg
  in  and
        $  [ fromMaybe True $ (== hasAt)   <$> botIgnoreAtPattern bot_ignore
           , fromMaybe True $ (== pureMsg) <$> botIgnoreString    bot_ignore
           ]
        ++ [eventType cqmsg == PrivateMessage | fromMaybe False $ botIgnorePrivate bot_ignore]
        ++ [eventType cqmsg == GroupMessage   | fromMaybe False $ botIgnoreGroup   bot_ignore]
matchIgnoredMessage cqmsg bot_ignore@BotIgnore { botIgnoreMatchType = IgnorePrefix } =
  let hasAt = fromMaybe False $ (\l -> not . null $ [() | Left (CQAt {}) <- l]) . mixedMessage <$> metaMessage cqmsg
      pureMsg = fromMaybe ""  $ (\l -> T.concat [t | Right t <- l])             . mixedMessage <$> metaMessage cqmsg
  in  and
        $  [ fromMaybe True $ (== hasAt)               <$> botIgnoreAtPattern bot_ignore
           , fromMaybe True $ (`T.isPrefixOf` pureMsg) <$> botIgnoreString    bot_ignore
           ]
        ++ [eventType cqmsg == PrivateMessage | fromMaybe False $ botIgnorePrivate bot_ignore]
        ++ [eventType cqmsg == GroupMessage   | fromMaybe False $ botIgnoreGroup   bot_ignore]

isIgnoredMessage :: CQMessage -> Meow Bool
isIgnoredMessage cqmsg = do
  botId <- query
  botIgnores <- fmap (map entityVal) $ runDB $ selectList [BotIgnoreBotId ==. botId] []
  return $ any (matchIgnoredMessage cqmsg) botIgnores
