{-# LANGUAGE OverloadedStrings, ImpredicativeTypes #-}
module Command
  ( BotCommand(..), CommandId(..)
  , doBotCommands
  , doBotAction
  , botT
  , restrictNumber
  , commandParserTransformByBotName
  ) where

import MeowBot
import MeowBot.CommandRule
import Data.Aeson (encode)
import qualified Data.Set as S
import qualified Data.Text as T
import Network.WebSockets (Connection, sendTextData)

import Module.Async
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import qualified MeowBot.Parser as MP
import MeowBot.Parser (tshow)

commandParserTransformByBotName :: (MP.Chars sb, Monad m) => MP.Parser sb Char a -> MeowT r mods m (MP.Parser sb Char a)
commandParserTransformByBotName cp = do
  botname <- maybeBotName <$> query
  return $ case botname of
    Just bn -> MP.string bn >> MP.opt_ MP.commandSeparator >> cp
    Nothing -> cp
{-# INLINE commandParserTransformByBotName #-}

restrictNumber :: Int -> [Text] -> [Text]
restrictNumber _ [] = ["什么也没找到 o.o"]
restrictNumber n xs =  [tshow i <> " " <> x | (i, x) <- zip [1 :: Int ..] $ take n xs]
                    <> ["(显示了前" <> tshow (min n (length xs)) <> "/" <> tshow (length xs) <> "条)" | length xs > n]

botT :: Monad m => MaybeT (MeowT r mods m) [a] -> MeowT r mods m [a]
botT = fmap (fromMaybe []) . runMaybeT

-- | Execute a BotAction, if it is a BAAsync, then put it into the asyncActions instead of waiting for it
doBotAction :: Connection -> BotAction -> Meow ()
doBotAction conn (BASendPrivate uid txt) = query >>= lift . sendPrivate conn uid txt . Just . pack . show . message_number
doBotAction conn (BASendGroup gid txt)   = query >>= lift . sendGroup   conn gid txt . Just . pack . show . message_number
doBotAction conn (BARetractMsg mid)      = lift $ deleteMsg conn mid
doBotAction _    (BAAsync act)      = modifyOneModuleAndState (Proxy @AsyncModule) $ S.insert act
--change $ \other -> other { asyncActions   = S.insert act $ asyncActions other }
doBotAction conn (BAPureAsync pAct) = doBotAction conn (BAAsync $ return <$> pAct)

-- | Low-level functions to send private messages
sendPrivate :: Connection -> UserId -> Text -> Maybe Text -> IO ()
sendPrivate conn uid text mecho = do
  sendTextData conn $ encode (SendMessageForm "send_private_msg" (PrivateParams uid text) mecho)
  putStrLn $ concat ["-> user ", show uid, ": ", T.unpack text]

-- | Low-level functions to send group messages
sendGroup :: Connection -> GroupId -> Text -> Maybe Text -> IO ()
sendGroup conn gid text mecho = do
  sendTextData conn $ encode (SendMessageForm "send_group_msg" (GroupParams gid text) mecho)
  putStrLn $ concat ["-> group ", show gid, ": ", T.unpack text]

-- | Low-level functions to delete messages
deleteMsg :: Connection -> MessageId -> IO ()
deleteMsg conn mid = do
  sendTextData conn $ encode (SendMessageForm "delete_msg" (DeleteParams mid) Nothing)
  putStrLn $ "=> Delete message: " ++ show mid

-- | Check if the command is allowed, and execute it if it is
permissionCheck :: BotCommand -> Meow [BotAction]
permissionCheck botCommand = botT $ do
  (_, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  other <- lift query
  let sd = savedData other
  if checkCommandRule sd (identifier botCommand) cid uid
  then lift $ command botCommand
  else return []
  where
    checkCommandRule :: SavedData -> CommandId -> ChatId -> UserId -> Bool
    checkCommandRule sd cmdId cid uid = allowedAtLeastOnce && notDenied
      where
        allowedAtLeastOnce = not $ null $
          [() | Allow uobj cobj <- rules
              , inUserObject ugs uid uobj
              , cmdId `inCommandObject` cobj
          ]
          ++ case cid of
            PrivateChat _ -> []
            GroupChat gid -> [()| Allow uobj cobj <- rules
                                , gInUserObject ggs gid uobj
                                , cmdId `inCommandObject` cobj
                             ]
        notDenied = null $
          [() | Deny uobj cobj <- rules
              , inUserObject ugs uid uobj
              , cmdId `inCommandObject` cobj
          ]
          ++ case cid of
            PrivateChat _ -> []
            GroupChat gid -> [()| Deny uobj cobj <- rules
                                , gInUserObject ggs gid uobj
                                , cmdId `inCommandObject` cobj
                             ]
        rules = commandRules sd
        ugs = userGroups sd
        ggs = groupGroups sd

-- | Input all data, all commands, do the commands that is required by the input, then return updated data
-- if there are any async bot actions, put them into the asyncActions instead of waiting for them
doBotCommands ::  Connection -> [BotCommand] -> Cat ()
doBotCommands conn commands = globalizeMeow $ do
  actions <- permissionCheck `mapM` commands
  doBotAction conn `mapM_` concat actions
