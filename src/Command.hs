{-# LANGUAGE OverloadedStrings, ImpredicativeTypes #-}
module Command
  ( BotCommand(..), CommandId(..)
  , doBotCommands
  , doBotAction
  , botT
  , restrictNumber
  , commandParserTransformByBotName
  ) where

import MeowBot.BotStructure
import MeowBot.CommandRule
import Data.Aeson (encode)
import qualified Data.Set as S
import qualified Data.Text as T
import Network.WebSockets (Connection, sendTextData)

import Control.Monad.Trans
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Trans.ReaderState as RS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import qualified MeowBot.Parser as MP
import MeowBot.Parser (tshow)

commandParserTransformByBotName :: (MP.Chars sb, Monad m) => MP.Parser sb Char a -> MeowT m (MP.Parser sb Char a)
commandParserTransformByBotName cp = do
  botname <- nameOfBot . botModules <$> get
  return $ case botname of
    Just bn -> MP.string bn >> MP.opt_ MP.commandSeparator >> cp
    Nothing -> cp
{-# INLINE commandParserTransformByBotName #-}

restrictNumber :: Int -> [Text] -> [Text]
restrictNumber _ [] = ["什么也没找到 o.o"]
restrictNumber n xs =  [tshow i <> " " <> x | (i, x) <- zip [1 :: Int ..] $ take n xs]
                    <> ["(显示了前" <> tshow (min n (length xs)) <> "/" <> tshow (length xs) <> "条)" | length xs > n]

botT :: Monad m => MaybeT (MeowT m) [a] -> MeowT m [a]
botT = fmap (fromMaybe []) . runMaybeT

-- | Execute a BotAction, if it is a BAAsync, then put it into the asyncActions instead of waiting for it
doBotAction :: Connection -> BotAction -> Meow ()
doBotAction conn (BASendPrivate uid txt) = RS.get >>= lift . sendPrivate conn uid txt . Just . pack . show . message_number
doBotAction conn (BASendGroup gid txt)   = RS.get >>= lift . sendGroup   conn gid txt . Just . pack . show . message_number
doBotAction conn (BARetractMsg mid)      = lift $ deleteMsg conn mid
doBotAction _    (BAAsync act)      = RS.modify $ \other -> other { asyncActions   = S.insert act $ asyncActions other }
doBotAction conn (BAPureAsync pAct) = doBotAction conn (BAAsync $ return <$> pAct)

sendPrivate :: Connection -> UserId -> Text -> Maybe Text -> IO ()
sendPrivate conn uid text mecho = do
  sendTextData conn $ encode (SendMessageForm "send_private_msg" (PrivateParams uid text) mecho)
  putStrLn $ concat ["-> user ", show uid, ": ", T.unpack text]

sendGroup :: Connection -> GroupId -> Text -> Maybe Text -> IO ()
sendGroup conn gid text mecho = do
  sendTextData conn $ encode (SendMessageForm "send_group_msg" (GroupParams gid text) mecho)
  putStrLn $ concat ["-> group ", show gid, ": ", T.unpack text]

deleteMsg :: Connection -> MessageId -> IO ()
deleteMsg conn mid = do
  sendTextData conn $ encode (SendMessageForm "delete_msg" (DeleteParams mid) Nothing)
  putStrLn $ "=> Delete message: " ++ show mid

permissionCheck :: BotCommand -> CommandValue
permissionCheck botCommand = botT $ do
  (_, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> readable
  other <- lift RS.get
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
doBotCommands ::  Connection -> [BotCommand] -> StateT AllData IO ()
doBotCommands conn commands = globalize (\a -> (wholechat a, botConfig a)) otherdata (uncurry AllData) $ runMeowT $ do
  actions <- permissionCheck `mapM` commands
  doBotAction conn `mapM_` concat actions
