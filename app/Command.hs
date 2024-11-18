{-# LANGUAGE OverloadedStrings, ImpredicativeTypes #-}
module Command 
  ( BotCommand(..), CommandId(..)
  , doBotCommands
  , botT
  , restrictNumber
  , commandParserTransformByBotName
  ) where

import MeowBot.BotStructure
import MeowBot.CommandRule
import Data.Text (Text, pack)
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.WebSockets (Connection, sendTextData)

import Control.Monad.Trans
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Trans.ReaderState as RS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import qualified MeowBot.Parser as MP
import MeowBot.Parser (tshow)

commandParserTransformByBotName :: (MP.Chars sb, Monad m) => MP.Parser sb Char a -> ReaderStateT WholeChat OtherData m (MP.Parser sb Char a)
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

botT :: Monad m => MaybeT (ReaderStateT WholeChat OtherData m) [a] -> ReaderStateT WholeChat OtherData m [a]
botT = fmap (fromMaybe []) . runMaybeT

doBotAction :: Connection -> BotAction -> ReaderStateT WholeChat OtherData IO ()
doBotAction conn (BASendPrivate uid txt) = RS.get >>= lift . sendPrivate conn uid txt . Just . pack . show . message_number 
doBotAction conn (BASendGroup gid txt)  = RS.get >>= lift . sendGroup   conn gid txt . Just . pack . show . message_number
doBotAction conn (BARetractMsg mid)  = lift $ deleteMsg conn mid

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
  (_, cid, uid, _) <- MaybeT $ getEssentialContent <$> ask
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
doBotCommands ::  Connection -> [BotCommand] -> StateT AllData IO () 
doBotCommands conn commands = globalize wholechat otherdata AllData $ do
  actions <- permissionCheck `mapM` commands
  doBotAction conn `mapM_` concat actions
