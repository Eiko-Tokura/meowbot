{-# LANGUAGE TemplateHaskell, OverloadedStrings, ImpredicativeTypes, TransformListComp #-}
module Command
  ( BotCommand(..), CommandId(..)
  , actionAPI
  , queryAPI
  , doBotCommands
  , doBotAction
  , botCommandsToMeow
  , botCommandsWithIgnore
  , botMessageCounter
  , botT
  , restrictNumber
  , commandParserTransformByBotName
  , concatOutput
  ) where

import Control.Monad.Logger
import Control.Monad.State
import Data.Aeson
import Data.Bifunctor
import Data.HList
import Data.PersistModel
import Data.UpdateMaybe
import MeowBot.API
import MeowBot.BotStatistics
import MeowBot.BotStructure
import MeowBot.CommandRule
import MeowBot.IgnoreMatch
import MeowBot.Prelude
import MeowBot.Update
import Module
import Module.Async
import Module.AsyncInstance
import Network.WebSockets (Connection, sendTextData)
import System.General
import System.Meow
import Utils.ListComp
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified MeowBot.Parser as MP

commandParserTransformByBotName :: (MP.Chars sb, Monad m) => MP.Parser sb Char a -> MeowT r mods m (MP.Parser sb Char a)
commandParserTransformByBotName cp = do
  botname <- maybeBotName <$> query
  return $ case botname of
    Just bn -> MP.string bn >> MP.opt_ MP.commandSeparator >> cp
    Nothing -> cp
{-# INLINE commandParserTransformByBotName #-}

-- | A utility function to restrict the number of lines to show
restrictNumber :: Int -> [Text] -> [Text]
restrictNumber _ [] = ["什么也没找到 o.o"]
restrictNumber n xs =  [tshow i <> " " <> x | (i, x) <- zip [1 :: Int ..] $ take n xs]
                    <> ["(显示了前" <> tshow (min n (length xs)) <> "/" <> tshow (length xs) <> "条)" | length xs > n]

botT :: Monad m => MaybeT (MeowT r mods m) [a] -> MeowT r mods m [a]
botT = fmap (fromMaybe []) . runMaybeT

-- | Execute a BotAction, if it is a BAAsync, then put it into the asyncActions instead of waiting for it
doBotAction :: Connection -> BotAction -> Meow ()
doBotAction conn (BASendPrivate uid txt) = query >>= MeowT . lift . sendPrivate conn uid txt . Just . pack . show . message_number
doBotAction conn (BASendGroup gid txt)   = query >>= MeowT . lift . sendGroup   conn gid txt . Just . pack . show . message_number
doBotAction conn (BARetractMsg mid)      = MeowT . lift $ deleteMsg conn mid
doBotAction conn (BAActionAPI af)        = query >>= MeowT . lift . actionAPI conn . ActionForm af . Just . pack . show . message_number
doBotAction _    (BAAsync act)           = do
  $(logDebug) "BAAsync put into set"
  modify . first $ modifyF @AsyncModule (AsyncModuleL . S.insert act . asyncSet)
--change $ \other -> other { asyncActions   = S.insert act $ asyncActions other }
doBotAction conn (BAPureAsync pAct)      = doBotAction conn (BAAsync $ return <$> pAct)
doBotAction _    (BASimpleAction meow)   = meow
doBotAction _    (BAQueryAPI contMaybes)  = do
  tvarQueries <- askSystem @(TVar [(Int, WithTime (BL.ByteString -> Maybe (Meow [BotAction])) ) ])
  utcTime <- liftIO getCurrentTime
  liftIO . atomically $ do
    list <- readTVar tvarQueries
    let qid' = maximum (0 : map fst list) + 1
    modifyTVar' tvarQueries (<> [ (qid, WithTime utcTime contMaybe)
                                | contMaybe <- contMaybes
                                | qid <- [qid'..]
                                ])

-- | Low-level functions to send private messages
sendPrivate :: Connection -> UserId -> Text -> Maybe Text -> LoggingT IO ()
sendPrivate conn uid text mecho = do
  lift . sendTextData conn $ encode (ActionForm (SendPrivateMessage uid text Nothing) mecho)
  $(logInfo) $ T.concat ["-> user ", tshow uid, ": ", restrictLength 512 text]

-- | Low-level functions to send group messages
sendGroup :: Connection -> GroupId -> Text -> Maybe Text -> LoggingT IO ()
sendGroup conn gid text mecho = do
  lift . sendTextData conn $ encode (ActionForm (SendGroupMessage gid text Nothing) mecho)
  $(logInfo) $ T.concat ["-> group ", tshow gid, ": ", restrictLength 512 text]

-- | Low-level functions to delete messages
deleteMsg :: Connection -> MessageId -> LoggingT IO ()
deleteMsg conn mid = do
  lift . sendTextData conn $ encode (ActionForm (DeleteMessage mid) Nothing)
  $(logInfo) $ "=> Delete message: " <> tshow mid

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

-- | Extract the bot actions from a list of bot commands, checking the permissions
botCommandsToMeow :: [BotCommand] -> [Meow [BotAction]]
botCommandsToMeow = fmap permissionCheck

botMessageCounter :: forall a. CQMessage -> Meow [a]
botMessageCounter cqmsg = do
  let mCid = case eventType cqmsg of
        PrivateMessage -> PrivateChat <$> userId cqmsg
        GroupMessage   -> GroupChat <$> groupId cqmsg
        _              -> Nothing
  case mCid of
    Nothing -> return []
    Just cid -> logBotStatistics cid StatRecv >> return []

botCommandsWithIgnore :: CQMessage -> [BotCommand] -> Meow [BotAction]
botCommandsWithIgnore cqmsg bcs = do
  ignore <- isIgnoredMessage cqmsg
  if ignore
  then do
    $(logInfo) "Ignored, not triggering commands"
    return []
  else do
    updateSavedDataDB -- ^ make sure the saved data is up-to-date
    fmap concat . sequence $ botCommandsToMeow bcs

updateSavedDataDB :: Meow ()
updateSavedDataDB = do
  botid <- query
  inUserGroups   <- map entityVal <$> runDB (selectList [InUserGroupBotId ==. botid] [])
  inGroupGroups  <- map entityVal <$> runDB (selectList [InGroupGroupBotId ==. botid] [])
  commandRulesDB <- map entityVal <$> runDB (selectList [CommandRuleDBBotId ==. botid] [])
  let  
      userIds_userGroups   = [(inUserGroupUserId u, inUserGroupUserGroup u) | u <- inUserGroups]
      groupIds_groupGroups = [(inGroupGroupGroupId g, inGroupGroupGroupGroup g) | g <- inGroupGroups]
      commandRules         = [commandRuleDBCommandRule c | c <- commandRulesDB]
  change $ \od -> od { savedData = (savedData od)
                        { userGroups   = userIds_userGroups
                        , groupGroups  = groupIds_groupGroups
                        , commandRules = commandRules
                        }
                     }

-- | Concating the output of a command, grouping by the type of output (private/group) and mergeing the messages within each group using the first argument as intercalate text (default to "\n")
--
-- Warning: Drops BotAction that are not BASendGroup or BASendPrivate
concatOutput :: Monad m => Maybe Text -> m (NonEmpty (Maybe [BotAction])) -> m (Maybe [BotAction])
concatOutput intercalateText m = do
  outputsMaybe <- m
  let outputs = concat $ catMaybes $ NE.toList outputsMaybe
  if null outputs
  then return Nothing
  else
    let grouped = [ concatSend output
                  | output <- outputs
                  , let outputType = typeOutput output
                  , then group by outputType using groupWith
                  ] & catMaybes

        typeOutput (BASendGroup gid _)   = Just $ Right gid
        typeOutput (BASendPrivate uid _) = Just $ Left uid
        typeOutput _                     = Nothing

        concatSend []                              = Nothing
        concatSend (BASendGroup gid msgs : rest)   =
          case concatSend rest of
            Just (BASendGroup _ msgs') -> Just $ BASendGroup gid (msgs <> fromMaybe "\n" intercalateText <> msgs')
            _                          -> Just $ BASendGroup gid msgs
        concatSend (BASendPrivate uid msgs : rest) =
          case concatSend rest of
            Just (BASendPrivate _ msgs') -> Just $ BASendPrivate uid (msgs <> fromMaybe "\n" intercalateText <> msgs')
            _                            -> Just $ BASendPrivate uid msgs
        concatSend (_          : rest)             = concatSend rest
    in return $ Just grouped
