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

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Parallel.Strategies as PS
import Control.Monad.Logger
import Control.Monad.RS.Class
import Control.Monad.Effect
import Data.Aeson
import Data.PersistModel
import Data.UpdateMaybe
import MeowBot.API
import MeowBot.BotStatistics
import MeowBot.BotStructure
import MeowBot.CommandRule
import MeowBot.IgnoreMatch
import MeowBot.Prelude
import Module.MeowConnection
import Module.Logging
import Module.Async
import Module.AsyncInstance
import System.Meow
import Utils.ListComp
import Utils.RunDB
import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified MeowBot.Parser       as MP

commandParserTransformByBotName :: (MP.Chars sb, Monad m, MeowAllData' m mods) => MP.Parser sb Char a -> MeowT mods m (MP.Parser sb Char a)
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

botT :: Monad m => MaybeT (MeowT mods m) [a] -> MeowT mods m [a]
botT = fmap (fromMaybe []) . runMaybeT

-- | Execute a BotAction, if it is a BAAsync, then put it into the asyncActions instead of waiting for it
doBotAction :: BotAction -> Meow ()
doBotAction (BASendPrivate uid txt) = query >>= sendPrivate uid txt . Just . pack . show . message_number
doBotAction (BASendGroup gid txt)   = query >>= sendGroup   gid txt . Just . pack . show . message_number
doBotAction (BARetractMsg mid)      = deleteMsg mid
doBotAction (BAActionAPI af)        = query >>= actionAPI . ActionForm af . Just . pack . show . message_number
doBotAction (BAAsync act)           = do
  $(logDebug) "BAAsync put into set"
  modifyModule @AsyncModule (AsyncState . S.insert act . asyncSet)
--modify $ \other -> other { asyncActions   = S.insert act $ asyncActions other }
doBotAction (BAPureAsync pAct)      = doBotAction (BAAsync $ return <$> pAct)
doBotAction (BASimpleAction meow)   = meow
doBotAction (BAQueryAPI (SomeQueryAPI query cont)) = do
  cont' <- queryAPI query
  doBotAction (BARawQueryCallBack $ pure $ fmap cont . cont')
doBotAction (BARawQueryCallBack contMaybes) = do
  tvarQueries <- asksModule meowReadsQueries
  utcTime <- liftIO getCurrentTime
  liftIO . atomically $ do
    list <- readTVar tvarQueries
    let qid' = maximum (0 : map fst list) + 1
    modifyTVar tvarQueries (<> [ (qid, WithTime utcTime contMaybe)
                               | contMaybe <- contMaybes
                               | qid <- [qid'..]
                               ])
doBotAction (BADelayedAction ms meow) = do
  waitMeow <- liftIO $ async $ do
    threadDelay (ms * 1000) -- milliseconds to microseconds
    return meow
  doBotAction (BAAsync waitMeow)
doBotAction (BADelayedPureAction ms acts) = doBotAction (BADelayedAction ms (return acts))
doBotAction (BADelayedPureAction1 ms act) = doBotAction (BADelayedPureAction ms [act])

-- | Low-level functions to send private messages
sendPrivate :: (In MeowConnection mods, In LoggingModule mods, InList (ErrorText "send_connection") es)
  => UserId -> Text -> Maybe Text -> EffT mods es IO ()
sendPrivate uid text mecho = do
  sendTextData $ encode (ActionForm (SendPrivateMessage uid text Nothing) mecho)
  $(logInfo) $ T.concat ["-> user ", tshow uid, ": ", restrictLength 512 text]

-- | Low-level functions to send group messages
sendGroup :: (In MeowConnection mods, In LoggingModule mods, InList (ErrorText "send_connection") es)
  => GroupId -> Text -> Maybe Text -> EffT mods es IO ()
sendGroup gid text mecho = do
  sendTextData $ encode (ActionForm (SendGroupMessage gid text Nothing) mecho)
  $(logInfo) $ T.concat ["-> group ", tshow gid, ": ", restrictLength 512 text]

-- | Low-level functions to delete messages
deleteMsg :: (MeowSend mods es, In LoggingModule mods) => CQMessageId -> EffT mods es IO ()
deleteMsg mid = do
  sendTextData $ encode (ActionForm (DeleteMessage mid) Nothing)
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
doBotCommands :: [BotCommand] -> Meow ()
doBotCommands commands = do
  actions <- permissionCheck `mapM` commands
  doBotAction `mapM_` concat actions

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
    -- updateSavedDataDB -- ^ make sure the saved data is up-to-date
    fmap concat . sequence $ botCommandsToMeow bcs

_updateSavedDataDB :: Meow ()
_updateSavedDataDB = do
  botid <- query
  inUserGroups   <- map entityVal <$> runMeowCoreDB (selectList [InUserGroupBotId   ==. botid] [])
  inGroupGroups  <- map entityVal <$> runMeowCoreDB (selectList [InGroupGroupBotId  ==. botid] [])
  commandRulesDB <- map entityVal <$> runMeowCoreDB (selectList [CommandRuleDBBotId ==. botid] [])
  let
      userIds_userGroups   = [(inUserGroupUserId u, inUserGroupUserGroup u) | u <- inUserGroups]
      groupIds_groupGroups = [(inGroupGroupGroupId g, inGroupGroupGroupGroup g) | g <- inGroupGroups]
      commandRules         = [commandRuleDBCommandRule c | c <- commandRulesDB]
  modify $ \od -> od { savedData = (savedData od)
                        { userGroups   = userIds_userGroups   `PS.using` PS.rdeepseq
                        , groupGroups  = groupIds_groupGroups `PS.using` PS.rdeepseq
                        , commandRules = commandRules         `PS.using` PS.rdeepseq
                        } `PS.using` rseqSavedData
                     }

-- | Concating the output of a command, grouping by the type of output (private/group) and mergeing the messages within each group using the first argument as intercalate text (default to "\n")
--
-- Warning: Drops BotAction items that are not BASendGroup or BASendPrivate
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
