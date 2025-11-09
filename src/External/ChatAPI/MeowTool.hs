{-# LANGUAGE UndecidableInstances, TransformListComp #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module External.ChatAPI.MeowTool where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Trans.Maybe
import Data.PersistModel
import Data.Time.Clock
import Control.Concurrent.STM
import External.ChatAPI.MeowToolEnv
import External.ChatAPI.Tool
import MeowBot.BotStructure
import MeowBot.Action
import Module.LogDatabase
import Module.MeowTypes
import System.Meow
import Utils.RunDB hiding (In)
import Utils.Persist
import MeowBot.CronTab.CronMeowAction
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.Esqueleto.Experimental as E
import Cron.Parser (validateCronText)
import Utils.ListComp

data NoteToolRead
data NoteToolAdd
data NoteToolReplace
data NoteToolDelete

instance (MeowAllData mods, MeowDatabase `In` mods, LogDatabase `In` mods) => ToolClass (MeowToolEnv mods) NoteToolRead where
  type ToolInput NoteToolRead = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the note to read"])
  type ToolOutput NoteToolRead = ParamToData (ObjectP0 --"note" "retrived note"
    '[ IntP "note_id" "note_id of the note"
     , StringP "title" "the note title"
     , StringP "content" "the note content"
     , StringP "time" "the time the note was last modified"
     ])
  data ToolError NoteToolRead = NoteReadError Text deriving Show
  toolEnabled _        = computeSettingFromDB botSettingEnableNotes botSettingPerChatEnableNotes
  toolName _ _ = "note_read"
  toolDescription _ _ = "Get note content by note_id"
  toolHandler _ _ ((IntT note_id) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid  <- baseMaybeInWith (NoteReadError "no cid found") getCid
    note <- lift $ fmap (fmap entityVal) . runMeowDBMeowTool $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id] []
    case note of
      Just (AssistantNote _ _ _ title content time) -> return $ IntT note_id :%* StringT title :%* StringT content :%* StringT (toText time) :%* ObjT0Nil
      Nothing -> effThrow $ NoteReadError "Note not found"

instance (MeowAllData mods, MeowDatabase `In` mods, LogDatabase `In` mods) => ToolClass (MeowToolEnv mods) NoteToolAdd where
  type ToolInput NoteToolAdd = ParamToData (ObjectP0 --"note" "the note to add"
    '[ StringP "title" "the note title should be informative"
     , StringP "content" "the note content, you can include more detailed information here"
     ])
  type ToolOutput NoteToolAdd = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the added note"])
  data ToolError NoteToolAdd = NoteAddError Text deriving Show
  toolEnabled _        = computeSettingFromDB botSettingEnableNotes botSettingPerChatEnableNotes
  toolName _ _ = "note_add"
  toolDescription _ _ = "Add a note, You can use the note tools to take notes if you want to memorize things that people teach you, or you learn about the people more. The title will be added to your system prompt which becomes part of your memory."
  toolHandler _ _ ((StringT title) :%* (StringT content) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid <- baseMaybeInWith (NoteAddError "no cid found") getCid
    time <- liftIO getCurrentTime
    maxNoteId <- lift
      $ fmap (maybe 0 (assistantNoteNoteId . entityVal)) . runMeowDBMeowTool
      $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid] [Desc AssistantNoteNoteId, LimitTo 1]
    let newNoteId = maxNoteId + 1
    lift $ runMeowDBMeowTool $ insert $ AssistantNote
      { assistantNoteBotName = botname
      , assistantNoteChatId = cid
      , assistantNoteNoteId = newNoteId
      , assistantNoteTitle = title
      , assistantNoteContent = content
      , assistantNoteTime = time
      }
    return $ IntT newNoteId :%* ObjT0Nil

instance (MeowAllData mods, MeowDatabase `In` mods, LogDatabase `In` mods)
  => ToolClass (MeowToolEnv mods) NoteToolReplace where
  type ToolInput NoteToolReplace = ParamToData (ObjectP0 --"note" "the note to replace"
    '[ IntP "note_id" "note_id of the note to replace"
     , StringP "title" "the note title"
     , StringP "content" "the note content"
     ])
  type ToolOutput NoteToolReplace = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the replaced note"])
  data ToolError NoteToolReplace = NoteReplaceError Text deriving Show
  toolEnabled _        = computeSettingFromDB botSettingEnableNotes botSettingPerChatEnableNotes
  toolName _ _ = "note_replace"
  toolDescription _ _ = "Replace a note"
  toolHandler _ _ ((IntT note_id) :%* (StringT title) :%* (StringT content) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid <- baseMaybeInWith (NoteReplaceError "no cid found") getCid
    time <- liftIO getCurrentTime
    note <- lift $ fmap (fmap entityVal) . runMeowDBMeowTool $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id] []
    case note of
      Just _ -> lift $ runMeowDBMeowTool $ updateWhere
        [ AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id ]
        [ AssistantNoteTitle =. title
        , AssistantNoteContent =. content
        , AssistantNoteTime =. time
        ]
      Nothing -> effThrow $ NoteReplaceError "Note not found"
    return $ IntT note_id :%* ObjT0Nil

instance (MeowAllData mods, MeowDatabase `In` mods, LogDatabase `In` mods)
  => ToolClass (MeowToolEnv mods) NoteToolDelete where
  type ToolInput NoteToolDelete  = ParamToData (ObjectP0 '[ArrayPInt "note_id" "array of note_id to delete"])
  type ToolOutput NoteToolDelete = ParamToData (ObjectP0 '[StringP "result" "the result of the deletion"])
  data ToolError NoteToolDelete = NoteDeleteError Text deriving Show
  toolEnabled _ = computeSettingFromDB botSettingEnableNotes botSettingPerChatEnableNotes
  toolName _ _  = "note_delete"
  toolDescription _ _ = "Delete one or more note"
  toolHandler _ _ ((ArrayT note_ids) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid <- baseMaybeInWith (NoteDeleteError "no cid found") getCid
    note <- lift $ fmap (fmap entityVal) . runMeowDBMeowTool $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId <-. note_ids] []
    case note of
      Just _ -> lift $ runMeowDBMeowTool $ deleteWhere [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId <-. note_ids]
      Nothing -> effThrow $ NoteDeleteError "Note not found"
    return $ StringT "success" :%* ObjT0Nil

listNoteTitleAndContents :: BotName -> ChatId -> Meow [(Int, (Text, Text))]
listNoteTitleAndContents botname cid
  = fmap (fmap (\(Entity _ note) -> (assistantNoteNoteId note, (assistantNoteTitle note, assistantNoteContent note))))
  . runMeowDB $ selectList [AssistantNoteBotName ==. maybeBotName botname, AssistantNoteChatId ==. cid] []

-- | Get the note listing, including the note_id, title and content (truncated to 20 characters)
getNoteListing :: BotName -> ChatId -> Meow (Maybe Text)
getNoteListing bn cid = do
  noteTitles <- listNoteTitleAndContents bn cid
  return $ case noteTitles of
    [] -> Nothing
    xs -> Just $ T.unlines $
      [ T.concat
        [ "id="
        , toText note_id
        , " "
        , title
        , " :"
        , T.take 40 content
        , if T.length content > 40 then "...(truncated)" else ""
        ]
      | (note_id, (title, content)) <- xs
      ]


-- | The tool to send like or send poke
data ActionTool
-- HasSystemRead (TVar [Meow [BotAction]])
instance (In MeowActionQueue mods, MeowAllData mods)
  => ToolClass (MeowToolEnv mods) ActionTool where
  type ToolInput ActionTool = ParamToData
    (ObjectP0
      [ StringP "action" "action can be 'like' or 'poke'"
      , ArrayPInt "user_list" "list of user_id of target user(s)"
      ]
    )
  type ToolOutput ActionTool = ParamToData (ObjectP0 '[StringP "result" "the result of the action"])
  data ToolError ActionTool = ActionError Text deriving Show
  toolName _ _ = "action"
  toolDescription _ _ =  "Send a like (点赞) or a poke (戳一戳) to one or multiple user. Example Output : "
                      <> "{\"tool\": \"action\", \"args\": {\"action\": \"like\", \"user_list\": [<user_id>]}}"
  toolHandler _ _ ((StringT act) :%* (ArrayT user_ids) :%* ObjT0Nil) = do
    tvarBotAction <- lift $ asksModule meowReadsAction
    cid <- baseMaybeInWith (ActionError "no ChatId found") getCid
    action <- case act of
      "like" -> liftIO $ baSequenceDelayFullAsync intercalateDelay
        [ BAActionAPI (SendLike (UserId user_id) 10)   | user_id <- user_ids ]
      "poke" -> liftIO $ baSequenceDelayFullAsync intercalateDelay
        [ BAActionAPI (SendPoke (UserId user_id) cid) | user_id <- user_ids ]
      _ -> effThrow $ ActionError "action can only be 'like' or 'poke'"
    liftIO $ atomically $ modifyTVar tvarBotAction (<> [return action])
    return $ StringT "success" :%* ObjT0Nil
    where intercalateDelay = 2_000_000 -- 2 second

data CronTabTool

instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) CronTabTool where
  type ToolInput CronTabTool = ParamToData
    (ObjectP0
      [ StringP "crontab" "crontab format schedule in UTC, for example '10 0 * * *' means trigger at every 0:10 UTC"
      , IntP "repeat" "number of times to trigger, 1 means one-off, 0 means repeat indefinitely"
      , StringP "detail" "informative description of what exactly you need to do when the scheduled time comes, the information will be displayed to you. Include contexts you will need like user_id of the user you need to inform."
      ]
    )
  type ToolOutput CronTabTool = ParamToData (ObjectP0 '[StringP "result" "the result of the tool"])
  data ToolError CronTabTool = TimedTaskToolError Text deriving Show
  toolEnabled _        = computeSettingFromDB botSettingEnableCronTab botSettingPerChatEnableCronTab
  toolName _ _ = "crontab"
  toolDescription _ _ =  "Set a cron job to trigger a chat after a certain time. Example Output : "
                      <> "{\"tool\": \"crontab\", \"args\": {\"crontab\": <crontab format>, \"repeat\": <repeat>, \"detail\": <detailed description>}}"
  toolHandler _ _ ((StringT unVerifiedCronText) :%* (IntT repeat) :%* (StringT desc) :%* ObjT0Nil) = do
    botId   <- lift getBotId
    botname <- lift getBotName
    cid <- baseMaybeInWith (TimedTaskToolError  "no ChatId found") getCid
    cronText <- pureEitherInWith (\t -> TimedTaskToolError $ "invalid crontab format: " <> toText t) $ validateCronText unVerifiedCronText
    cronId <- lift $ runMeowDBMeowTool $ insert $ BotCronJob
      { botCronJobBotName          = botname
      , botCronJobBotId            = botId
      , botCronJobChatId           = Just cid
      , botCronJobCronSchedule     = cronText
      , botCronJobCronRepeatFinite = if repeat == 0 then Nothing else Just repeat
      , botCronJobCronMeowAction   = CronMeowChatBack cid desc
      , botCronJobCronDetail       = Just desc
      }
    return $ StringT (tshow cronId) :%* ObjT0Nil

data CronTabList
-- ^ List all cron jobs for the current bot in the current chat.
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) CronTabList where
  type ToolInput CronTabList = ParamToData (ObjectP0 '[])
  type ToolOutput CronTabList = ParamToData (ObjectP0 '[StringP "cron_jobs" "list of cron jobs"])
  data ToolError CronTabList = CronTabListError Text deriving Show
  toolEnabled _        = computeSettingFromDB botSettingEnableCronTab botSettingPerChatEnableCronTab
  toolName _ _ = "crontab_list"
  toolDescription _ _ = "List all cron jobs for the current chat."
  toolHandler _ _ ObjT0Nil = do
    botId   <- lift getBotId
    cid <- baseMaybeInWith (CronTabListError "no ChatId found") getCid
    cronJobs <- lift $ runMeowDBMeowTool $ selectList [BotCronJobBotId ==. botId, BotCronJobChatId ==. Just cid] []
    let cronJobsJson = map cronTabDisplayText cronJobs
    return $ StringT (T.intercalate "\n" cronJobsJson) :%* ObjT0Nil

data CronTabDelete
-- ^ Delete a cron job by its ID.
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) CronTabDelete where
  type ToolInput CronTabDelete = ParamToData (ObjectP0 '[ArrayPInt "cron_id" "list of cron job IDs to delete"])
  type ToolOutput CronTabDelete = ParamToData (ObjectP0 '[StringP "result" "the result of the deletion"])
  data ToolError CronTabDelete = CronTabDeleteError Text deriving Show
  toolEnabled _        = computeSettingFromDB botSettingEnableCronTab botSettingPerChatEnableCronTab
  toolName _ _ = "crontab_delete"
  toolDescription _ _ = "Delete one or more cron jobs by their ID. Use the 'crontab_list' tool to get the cron job list and IDs before using this tool."
  toolHandler _ _ ((ArrayT cronIds) :%* ObjT0Nil) = do
    botId <- lift getBotId
    cid <- baseMaybeInWith (CronTabDeleteError "no ChatId found") getCid
    lift $ runMeowDBMeowTool $ deleteWhere [BotCronJobBotId ==. botId, BotCronJobChatId ==. Just cid, BotCronJobId <-. map intToKey cronIds]
    return $ StringT "success" :%* ObjT0Nil

data SetEssenceMessage
-- ^ Set a very good message as essence (only valid in group chats).
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) SetEssenceMessage where
  type ToolInput SetEssenceMessage = ParamToData (ObjectP0 '[IntP "message_id" "the message_id of the message to set as essence"])
  type ToolOutput SetEssenceMessage = ParamToData (ObjectP0 '[StringP "result" "the result of the action"])
  data ToolError SetEssenceMessage = EssenceError Text deriving Show
  enabledByDefault _ _ = True
  toolEnabled _        = computeSettingFromDB botSettingEnableSetEssence botSettingPerChatEnableSetEssence
  toolUsable _         = meowGroupAdmin

  toolName _ _ = "set_essence"
  toolDescription _ _ = "Set a message as essence. 当群友发表了非常棒的见解你认为可以为全群员收藏时使用. The message will be pinned to a Highlight list."
  toolHandler _ _ ((IntT messageId) :%* ObjT0Nil) = do
    _ <- baseMaybeInWith (EssenceError "This tool is only valid in group chats, this is a private chat.") getGid
    action <- liftIO $ baSequenceDelayFullAsync intercalateDelay [BAActionAPI (SetEssenceMessage messageId)]
    tvarBotAction <- lift $ asksModule meowReadsAction
    liftIO $ atomically $ modifyTVar tvarBotAction (<> [return action])
    return $ StringT "success" :%* ObjT0Nil
    where intercalateDelay = 2_000_000 -- 2 second

data DeleteMessageTool
-- ^ A tool that provides the ability to delete a message in the current chat. (Only for group chats where the bot is an admin)
-- This tool is useful for moderating the chat and preventing unwanted interactions.
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) DeleteMessageTool where
  type ToolInput DeleteMessageTool = ParamToData (ObjectP0 '[IntP "message_id" "the message_id of the message to delete"])
  type ToolOutput DeleteMessageTool = ParamToData (ObjectP0 '[StringP "result" "the result of the deletion action"])
  data ToolError DeleteMessageTool = DeleteError Text deriving Show
  enabledByDefault _ _ = True
  toolEnabled _        = computeSettingFromDB botSettingEnableDeleteMessage botSettingPerChatEnableDeleteMessage
  toolUsable _         = meowGroupAdmin
  toolName _ _ = "delete_message"
  toolDescription _ _ = "As admin, delete a message (撤回消息) in the current group chat. Use with extra caution, can be used to prevent abuse or very bad behavior."
  toolHandler _ _ ((IntT messageId) :%* ObjT0Nil) = do
    _ <- baseMaybeInWith (DeleteError "This tool is only valid in group chats, this is a private chat.") getGid
    action <- liftIO $ baSequenceDelayFullAsync intercalateDelay [BAActionAPI (DeleteMessage messageId)]
    tvarBotAction <- lift $ asksModule meowReadsAction
    liftIO $ atomically $ modifyTVar tvarBotAction (<> [return action])
    return $ StringT "success" :%* ObjT0Nil
    where intercalateDelay = 2_000_000 -- 2 second

data SetGroupBanTool
-- ^ A tool that provides the ability to ban a user in the current chat. (Only for group chats where the bot is an admin)
-- This tool is useful for moderating the chat and preventing unwanted interactions.
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) SetGroupBanTool where
  type ToolInput SetGroupBanTool  = ParamToData (ObjectP0
      '[ IntP "user_id" "the user_id of the user to ban, 0 means ban everyone (except admins)"
       , IntP "duration" "the duration in seconds to ban the user, 0 means cancel the ban. When banning everyone, duration can only be 0 (stop ban) or 1 (enable ban)."
       ]
    )
  type ToolOutput SetGroupBanTool = ParamToData (ObjectP0 '[StringP "result" "the result of the ban action"])
  data ToolError SetGroupBanTool = BanError Text deriving Show
  enabledByDefault _ _ = True
  toolEnabled _        = computeSettingFromDB botSettingEnableSetGroupBan botSettingPerChatEnableSetGroupBan
  toolUsable _         = meowGroupAdmin

  toolName _ _ = "ban"
  toolDescription _ _ = "Ban a non-admin user in the current chat for a given period. Use with extra caution, can be used to prevent abuse or very bad behavior."
  toolHandler _ _ (IntT userId :%* IntT duration :%* ObjT0Nil) = do
    gid <- baseMaybeInWith (BanError "This tool is only valid in group chats, this is a private chat.") getGid
    banAction <- case (userId, duration) of
      (0, 0) -> return $ BAActionAPI (SetGroupWholeBan gid False) -- disable ban for everyone
      (0, 1) -> return $ BAActionAPI (SetGroupWholeBan gid True) -- enable ban for everyone
      (0, _) -> effThrow $ BanError "When banning everyone, duration can only be 0 (stop ban) or 1 (enable ban). You cannot set a duration for banning everyone, but you can use a crontab job to notify yourself, so you can stop the ban at specific time."
      (uid, duration)
        -> return $ BAActionAPI (SetGroupBan gid (UserId uid) duration)
    action <- liftIO $ baSequenceDelayFullAsync intercalateDelay [banAction]
    tvarBotAction <- lift $ asksModule meowReadsAction
    liftIO $ atomically $ modifyTVar tvarBotAction (<> [return action])
    return $ StringT "success" :%* ObjT0Nil
    where intercalateDelay = 2_000_000 -- 2 second

data LeaveGroupTool
-- ^ A tool that provides the ability to make the bot leave the current group chat.
-- It can be useful when the bot is getting abused.

instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) LeaveGroupTool where
  type ToolInput LeaveGroupTool = ParamToData (ObjectP0 '[])
  type ToolOutput LeaveGroupTool = ParamToData (ObjectP0 '[StringP "result" "the result of the leave action"])
  data ToolError LeaveGroupTool = LeaveError Text deriving Show
  enabledByDefault _ _ = True
  toolEnabled _        = computeSettingFromDB botSettingEnableLeaveGroup botSettingPerChatEnableLeaveGroup
  toolUsable _         = isGroupChat

  toolName _ _ = "leave_group"
  toolDescription _ _ = "Make the bot leave the current group chat. Can be useful when getting extreme abuse and bullying, use with extra caution."
  toolHandler _ _ ObjT0Nil = do
    gid <- baseMaybeInWith (LeaveError "This tool is only valid in group chats, this is a private chat.") getGid
    action <- liftIO $ baSequenceDelayFullAsync intercalateDelay [BADelayedPureAction 5000 $ pure $ BAActionAPI (SetGroupLeave gid False)]
    tvarBotAction <- lift $ asksModule meowReadsAction
    liftIO $ atomically $ modifyTVar tvarBotAction (<> [return action])
    return $ StringT "success, will leave group in 5 seconds" :%* ObjT0Nil
    where intercalateDelay = 2_000_000 -- 2 second

data BlackListUserTool
-- ^ A tool that provides the ability to blacklist a user from interacting with the bot in the current chat.
-- This tool is useful for preventing unwanted interactions from specific users.
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) BlackListUserTool where
  type ToolInput BlackListUserTool  = ParamToData (ObjectP0
      '[ IntP  "user_id"         "the user_id of the user to blacklist / unblacklist"
       , BoolP "ignore_reaction" "if true, will stop reacting to the user, still receives messages in the message history (level 1 blacklist)"
       , BoolP "blacklist"       "if true, will not receive any messsages from the user (level 2 blacklist)"
       , IntP  "duration"        "the duration in seconds to blacklist / ignore the user, 0 means permanent blacklist"
       , StringP "reason" "the reason"
       ]
    )
  type ToolOutput BlackListUserTool = ParamToData (ObjectP0 '[StringP "result" "the result of the blacklist action"])
  data ToolError BlackListUserTool = BlackListError Text deriving Show
  enabledByDefault _ _ = True
  toolEnabled _        = do
    mConfig <- computeSettingFromDB botSettingEnableBlackListUser botSettingPerChatEnableBlackListUser
    case mConfig of
      Just _  -> return mConfig
      Nothing -> Just . not <$> hasPositiveCostModel -- ^ without config, enable if has no cost model
  toolUsable _         = pure True
  toolName _ _ = "blacklist_user"
  toolDescription _ _ = "Blacklist a user from interacting with the bot in the current chat. Use with extra caution. To unblacklist a user, use this tool with the relevant fields set to false."
  toolHandler _ _ (IntT userId :%* BoolT ignore :%* BoolT blacklist :%* IntT duration :%* StringT reason :%* ObjT0Nil) = do
    cid     <- baseMaybeInWith (BlackListError "no ChatId found") getCid
    botname <- lift getBotName
    botid   <- lift getBotId
    time    <- liftIO getCurrentTime
    _ <- lift $ runMeowDBMeowTool $ insert_
      (BotUserBlackList
        { botUserBlackListBotId          = botid
        , botUserBlackListUserId         = UserId userId
        , botUserBlackListChatId         = Just cid
        , botUserBlackListBotName        = botname
        , botUserBlackListIgnoreReaction = ignore
        , botUserBlackListBlackListed    = blacklist
        , botUserBlackListReason         = reason
        , botUserBlackListTime           = time
        , botUserBlackListValidTo        = if duration == 0 then Nothing else Just (addUTCTime (fromIntegral duration) time)
        }
      )
    return $ StringT "success" :%* ObjT0Nil

data BlackListListingTool
-- ^ A tool that provides the ability to list all blacklisted users in the current chat.
instance
  ( In MeowActionQueue mods
  , In LogDatabase mods
  , In MeowDatabase mods
  , MeowAllData mods
  ) => ToolClass (MeowToolEnv mods) BlackListListingTool where
  type ToolInput BlackListListingTool = ParamToData (ObjectP0 '[])
  type ToolOutput BlackListListingTool = ParamToData (ObjectP0 '[StringP "blacklist" "the list of blacklisted users"])
  data ToolError BlackListListingTool = BlackListListingError Text deriving Show
  enabledByDefault _ _ = True
  toolEnabled _        = do
    mConfig <- computeSettingFromDB botSettingEnableBlackListUser botSettingPerChatEnableBlackListUser
    case mConfig of
      Just _  -> return mConfig
      Nothing -> Just . not <$> hasPositiveCostModel -- ^ without config, enable if has no cost model
  toolUsable _         = pure True
  toolName _ _ = "blacklist_list"
  toolDescription _ _ = "List all blacklisted users in the current chat."
  toolHandler _ _ ObjT0Nil = do
    cid     <- baseMaybeInWith (BlackListListingError "no ChatId found") getCid
    botid   <- lift getBotId
    time    <- liftIO getCurrentTime
    -- | run an esqueleto query to get all blacklisted users in the current chat for the current bot, for each user only get the latest entry, i.e. for each BotUserBlackListUserId group return the row with maximal BotUserBlackListTime
    bls <- lift $ fmap (fmap entityVal) . runMeowDBMeowTool $ E.select $ do
      bl <- E.from $ E.table @BotUserBlackList
      E.where_ (     bl E.^. BotUserBlackListBotId  E.==. E.val  botid
               E.&&. bl E.^. BotUserBlackListChatId E.==. E.just (E.val cid)
               )
      E.orderBy [E.desc (bl E.^. BotUserBlackListTime)]
      return bl
    let distinctBls = [ head' bl
                      | bl <- bls
                      , maybe True (>= time) bl.botUserBlackListValidTo
                      , then group by bl.botUserBlackListUserId using groupWith
                      ]
    let blsText = if null distinctBls
          then "No blacklisted users."
          else TL.intercalate "\n---\n"
            [ TL.concat
              [ "user_id="
              , toText (unUserId $ botUserBlackListUserId bl)
              , ", ignore_reaction="
              , toText (botUserBlackListIgnoreReaction bl)
              , ", blacklisted="
              , toText (botUserBlackListBlackListed bl)
              , ", reason="
              , TL.fromStrict $ botUserBlackListReason bl
              , ", time="
              , toText (botUserBlackListTime bl)
              , ", valid_to="
              , maybe "permanent" toText (botUserBlackListValidTo bl)
              ]
            | bl <- bls
            ]
    return $ StringT (TL.toStrict blsText) :%* ObjT0Nil

-- | A helper function to compute whether a setting is enabled from the database.
computeSettingFromDB
  :: ( In LogDatabase mods
     , In MeowDatabase mods
     , MeowAllData mods
     )
  => (BotSetting -> Maybe b) -> (BotSettingPerChat -> Maybe b) -> MeowToolEnv mods (Maybe b)
computeSettingFromDB gSel lSel = fmap (>>= id) . runMaybeT $ do
  botId <- lift getBotId
  cid <- MaybeT getCid
  mGlobal <- do
    grec <- MaybeT $ runMeowDBMeowTool $ selectFirst [BotSettingBotId ==. botId] []
    return $ gSel $ entityVal grec
  mLocal <- runMaybeT $ do
    localSetting <- MaybeT . lift . runMeowDBMeowTool $ selectFirst [BotSettingPerChatChatId ==. cid, BotSettingPerChatBotId ==. botId] []
    MaybeT . pure $ lSel $ entityVal localSetting
  return $ mLocal <|> mGlobal
