{-# LANGUAGE UndecidableInstances #-}
module External.ChatAPI.MeowTool where

import Control.Monad.Except
import Control.Monad.ExceptionReturn
import Control.Monad.Reader
import Data.HList
import Data.PersistModel
import Data.Time.Clock
import Control.Concurrent.STM
import External.ChatAPI.MeowToolEnv
import External.ChatAPI.Tool
import MeowBot.BotStructure
import MeowBot.Action
import Module
import Module.LogDatabase
import System.Meow
import Utils.RunDB hiding (In)
import MeowBot.CronTab.CronMeowAction
import qualified Data.Text as T
import Cron.Parser (validateCronText)

data NoteToolRead
data NoteToolAdd
data NoteToolReplace
data NoteToolDelete

instance LogDatabase `In` mods => ToolClass (MeowToolEnv r mods) NoteToolRead where
  type ToolInput NoteToolRead = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the note to read"])
  type ToolOutput NoteToolRead = ParamToData (ObjectP0 --"note" "retrived note"
    '[ IntP "note_id" "note_id of the note"
     , StringP "title" "the note title"
     , StringP "content" "the note content"
     , StringP "time" "the time the note was last modified"
     ])
  data ToolError NoteToolRead = NoteReadError Text deriving Show
  toolName _ _ = "note_read"
  toolDescription _ _ = "Get note content by note_id"
  toolHandler _ _ ((IntT note_id) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid  <- effectEWith' (const $ NoteReadError "no cid found") $ getCid
    note <- lift $ fmap (fmap entityVal) . runDBMeowTool $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id] []
    case note of
      Just (AssistantNote _ _ _ title content time) -> return $ IntT note_id :%* StringT title :%* StringT content :%* StringT (toText time) :%* ObjT0Nil
      Nothing -> throwError $ NoteReadError "Note not found"

instance LogDatabase `In` mods => ToolClass (MeowToolEnv r mods) NoteToolAdd where
  type ToolInput NoteToolAdd = ParamToData (ObjectP0 --"note" "the note to add"
    '[ StringP "title" "the note title should be informative"
     , StringP "content" "the note content, you can include more detailed information here"
     ])
  type ToolOutput NoteToolAdd = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the added note"])
  data ToolError NoteToolAdd = NoteAddError Text deriving Show
  toolName _ _ = "note_add"
  toolDescription _ _ = "Add a note, You can use the note tools to take notes if you want to memorize things that people teach you, or you learn about the people more. The title will be added to your system prompt which becomes part of your memory."
  toolHandler _ _ ((StringT title) :%* (StringT content) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid <- effectEWith' (const $ NoteAddError "no cid found") $ getCid
    time <- liftIO getCurrentTime
    maxNoteId <- lift
      $ fmap (maybe 0 (assistantNoteNoteId . entityVal)) . runDBMeowTool
      $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid] [Desc AssistantNoteNoteId, LimitTo 1]
    let newNoteId = maxNoteId + 1
    lift $ runDBMeowTool $ insert $ AssistantNote
      { assistantNoteBotName = botname
      , assistantNoteChatId = cid
      , assistantNoteNoteId = newNoteId
      , assistantNoteTitle = title
      , assistantNoteContent = content
      , assistantNoteTime = time
      }
    return $ IntT newNoteId :%* ObjT0Nil

instance LogDatabase `In` mods => ToolClass (MeowToolEnv r mods) NoteToolReplace where
  type ToolInput NoteToolReplace = ParamToData (ObjectP0 --"note" "the note to replace"
    '[ IntP "note_id" "note_id of the note to replace"
     , StringP "title" "the note title"
     , StringP "content" "the note content"
     ])
  type ToolOutput NoteToolReplace = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the replaced note"])
  data ToolError NoteToolReplace = NoteReplaceError Text deriving Show
  toolName _ _ = "note_replace"
  toolDescription _ _ = "Replace a note"
  toolHandler _ _ ((IntT note_id) :%* (StringT title) :%* (StringT content) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid <- effectEWith' (const $ NoteReplaceError "no cid found") $ getCid
    time <- liftIO getCurrentTime
    note <- lift $ fmap (fmap entityVal) . runDBMeowTool $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id] []
    case note of
      Just _ -> lift $ runDBMeowTool $ updateWhere
        [ AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id ]
        [ AssistantNoteTitle =. title
        , AssistantNoteContent =. content
        , AssistantNoteTime =. time
        ]
      Nothing -> throwError $ NoteReplaceError "Note not found"
    return $ IntT note_id :%* ObjT0Nil

instance LogDatabase `In` mods => ToolClass (MeowToolEnv r mods) NoteToolDelete where
  type ToolInput NoteToolDelete  = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the note to delete"])
  type ToolOutput NoteToolDelete = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the deleted note"])
  data ToolError NoteToolDelete = NoteDeleteError Text deriving Show
  toolName _ _ = "note_delete"
  toolDescription _ _ = "Delete a note"
  toolHandler _ _ ((IntT note_id) :%* ObjT0Nil) = do
    botname <- lift getBotName
    cid <- effectEWith' (const $ NoteDeleteError "no cid found") $ getCid
    note <- lift $ fmap (fmap entityVal) . runDBMeowTool $ selectFirst [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id] []
    case note of
      Just _ -> lift $ runDBMeowTool $ deleteWhere [AssistantNoteBotName ==. botname, AssistantNoteChatId ==. cid, AssistantNoteNoteId ==. note_id]
      Nothing -> throwError $ NoteDeleteError "Note not found"
    return $ IntT note_id :%* ObjT0Nil


listNoteTitleAndContents :: BotName -> ChatId -> Meow [(Int, (Text, Text))]
listNoteTitleAndContents botname cid
  = fmap (fmap (\(Entity _ note) -> (assistantNoteNoteId note, (assistantNoteTitle note, assistantNoteContent note))))
  . runDB $ selectList [AssistantNoteBotName ==. maybeBotName botname, AssistantNoteChatId ==. cid] []

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

instance HasSystemRead (TVar [Meow [BotAction]]) r => ToolClass (MeowToolEnv r mods) ActionTool where
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
    tvarBotAction <- asks (readSystem @(TVar [Meow [BotAction]]) . snd . snd . fst)
    cid <- effectEWith' (const $ ActionError "no ChatId found") $ getCid
    action <- case act of
      "like" -> liftIO $ baSequenceDelayFullAsync intercalateDelay 
        [ BAActionAPI (SendLike (UserId user_id) 10)   | user_id <- user_ids ]
      "poke" -> liftIO $ baSequenceDelayFullAsync intercalateDelay
        [ BAActionAPI (SendPoke (UserId user_id) cid) | user_id <- user_ids ]
      _ -> throwError $ ActionError "action can only be 'like' or 'poke'"
    liftIO $ atomically $ modifyTVar tvarBotAction $ (<> [return action])
    return $ StringT "success" :%* ObjT0Nil
    where intercalateDelay = 2_000_000 -- 2 second

data CronTabTool

instance
  ( HasSystemRead (TVar [Meow [BotAction]]) r
  , In LogDatabase mods
  ) => ToolClass (MeowToolEnv r mods) CronTabTool where
  type ToolInput CronTabTool = ParamToData
    (ObjectP0
      [ StringP "crontab" "crontab format trigger, e.g. '0 0 * * *' "
      , IntP "repeat" "number of times to trigger, 1 means one-off, 0 means repeat indefinitely"
      , StringP "description" "description of what exactly you need to do when the time comes"
      ]
    )
  type ToolOutput CronTabTool = ParamToData (ObjectP0 '[StringP "result" "the result of the tool"])
  data ToolError CronTabTool = TimedTaskToolError Text deriving Show
  toolName _ _ = "crontab"
  toolDescription _ _ =  "Set a cron job to trigger a chat after a certain time. Example Output : "
                      <> "{\"tool\": \"crontab\", \"args\": {\"crontab\": <crontab format>, \"description\": <description>}}"
  toolHandler _ _ ((StringT unVerifiedCronText) :%* (IntT repeat) :%* (StringT desc) :%* ObjT0Nil) = do
    botId   <- lift getBotId
    botname <- lift getBotName
    cid <- effectEWith' (const $ TimedTaskToolError  "no ChatId found") $ getCid
    cronText <- pureEWith' (const $ TimedTaskToolError "invalid crontab format") $ validateCronText unVerifiedCronText
    lift $ runDBMeowTool $ insert $ BotCronJob
      { botCronJobBotName          = botname
      , botCronJobBotId            = botId
      , botCronJobCronSchedule     = cronText
      , botCronJobCronRepeatFinite = if repeat == 0 then Nothing else Just repeat
      , botCronJobCronMeowAction   = CronMeowChatBack cid desc
      }
    return $ StringT "success" :%* ObjT0Nil
