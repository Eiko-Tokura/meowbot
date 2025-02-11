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
import Module
import Module.LogDatabase
import System.Meow
import Utils.RunDB hiding (In)
import qualified Data.Text as T

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


listNoteTitles :: BotName -> ChatId -> Meow [(Int, Text)]
listNoteTitles botname cid
  = fmap (fmap (\(Entity _ note) -> (assistantNoteNoteId note, assistantNoteTitle note)))
  . runDB $ selectList [AssistantNoteBotName ==. maybeBotName botname, AssistantNoteChatId ==. cid] []

getNoteListing :: BotName -> ChatId -> Meow (Maybe Text)
getNoteListing bn cid = do
  noteTitles <- listNoteTitles bn cid
  return $ case noteTitles of
    [] -> Nothing
    xs -> Just $ T.unlines $
      [ T.concat
        [ toText note_id
        , ". "
        , title
        ]
      | (note_id, title) <- xs
      ]


-- | The tool to send like or send poke
data ActionTool

instance HasSystemRead (TVar [Meow [BotAction]]) r => ToolClass (MeowToolEnv r mods) ActionTool where
  type ToolInput ActionTool = ParamToData 
    (ObjectP0 
      [ StringP "action" "action can be 'like' or 'poke'"
      , IntP "user_id" "user_id of the target user"
      ]
    )
  type ToolOutput ActionTool = ParamToData (ObjectP0 '[])
  data ToolError ActionTool = ActionError Text deriving Show
  toolName _ _ = "action"
  toolDescription _ _ = "Send a like or a poke"
  toolHandler _ _ ((StringT act) :%* (IntT user_id) :%* ObjT0Nil) = do
    tvarBotAction <- asks (readSystem @(TVar [Meow [BotAction]]) . snd . snd . fst)
    cid <- effectEWith' (const $ ActionError "no ChatId found") $ getCid
    botAction <- liftIO $ readTVarIO tvarBotAction
    action <- case act of
      "like" -> return . pure $ BAActionAPI (SendLike (UserId user_id) 10)
      "poke" -> return . pure $ BAActionAPI (SendPoke (UserId user_id) cid)
      _ -> throwError $ ActionError "action can only be 'like' or 'poke'"
    liftIO $ atomically $ writeTVar tvarBotAction $ botAction ++ [return action]
    return ObjT0Nil
