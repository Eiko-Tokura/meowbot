{-# LANGUAGE UndecidableInstances #-}
module External.ChatAPI.MeowTool where

import Control.Monad.ExceptionReturn
import Control.Monad.Except
import Control.Monad.Reader
import Data.Time.Clock
import Data.PersistModel
import External.ChatAPI.MeowToolEnv
import External.ChatAPI.Tool
import MeowBot.BotStructure
import Module.LogDatabase
import Utils.RunDB hiding (In)
import Data.HList
import qualified Data.Text as T
import System.Meow

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
  toolName _ _ = "NoteToolRead"
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
    '[ StringP "title" "the note title"
     , StringP "content" "the note content"
     ])
  type ToolOutput NoteToolAdd = ParamToData (ObjectP0 '[IntP "note_id" "note_id of the added note"])
  data ToolError NoteToolAdd = NoteAddError Text deriving Show
  toolName _ _ = "NoteToolAdd"
  toolDescription _ _ = "Add a note"
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
  toolName _ _ = "NoteToolReplace"
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
  toolName _ _ = "NoteToolDelete"
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
