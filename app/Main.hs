module Main where

import Command
import Command.Cat
import Command.Md
import Command.Help
import Command.SetSysMessage
import Command.User
import Command.Aokana
import Command.Random
import MeowBot.BotStructure

import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import System.IO (stdout, stderr)
import System.Directory (doesFileExist)
import Data.Text (unpack)
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets (Connection, ClientApp, runClient, receiveData)

import Control.Monad.Trans.State
import Control.Monad.Trans

allPrivateCommands :: [BotCommand]
allPrivateCommands =[commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom]

allGroupCommands :: [BotCommand]
allGroupCommands = allPrivateCommands

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runClient "127.0.0.1" 3001 "" botClient

botClient :: ClientApp ()
botClient connection = do
  putStrLn "Connected to go-cqhttp WebSocket server."
  initialData >>= botLoop connection 

botLoop :: Connection -> AllData -> IO never_returns
botLoop conn allData = runStateT (botSingleLoop conn) allData >>= botLoop conn . snd

botSingleLoop :: Connection -> StateT AllData IO ()
botSingleLoop conn = do
  msgText <- lift $ receiveData conn
  preData <- get
  let strText = unpack msgText
      eCQmsg  = eitherDecode ((BL.fromStrict . TE.encodeUtf8) msgText) :: Either String CQMessage
  case eCQmsg of
    Left errMsg -> lift $ putStrLn ("Failed to decode message: " ++ errMsg ++ "\n" ++ strText)
    Right cqmsg -> case eventType cqmsg of
      HeartBeat -> return ()
      Response -> do
        modify $ updateAllDataByMessage cqmsg
        lift $ putStrLn "<- response."
      PrivateMessage -> do
        cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
        modify $ updateAllDataByMessage   cqmsg'
        lift $ putStrLn $ "<- " ++ showCQ cqmsg'
        doBotCommands conn allPrivateCommands
      GroupMessage -> do
        cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
        modify $ updateAllDataByMessage   cqmsg'
        lift $ putStrLn $ "<- " ++ showCQ cqmsg'
        doBotCommands conn allGroupCommands
      UnknownMessage -> return ()
      _ -> return ()
  saveData preData

initialData :: IO AllData
initialData = do
  fileExist <- doesFileExist savedDataPath
  if fileExist
    then do
      putStrLn "Found saved data file, loading data! owo"
      savedData <- readFile savedDataPath
      let msavedData = read savedData
      AllData [] . OtherData 0 [] msavedData <$> getAllScripts
    else do
      putStrLn "No saved data file found, starting with empty data! owo" 
      AllData [] . OtherData 0 [] (SavedData [] initialAllowedGroups initialAllowedUsers initialDeniedUsers initialAdminUsers ) <$> getAllScripts
  where 
    initialAllowedGroups = [437447251]            -- my qq group number
    initialAllowedUsers  = UserId <$> [754829466]
    initialAdminUsers    = UserId <$> [754829466] -- my qq number
    initialDeniedUsers   = UserId <$> []
