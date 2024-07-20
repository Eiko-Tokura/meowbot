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
        mid <- increaseAbsoluteId
        modify $ updateAllDataByMessage $ cqmsg {absoluteId = Just mid}
        lift $ putStrLn $ "<- " ++ showCQ cqmsg {absoluteId = Just mid}
        doBotCommands conn allPrivateCommands
      GroupMessage -> do
        mid <- increaseAbsoluteId
        modify $ updateAllDataByMessage cqmsg {absoluteId = Just mid}
        lift $ putStrLn $ "<- " ++ showCQ cqmsg {absoluteId = Just mid}
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

-- Things I learned in this project, just some notes, not a plan
-- 1. Either就是coproduct
-- 2. Either e是函子，是Monad
-- 3. the unit () is a final object, but not initial object, it is canonically isomorphic to a set with one element.
-- 4. the full sub category of Num class is pre-additive but not additive, since there is no zero object.
-- 5. In haskell, everything is safe except for the type variables. They can be error prone and produce hidden error if not paying attention to. I suggest labeling out the exact type you are using everytime using type variables.
-- 6. Before you build something big, do take time to write and think what you should be doing, like this one.
-- 7. name@pattern can be used to capture both the whole data and parts of the data
