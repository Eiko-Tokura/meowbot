module Main where

import Command
import Command.Cat
import Command.Md
import Command.Help
import Command.SetSysMessage
import Command.User
import Command.Aokana
import Command.Random (commandRandom)
import Command.Retract
import Command.Study
import MeowBot.BotStructure
import MeowBot.CommandRule

import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import System.IO (stdout, stderr)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Text (unpack)
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets (Connection, ClientApp, runClient, receiveData)

import Control.Monad.Trans.State
import Control.Monad.Trans

import Debug.Trace

allPrivateCommands :: [BotCommand]
allPrivateCommands = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook]

allGroupCommands :: [BotCommand]
allGroupCommands   = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook]

type RunningMode = [DebugFlag]
data DebugFlag = DebugJson | DebugCQMessage deriving (Eq)

traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a 
  | flag `elem` ls = trace (f a) a
  | otherwise      = a

main :: IO ()
main = do
  args <- getArgs
  let mode = [ DebugJson | "--debug-json" `elem` args ] ++ [ DebugCQMessage | "--debug-cqmsg" `elem` args ]
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runClient "127.0.0.1" 3001 "" (botClient mode)

botClient :: RunningMode -> ClientApp ()
botClient mode connection = do
  putStrLn "Connected to go-cqhttp WebSocket server."
  initialData >>= botLoop mode connection 

botLoop :: RunningMode -> Connection -> AllData -> IO never_returns
botLoop mode conn allData = runStateT (botSingleLoop mode conn) allData >>= botLoop mode conn . snd

botSingleLoop :: RunningMode -> Connection -> StateT AllData IO ()
botSingleLoop mode conn = do
  msgText <- lift $ traceModeWith DebugJson mode unpack <$> receiveData conn
  preData <- get
  let strText = unpack msgText
      eCQmsg  = eitherDecode ((BL.fromStrict . TE.encodeUtf8) msgText) :: Either String CQMessage
  case traceModeWith DebugCQMessage mode show eCQmsg of
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
      AllData [] . OtherData 0 [] (SavedData [] initialUGroups initialGGroups initialRules initialBooks) <$> getAllScripts
  where 
    initialUGroups = [(me, Admin)]
    initialGGroups = [(myGroup, AllowedGroup)]
    initialRules =
      [ Allow (UGroup Admin)          (ExceptCommands [Retract])
      , Allow AllUserAndGroups        (CGroup [Cat, Help, Md, Random])
      , Allow (GGroup AllowedGroup)   (CGroup [System, Aokana])
      , Allow (UGroup Allowed)        (CGroup [System, Aokana])
      , Allow (SingleGroup myGroup)   (SingleCommand Retract)
      , Deny  (UGroup Denied)         AllCommands
      ]
    initialBooks = []
    me = 754829466 :: UserId
    myGroup = 437447251 :: GroupId

