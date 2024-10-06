{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe (listToMaybe)
import Data.Text (unpack)
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets (Connection, ClientApp, runClient, runServer, receiveData, PendingConnection, acceptRequest)

import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad

import GHC.Conc (forkIO)

import Debug.Trace

allPrivateCommands :: [BotCommand]
allPrivateCommands = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook]

allGroupCommands :: [BotCommand]
allGroupCommands   = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook]

type RunningMode = [DebugFlag]
data DebugFlag = DebugJson | DebugCQMessage deriving (Eq, Show)
data RunningFlag = RunClient String Int | RunServer String Int deriving (Eq, Show)
newtype IdentityFlag = UseName String deriving (Eq, Show)
newtype CommandFlags = CommandFlag CommandId deriving (Eq, Show)
--newtype SystemMessageFlag = UseSysMessage String deriving (Eq, Show)

traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a 
  | flag `elem` ls = trace (f a) a
  | otherwise      = a

main :: IO ()
main = do
  args <- getArgs
  let mode = [ DebugJson | "--debug-json" `elem` args ] ++ [ DebugCQMessage | "--debug-cqmsg" `elem` args ] 
      runFlags 
        =  [ RunClient ip (read port) | ("--run-client", ip, port) <- zip3 args (tail args) (tail $ tail args) ] 
        ++ [ RunServer ip (read port) | ("--run-server", ip, port) <- zip3 args (tail args) (tail $ tail args) ]
      identityFlags 
        =  [ UseName name | ("--name", name) <- zip args (tail args) ]
      commandIds
        =  if "--all-commands" `elem` args 
           then []
           else [ read cmd | ("--command", cmd) <- zip args (tail args) ]
      mGlobalSysMsg
        =  listToMaybe [ sysMsg | ("--sys-msg", sysMsg) <- zip args (tail args) ]
  putStrLn "Meow~ Here comes the MeowBot! owo"
  putStrLn $ "Running mode: "   ++ show mode
  putStrLn $ "Running flags: "  ++ show runFlags
  putStrLn $ "Identity flags: " ++ show identityFlags
  putStrLn $ "Command flags: "  ++ show commandIds
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let withDefault def [] = def
      withDefault _ xs = xs
      botModules = BotModules
        { canUseGroupCommands   = withDefault (identifier <$> allGroupCommands) commandIds
        , canUsePrivateCommands = withDefault (identifier <$> allPrivateCommands) commandIds
        , nameOfBot = case identityFlags of
            [] -> Nothing
            (UseName n) : _ -> Just n
        , globalSysMsg = mGlobalSysMsg
        }
  if null runFlags
  then runClient "127.0.0.1" 3001 "" (botClient botModules mode)
  else forM_ runFlags $ \case
    (RunClient ip port) -> void $ runClient ip port "" (botClient botModules mode)
    (RunServer ip port) -> void $ runServer ip port (botServer botModules mode)

botClient :: BotModules -> RunningMode -> ClientApp ()
botClient mods mode connection = do
  putStrLn "Connected to go-cqhttp WebSocket server."
  initialData mods >>= botLoop mods mode connection

botServer :: BotModules -> RunningMode -> PendingConnection -> IO ()
botServer mods mode connection = do
  conn <- acceptRequest connection
  putStrLn "As server, connected to go-cqhttp WebSocket client."
  initialData mods >>= botLoop mods mode conn

botLoop :: BotModules -> RunningMode -> Connection -> AllData -> IO never_returns
botLoop mods mode conn allData = runStateT (botSingleLoop mods mode conn) allData >>= botLoop mods mode conn . snd

botSingleLoop :: BotModules -> RunningMode -> Connection -> StateT AllData IO ()
botSingleLoop mods mode conn = do
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
        doBotCommands conn (filter ((`elem` canUsePrivateCommands mods) . identifier) allPrivateCommands)
      GroupMessage -> do
        cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
        modify $ updateAllDataByMessage   cqmsg'
        lift $ putStrLn $ "<- " ++ showCQ cqmsg'
        doBotCommands conn (filter ((`elem` canUseGroupCommands mods) . identifier) allGroupCommands)
      UnknownMessage -> return ()
      _ -> return ()
  saveData preData

initialData :: BotModules -> IO AllData
initialData mods = do
  fileExist <- doesFileExist (savedDataPath $ nameOfBot mods)
  if fileExist
    then do
      putStrLn "Found saved data file, loading data! owo"
      savedData <- readFile $ savedDataPath $ nameOfBot mods
      let msavedData = read savedData
      AllData [] . OtherData 0 [] msavedData mods <$> getAllScripts
    else do
      putStrLn "No saved data file found, starting with empty data! owo" 
      AllData [] . OtherData 0 [] (SavedData [] initialUGroups initialGGroups initialRules initialBooks) mods <$> getAllScripts
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

