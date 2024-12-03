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
import Command.Poll
import MeowBot.BotStructure
import MeowBot.CommandRule

import Control.Parallel.Strategies
import Control.Concurrent.Async

import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import System.IO (stdout, stderr)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Data.Aeson (eitherDecode)
import Data.Coerce (coerce)
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets (Connection, ClientApp, runClient, runServer, receiveData, PendingConnection, acceptRequest)

import Control.Monad.Trans.ReaderState(globalize)
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad

-- import GHC.Conc (forkIO)
-- import GHC.Debug.Stub

import Debug.Trace

allPrivateCommands :: [BotCommand]
allPrivateCommands = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook, commandPoll]

allGroupCommands :: [BotCommand]
allGroupCommands   = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook, commandPoll]

type RunningMode = [DebugFlag]
data DebugFlag   = DebugJson | DebugCQMessage deriving (Eq, Show)
data RunningFlag = RunClient String Int | RunServer String Int deriving (Eq, Show)
newtype IdentityFlag = UseName String deriving (Eq, Show)
newtype CommandFlags = CommandFlag CommandId deriving (Eq, Show)

traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a
  | flag `elem` ls = trace (f a) a
  | otherwise      = a

-- | The main function of the bot.
--  It will parse the command line arguments and start the bot.
--
--  you can run a debuger on port 2077
--  by changing to main = withGhcDebugTCP "127.0.0.1" 2077 $ do
--
main :: IO ()
main = do
  args <- getArgs
  let mode = [ DebugJson | "--debug-json" `elem` args ] ++ [ DebugCQMessage | "--debug-cqmsg" `elem` args ]
      runFlags
        =  [ RunClient ip (read port) | ("--run-client", ip, port) <- zip3 args (drop 1 args) (drop 2 args) ]
        ++ [ RunServer ip (read port) | ("--run-server", ip, port) <- zip3 args (drop 1 args) (drop 2 args) ]
      identityFlags
        =  [ UseName name | ("--name", name) <- zip args (drop 1 args) ]
      commandIds
        =  if "--all-commands" `elem` args
           then []
           else [ read cmd | ("--command", cmd) <- zip args (drop 1 args) ]
      mGlobalSysMsg
        =  listToMaybe [ sysMsg | ("--sys-msg", sysMsg) <- zip args (drop 1 args) ]
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
        , nameOfBot = coerce $ listToMaybe identityFlags
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

-- | consider changing the model to allow some concurrency
botSingleLoop :: BotModules -> RunningMode -> Connection -> StateT AllData IO ()
botSingleLoop mods mode conn = do
  asyncBotActions <- gets (asyncActions . otherdata)
  asyncMsgText    <- lift $ async $ traceModeWith DebugJson mode unpack <$> receiveData conn
  prevData        <- get
  result <- lift $ 
    if   null asyncBotActions 
    then Left <$> wait asyncMsgText
    else async (waitAny (S.toList asyncBotActions)) >>= waitEitherCancel asyncMsgText
  case result of
    Left  msgText                         -> handleMessage mods mode conn msgText
    Right (completedAsync, meowBotAction) -> handleCompletedAsync conn completedAsync meowBotAction
  saveData prevData

-- | deregister the completed async action and do the bot action
handleCompletedAsync :: Connection -> Async (Meow [BotAction]) -> Meow [BotAction] -> StateT AllData IO ()
handleCompletedAsync conn completedAsync meowBotAction = do
  -- | remove the completed async action from the set
  newAsyncSet <- gets (S.delete completedAsync . asyncActions . otherdata)
  modify $ \ad -> ad { otherdata = (otherdata ad) { asyncActions = newAsyncSet } }
  -- | do the bot action in the Meow monad
  globalize wholechat otherdata AllData $ meowBotAction >>= mapM_ (doBotAction conn)
  -- | update the saved data if needed
  updateSavedAdditionalData

-- | handle the message from the server
handleMessage :: BotModules -> RunningMode -> Connection -> Text -> StateT AllData IO ()
handleMessage mods mode conn msgText = do
  let strText = unpack msgText
      eCQmsg  = eitherDecode ((BL.fromStrict . TE.encodeUtf8) msgText) :: Either String CQMessage
  case traceModeWith DebugCQMessage mode show eCQmsg of
    Left errMsg -> lift $ putStrLn ("Failed to decode message: " ++ errMsg ++ "\n" ++ strText)
    Right cqmsg -> case eventType cqmsg of
      HeartBeat -> return ()
      Response -> do
        modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg
        updateSavedAdditionalData 
        lift $ putStrLn "<- response."
      PrivateMessage -> do
        cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
        modify $ (`using` rseqWholeChat) . updateAllDataByMessage   cqmsg'
        updateSavedAdditionalData
        lift $ putStrLn $ "<- " ++ showCQ cqmsg'
        doBotCommands conn (filter ((`elem` canUsePrivateCommands mods) . identifier) allPrivateCommands)
      GroupMessage -> do
        cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
        modify $ (`using` rseqWholeChat) . updateAllDataByMessage   cqmsg'
        updateSavedAdditionalData
        lift $ putStrLn $ "<- " ++ showCQ cqmsg'
        doBotCommands conn (filter ((`elem` canUseGroupCommands mods) . identifier) allGroupCommands)
      UnknownMessage -> return ()
      _ -> return ()

initialData :: BotModules -> IO AllData
initialData mods = do
  fileExist <- doesFileExist (savedDataPath $ nameOfBot mods)
  if fileExist
    then do
      putStrLn "Found saved data file, loading data! owo"
      savedData <- readFile $ savedDataPath $ nameOfBot mods
      let msavedData = read savedData
      AllData [] . OtherData 0 [] msavedData mods (coerce $ savedAdditional msavedData) S.empty <$> getAllScripts
    else do
      putStrLn "No saved data file found, starting with empty data! owo"
      AllData [] . OtherData 0 [] (SavedData [] initialUGroups initialGGroups initialRules initialBooks []) mods [] S.empty <$> getAllScripts
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

