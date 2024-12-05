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
import Parser.Run
import Parser.Except

import Control.Parallel.Strategies
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad.STM

import GHC.Exception (SomeException)
import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import System.IO (stdout, stderr)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Data.Aeson (eitherDecode)
import Data.Coerce (coerce)
import Data.List (isPrefixOf)
import Data.Either
import Data.Time.Clock (getCurrentTime)
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets (Connection, ClientApp, runClient, runServer, receiveData, PendingConnection, acceptRequest)

import Control.Monad.Trans.ReaderState(globalize)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Monad

-- import GHC.Conc (forkIO)
-- import GHC.Debug.Stub
import Debug.Trace

-- | A tracing function that will only print the message when the flag is in the list.
traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a
  | flag `elem` ls = trace (f a) a
  | otherwise      = a

allPrivateCommands :: [BotCommand]
allPrivateCommands = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandStudy, commandBook, commandPoll]

allGroupCommands :: [BotCommand]
allGroupCommands   = [commandCat, commandMd, commandHelp, commandSetSysMessage, commandUser, commandAokana, commandRandom, commandRetract, commandStudy, commandBook, commandPoll]

type RunningMode = [DebugFlag]
data DebugFlag   = DebugJson | DebugCQMessage deriving (Eq, Show)
data RunningFlag = RunClient String Int | RunServer String Int deriving (Eq, Show)
data IdentityFlag = UseName String | UseSysMsg String deriving (Eq, Show)
newtype CommandFlags = CommandFlag CommandId deriving (Eq, Show)

data BotInstance = BotInstance RunningFlag [IdentityFlag] [CommandFlags] [DebugFlag] deriving (Eq, Show)

parseArgs :: ParserE [String] String String [BotInstance]
parseArgs = many (do
  runFlag <- asum
    [ liftR1 just "--run-client" >> RunClient <$> withE "Usage: --run-client <ip> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
    , liftR1 just "--run-server" >> RunServer <$> withE "Usage: --run-server <ip> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
    ]
  restFlags <- many (identityParser |+| commandParser |+| debugParser |+| unrecognizedFlag)
  return $ BotInstance runFlag (lefts restFlags) (lefts $ rights restFlags) (lefts $ rights $ rights restFlags)
  ) <* liftR end
    where
      identityParser = asum 
        [ liftR1 just "--name" >> withE "--name needs a String argument" (UseName <$> nonFlag)
        , liftR1 just "--sys-msg" >> withE "--sys-msg needs a String argument" (UseSysMsg <$> nonFlag)
        ]
      commandParser = do
        liftR1 just "--command"
        addE ("--command needs exactly one commandId argument, " ++ commandIdHint) (CommandFlag <$> readE commandIdHint nonFlag)
      debugParser = liftR $ asum [ just "--debug-json" >> return DebugJson, just "--debug-cqmsg" >> return DebugCQMessage ]
      unrecognizedFlag = do
        flag <- liftR $ require ("--" `isPrefixOf`) getItem
        lift $ throwE $ "Unrecognized flag " ++ flag
      nonFlag = require (not . ("--" `isPrefixOf`)) getItem
      commandIdHint = "commandId must be one of " ++ show [minBound..maxBound :: CommandId]

-- | The main function of the bot.
--  It will parse the command line arguments and start the bot.
--
--  you can run a debuger on port 2077
--  by changing to main = withGhcDebugTCP "127.0.0.1" 2077 $ do
main :: IO ()
main = do
  args <- getArgs
  case runParserE argumentHelp parseArgs args of
    Left errMsg -> putStrLn errMsg
    Right []    -> runInstances [BotInstance (RunClient "127.0.0.1" 3001) [] [] []]
    Right bots  -> runInstances bots
  where argumentHelp = unlines 
          [ "Usage: MeowBot [--run-client <ip> <port> | --run-server <ip> <port>] [--name <name>] [--sys-msg <msg>] [--command <commandId>] [--debug-json] [--debug-cqmsg]"
          , "  --run-client <ip> <port>  : run the bot as a client connecting to the go-cqhttp WebSocket server"
          , "  --run-server <ip> <port>  : run the bot as a server, using reverse WebSocket connection"
          , "  --name <name>             : set the name of the bot"
          , "  --sys-msg <msg>           : set the global system message of the bot"
          , "  --command <commandId>     : allow the bot to use the command with the given commandId, use multiple --command flags to allow multiple commands"
          , "                              commandId must be one of " ++ show [minBound..maxBound :: CommandId]
          , "                              if no --command flags are given, the bot will use all commands"
          , "  --debug-json              : print the JSON message received from the server"
          , "  --debug-cqmsg             : print the decoded CQMessage"
          , "  If no arguments are given, the bot will run as a client connecting to the go-cqhttp WebSocket server on 127.0.0.1:3001"
          , ""
          , "Multiple bots can be started by using multiple sets of flags, starting with a run flag followed by other flags."
          ]

runInstances :: [BotInstance] -> IO ()
runInstances bots = do
  putStrLn "Meow~ Here comes the MeowBot! owo"
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  forM_ bots $ \bot@(BotInstance runFlag identityFlags commandFlags mode) -> do
    putStrLn $ "\n### Starting bot instance: " ++ show bot
    putStrLn $ "Running mode: "   ++ show mode
    putStrLn $ "Running flags: "  ++ show runFlag
    putStrLn $ "Identity flags: " ++ show identityFlags
    putStrLn $ "Command flags: "  ++ show commandFlags
    let mGlobalSysMsg = listToMaybe [ sysMsg | UseSysMsg sysMsg <- identityFlags ]
        withDefault def [] = def
        withDefault _ xs = xs
        botModules = BotModules
          { canUseGroupCommands   = withDefault (identifier <$> allGroupCommands)   (coerce commandFlags)
          , canUsePrivateCommands = withDefault (identifier <$> allPrivateCommands) (coerce commandFlags)
          , nameOfBot = listToMaybe [ nameBot | UseName nameBot <- identityFlags ]
          , globalSysMsg = mGlobalSysMsg
          }
    case runFlag of
      RunClient ip port -> runClient ip port "" (botClient botModules mode) `forkFinally` recoverBot bot
      RunServer ip port -> runServer ip port    (botServer botModules mode) `forkFinally` recoverBot bot
  -- halt the main thread forever to avoid the main thread exiting, in the future it can be replaced with a control panel
  putStrLn "All bots started! owo"
  sequence_ (repeat $ threadDelay 60_000_000)

putLogLn :: String -> IO ()
putLogLn str = do
  time <- show <$> getCurrentTime
  let str' = "[" ++ time ++ "]\n" ++ str in putStrLn str' >> appendFile "error.log" (str' ++ "\n")

recoverBot :: BotInstance -> Either SomeException () -> IO ()
recoverBot bot = \case
  Left e ->  do
    putLogLn $ "****The bot instance " ++ show bot ++ "\n****failed with exception:****\n" ++ show e
    putStrLn "****recovering in 60 seconds****"
    threadDelay 60_000_000
    putStrLn "****restarting****"
    runInstances [bot]
  Right _ -> putStrLn $ "Bot instance finished: " ++ show bot

botClient :: BotModules -> RunningMode -> ClientApp ()
botClient mods mode connection = do
  putStrLn "Connected to go-cqhttp WebSocket server."
  initialData mods >>= void . runStateT (botLoop Nothing mods mode connection)

botServer :: BotModules -> RunningMode -> PendingConnection -> IO ()
botServer mods mode connection = do
  conn <- acceptRequest connection
  putStrLn "As server, connected to go-cqhttp WebSocket client."
  initialData mods >>= void . runStateT (botLoop Nothing mods mode conn)

-- | changed the model to allow some concurrency
botLoop :: Maybe (Async Text) -> BotModules -> RunningMode -> Connection -> StateT AllData IO never_returns
botLoop reuseAsyncMsgText mods mode conn = do
  asyncMsgText    <- maybe (lift $ async $ traceModeWith DebugJson mode unpack <$> receiveData conn) return reuseAsyncMsgText
  asyncActionList <- gets (S.toList . asyncActions . otherdata)
  prevData        <- get
  result <- lift . atomically $ Left <$> waitSTM asyncMsgText <|> Right <$> asum [ (ba, ) <$> waitSTM ba | ba <- asyncActionList ]
  newAsyncMsg <- case result of
    Left  msgText                         -> handleMessage mods mode conn msgText >> return Nothing
    Right (completedAsync, meowBotAction) -> handleCompletedAsync conn completedAsync meowBotAction >> return (Just asyncMsgText)
  saveData prevData
  botLoop newAsyncMsg mods mode conn

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

