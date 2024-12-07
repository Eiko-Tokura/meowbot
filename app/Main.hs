{-# LANGUAGE LambdaCase, OverloadedStrings #-}
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

import External.ProxyWS
import Utils.ByteString

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
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Aeson (eitherDecode)
import Data.Coerce (coerce)
import Data.List (isPrefixOf)
import Data.Either
import Data.Time.Clock (getCurrentTime)
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets (Connection, ClientApp, runClient, runServer, receiveData, PendingConnection, acceptRequest, sendTextData)

import Control.Monad.Trans.State
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

parseArgs :: ParserE [String] String String [BotInstance]
parseArgs = many (do
  runFlag <- asum
    [ liftR1 just "--run-client" >> RunClient <$> withE "Usage: --run-client <ip> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
    , liftR1 just "--run-server" >> RunServer <$> withE "Usage: --run-server <ip> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
    ]
  restFlags <- many (identityParser |+| commandParser |+| debugParser |+| proxyParser |+| logParser |+| unrecognizedFlag)
  return $ BotInstance runFlag (lefts restFlags) (lefts $ rights restFlags) (lefts $ rights $ rights restFlags) (lefts $ rights $ rights $ rights restFlags) (lefts $ rights $ rights $ rights $ rights restFlags)
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
      proxyParser = do
        liftR1 just "--proxy"
        ProxyFlag <$> withE "Usage: --proxy <address> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
      logParser = LogFlag <$> liftR1 just "--log" >> withE "--log needs a file path argument" (LogFlag <$> nonFlag)
      unrecognizedFlag = do
        flag <- liftR $ require ((&&) <$> ("--" `isPrefixOf`) <*> (`notElem` ["--run-client", "--run-server"])) getItem
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
    Right []    -> runInstances [BotInstance (RunClient "127.0.0.1" 3001) [] [] [] [] []] >> halt
    Right bots  -> runInstances bots >> halt
  where halt = threadDelay maxBound >> halt
        argumentHelp = unlines
          [ "Usage: MeowBot [--run-client <ip> <port> | --run-server <ip> <port>] [--name <name>] [--sys-msg <msg>] [--command <commandId>] [--debug-json] [--debug-cqmsg] [--proxy <address> <port>]"
          , "  --run-client <ip> <port>  : run the bot as a client connecting to the go-cqhttp WebSocket server"
          , "  --run-server <ip> <port>  : run the bot as a server, using reverse WebSocket connection"
          , "  --name <name>             : set the name of the bot"
          , "  --sys-msg <msg>           : set the global system message of the bot"
          , "  --command <commandId>     : allow the bot to use the command with the given commandId, use multiple --command flags to allow multiple commands"
          , "                              commandId must be one of " ++ show [minBound..maxBound :: CommandId]
          , "                              if no --command flags are given, the bot will use all commands"
          , "  --debug-json              : print the JSON message received from the server"
          , "  --debug-cqmsg             : print the decoded CQMessage"
          , "  --proxy <address> <port>  : set the proxy server to connect to, use multiple --proxy flags to connect to multiple servers"
          , "  If no arguments are given, the bot will run as a client connecting to the go-cqhttp WebSocket server on 127.0.0.1:3001"
          , ""
          , "Multiple bots can be started by using multiple sets of flags, starting with a run flag followed by other flags."
          ]

runInstances :: [BotInstance] -> IO ()
runInstances bots = do
  putStrLn "Meow~ Here comes the MeowBot! owo"
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  forM_ bots $ \bot@(BotInstance runFlag identityFlags commandFlags mode proxyFlags logFlags) -> do
    putStrLn $ "\n### Starting bot instance: " ++ show bot
    putStrLn $ "Running mode: "   ++ show mode
    putStrLn $ "Running flags: "  ++ show runFlag
    putStrLn $ "Identity flags: " ++ show identityFlags
    putStrLn $ "Command flags: "  ++ show commandFlags
    putStrLn $ "Proxy flags: "    ++ show proxyFlags
    putStrLn $ "Log flags: "      ++ show logFlags
    proxyData <- sequence $ [ createProxyData addr port | ProxyFlag addr port <- proxyFlags ]
    let mGlobalSysMsg = listToMaybe [ sysMsg | UseSysMsg sysMsg <- identityFlags ]
        withDefault def [] = def
        withDefault _ xs = xs
        botModules = BotModules
          { canUseGroupCommands   = withDefault (identifier <$> allGroupCommands)   (coerce commandFlags)
          , canUsePrivateCommands = withDefault (identifier <$> allPrivateCommands) (coerce commandFlags)
          , nameOfBot = listToMaybe [ nameBot | UseName nameBot <- identityFlags ]
          , globalSysMsg = mGlobalSysMsg
          , proxyTChans = proxyData
          , logFile = [ logFile | LogFlag logFile <- logFlags ]
          , botInstance = bot
          }
    case runFlag of
      RunClient ip port -> runClient ip port "" (botClient botModules mode) `forkFinally` recoverBot botModules
      RunServer ip port -> runServer ip port    (botServer botModules mode) `forkFinally` recoverBot botModules
  putStrLn "All bots started! owo"

putLogLn :: [FilePath] -> String -> IO ()
putLogLn [] = putStrLn
putLogLn files = \str -> do
  time <- show <$> getCurrentTime
  let str' = "[" ++ time ++ "]\n" ++ str in putStrLn str' >> mapM_ (`appendFile` (str' ++ "\n")) files
{-# INLINE putLogLn #-}

recoverBot :: BotModules -> Either SomeException () -> IO ()
recoverBot bot = \case
  Left e ->  do
    putLogLn ("error.log":[f | LogFlag f <- botLogFlags (botInstance bot)]) $
      "****The bot instance " ++ show bot ++ "\n****failed with exception:****\n" ++ show e
    putStrLn   "****recovering in 60 seconds****"
    threadDelay 60_000_000
    putStrLn   "****restarting****"
    let mode = botDebugFlags (botInstance bot)
    case botRunFlag (botInstance bot) of
      RunClient ip port -> void $ runClient ip port "" (botClient bot mode) `forkFinally` recoverBot bot
      RunServer ip port -> void $ runServer ip port    (botServer bot mode) `forkFinally` recoverBot bot
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
botLoop :: Maybe (Async BL.ByteString) -> BotModules -> RunningMode -> Connection -> StateT AllData IO never_returns
botLoop reuseAsyncMsgText mods mode conn = do
  asyncMsgText    <- maybe (lift $ async $ traceModeWith DebugJson mode bsToString <$> receiveData conn) return reuseAsyncMsgText
  asyncActionList <- gets (S.toList . asyncActions . otherdata)
  prevData        <- get
  result <- lift . atomically $ asum
    [ Left <$> waitSTM asyncMsgText
    , Right . Left  <$> asum [ (ba, ) <$> waitSTM ba | ba <- asyncActionList ]
    , Right . Right <$> asum ( map receiveFromProxy (proxyTChans mods) )
    ]
  newAsyncMsg <- case result of
    Left  msgBS                                  -> handleMessage mods mode conn msgBS >> return Nothing
    Right (Left (completedAsync, meowBotAction)) -> handleCompletedAsync conn completedAsync meowBotAction >> return (Just asyncMsgText)
    Right (Right proxyMsg)                       -> do
      lift $ putStrLn $ fromMaybe "喵喵" (nameOfBot mods) ++ " <- Proxy : " ++ take 512 (bsToString proxyMsg)
      lift $ sendTextData conn proxyMsg
      return (Just asyncMsgText)
  saveData prevData
  botLoop newAsyncMsg mods mode conn

-- | deregister the completed async action and do the bot action
handleCompletedAsync :: Connection -> Async (Meow [BotAction]) -> Meow [BotAction] -> StateT AllData IO ()
handleCompletedAsync conn completedAsync meowBotAction = do
  -- remove the completed async action from the set
  newAsyncSet <- gets (S.delete completedAsync . asyncActions . otherdata)
  modify $ \ad -> ad { otherdata = (otherdata ad) { asyncActions = newAsyncSet } }
  -- do the bot action in the Meow monad
  globalizeMeow $ meowBotAction >>= mapM_ (doBotAction conn)
  -- update the saved data if needed
  updateSavedAdditionalData

-- | handle the message from the server, also handles proxy messages
handleMessage :: BotModules -> RunningMode -> Connection -> BL.ByteString -> StateT AllData IO ()
handleMessage mods mode conn msgBS = do
  let eCQmsg  = eitherDecode msgBS :: Either String CQMessage
      nameBot = fromMaybe "喵喵" $ nameOfBot mods
  case traceModeWith DebugCQMessage mode (((nameBot ++ "debug: ") ++) . show) eCQmsg of
    Left errMsg -> lift $ putStrLn $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msgBS)
    Right cqmsg -> case eventType cqmsg of
      LifeCycle -> updateSelfInfo cqmsg >> doProxyWork (not . null $ mode) nameBot
      HeartBeat -> doProxyWork (not . null $ mode) nameBot
      Response -> do
        modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg
        updateSavedAdditionalData
        lift $ putStrLn $ nameBot ++ " <- response."
      PrivateMessage -> do
        updateStates nameBot cqmsg
        doBotCommands conn (filter ((`elem` canUsePrivateCommands mods) . identifier) allPrivateCommands)
        when (filterMsg cqmsg) $ doProxyWork True nameBot
      GroupMessage -> do
        updateStates nameBot cqmsg
        doBotCommands conn (filter ((`elem` canUseGroupCommands mods) . identifier) allGroupCommands)
        when (filterMsg cqmsg) $ doProxyWork True nameBot
      _ -> return ()
      where
        updateStates nameBot cqmsg = do
          cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
          modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg'
          updateSavedAdditionalData
          lift $ putStrLn $ nameBot ++ " <- " ++ showCQ cqmsg'
        filterMsg cqmsg' = any (`isPrefixOf` (unpack $ fromMaybe "" $ message cqmsg')) ["!", "！", "/"]
        doProxyWork shouldPrint nameBot | null (proxyTChans mods) = return ()
                                        | otherwise = do
          when shouldPrint $ lift $ putStrLn (nameBot ++ " -> Proxy ") >> putStr (bsToString msgBS ++ "\n")
          lift $ mapM_ (`sendToProxy` msgBS) (proxyTChans mods)
          makeHeader >>= \case
            Nothing      -> return ()
            Just headers -> do
              pending <- gets (pendingProxies . otherdata)
              lift . mapM_ (\pd -> runProxyWS pd headers) $ pending
              unless (null pending) $ modify $ \ad -> ad { otherdata = (otherdata ad) { pendingProxies = [] } }

initialData :: BotModules -> IO AllData
initialData mods = do
  fileExist <- doesFileExist (savedDataPath $ nameOfBot mods)
  if fileExist
    then do
      putStrLn "Found saved data file, loading data! owo"
      savedData <- readFile $ savedDataPath $ nameOfBot mods
      let msavedData = read savedData
      AllData [] mods . OtherData 0 Nothing [] msavedData mods (coerce $ savedAdditional msavedData) (proxyTChans mods) S.empty <$> getAllScripts
    else do
      putStrLn "No saved data file found, starting with empty data! owo"
      AllData [] mods . OtherData 0 Nothing [] (SavedData [] initialUGroups initialGGroups initialRules initialBooks []) mods [] (proxyTChans mods) S.empty <$> getAllScripts
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

