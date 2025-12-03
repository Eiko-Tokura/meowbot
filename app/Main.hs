{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, TemplateHaskell, MultilineStrings #-}
module Main where

import Command
import MeowBot.BotStructure
import Parser.Run
import Parser.Except
import Control.Concurrent.STM

import Data.List (isPrefixOf)
import Data.Maybe
import Data.Either

import Control.Monad.Trans
import Control.Monad.Logger
import Control.Concurrent
import Control.Monad.Effect
import Control.System

import System.Environment
import System.Cat
import Module.BotGlobal
import Module.Logging
import Module.Logging.Logger
import Module.Database (DB)
import Module.Database.Sqlite
import Module.Database.PostgreSql
import Module.Prometheus
import Module.Prometheus.Manager
import Module.ConnectionManager
import qualified Data.Text.IO as TIO
import Data.PersistModel (migrateCore)
import Data.PersistModel.Data (migrateData)
import qualified Data.ByteString.Char8 as BC
import Text.Read (readMaybe)

import Debug.Trace

-- | A tracing function that will only print the message when the flag is in the list.
traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a
  | flag `elem` ls = trace (f a) a
  | otherwise      = a


parseArgs :: ParserE [String] String String ([String], [BotInstance])
parseArgs = (,) <$>
  (fmap (fromMaybe []) . optMaybe $ (liftR1 just "+GLOBAL" >> liftR2 manyTill (just "-GLOBAL") getItem <* liftR1 just "-GLOBAL"))
  <*>
  many (do
    runFlag <- asum
      [ liftR1 just "--run-client" >> RunClient <$> withE "Usage: --run-client <ip> <port>" nonFlag <*> readE "cannot read the port number" nonFlag
      , liftR1 just "--run-server" >> RunServer <$> withE "Usage: --run-server <ip> <port>" nonFlag <*> readE "cannot read the port number" nonFlag
      ]
    localFlags <- fmap (fromMaybe []) . optMaybe $ liftR1 just "+LOCAL" >> liftR2 manyTill (just "-LOCAL") getItem <* liftR1 just "-LOCAL"
    restFlags <- many (identityParser |+| commandParser |+| debugParser |+| proxyParser |+| logParser |+| (watchDogParser2 <|> watchDogParser) |+| unrecognizedFlag)
    return $ BotInstance runFlag (lefts restFlags) (lefts $ rights restFlags) (lefts $ rights $ rights restFlags) (lefts $ rights $ rights $ rights restFlags) (lefts $ rights $ rights $ rights $ rights restFlags) (lefts $ rights $ rights $ rights $ rights $ rights restFlags) localFlags
  ) <* liftR end
    where
      identityParser = asum
        [ liftR1 just "--name" >> withE "--name needs a String argument" (UseName <$> nonFlag)
        , liftR1 just "--id"   >> UseId . BotId <$> readE "cannot read the id number" nonFlag
        , liftR1 just "--sys-msg" >> withE "--sys-msg needs a String argument" (UseSysMsg <$> nonFlag)
        ]
      commandParser = do
        liftR1 just "--command"
        addE ("--command needs exactly one commandId argument, " ++ commandIdHint) (CommandFlag <$> readE commandIdHint nonFlag)
      debugParser = liftR $ asum [ just "--debug-json" >> return DebugJson, just "--debug-cqmsg" >> return DebugCQMessage, just "--debug-other" >> DebugOther <$> getItem ]
      proxyParser = do
        liftR1 just "--proxy"
        ProxyFlag <$> withE "Usage: --proxy <address> <port>" nonFlag <*> readE "cannot read the port number" nonFlag
      logParser = LogFlag <$> liftR1 just "--log" >> withE "--log needs a file path argument" (LogFlag <$> nonFlag)
      watchDogParser  = liftR1 just "--watchdog" >> WatchDogFlag <$> readE "--watchdog <interval> <action>" nonFlag <*> withE "--watchdog <interval> <action>" (SystemCmd <$> nonFlag)
      watchDogParser2 = liftR1 just "--watchdog-send-to-uid" >> WatchDogFlag <$> readE "--watchdog-send-to-uid <interval> <uid>" nonFlag <*> (SendToId <$> readE "--watchdog-send-to-uid <interval> <uid>" nonFlag)
      unrecognizedFlag = do
        flag <- liftR $ require ((&&) <$> ("--" `isPrefixOf`) <*> (`notElem` ["--run-client", "--run-server"])) getItem
        lift $ throwE $ "Unrecognized flag " ++ flag
      nonFlag = require (not . ("--" `isPrefixOf`)) getItem
      commandIdHint = "commandId must be one of " ++ show [minBound..maxBound :: CommandId]

meowPostgresFromEnv :: String -> Maybe String -> Migration -> EffT '[] '[Text] IO (ModuleInitData (DB name PostgreSql))
meowPostgresFromEnv prefix mDefConnString mig = do
  connString <- liftIO (lookupEnv $ prefix <> "DB_CONN_STRING") >>= \case
    Nothing -> pureEitherInWith id $ maybe (Left @Text $ "Environment variable " <> toText prefix <>"DB_CONN_STRING is not set.") Right mDefConnString
    Just "" -> effThrow @Text $ "Environment variable " <> toText prefix <> "DB_CONN_STRING is not set or empty."
    Just r  -> return r
  poolSize   <- liftIO $ (\ms -> fromMaybe 2 $ readMaybe =<< ms) <$> lookupEnv (prefix <> "DB_POOL_SIZE")
  migAction  <- liftIO $ (readMaybe =<<) <$> lookupEnv (prefix <> "DB_MIGRATION_ACTION")
  return $ PostgreSqlInitData (BC.pack connString) mig poolSize migAction

-- | The main function of the bot.
--  It will parse the command line arguments and start the bot.
--
--  you can run a debuger on port 2077
--  by changing to main = withGhcDebugTCP "127.0.0.1" 2077 $ do
main :: IO ()
main = runEffT00 $ flip effCatch (\(e :: Text) -> liftIO $ TIO.putStrLn e) $ do
  args       <- liftIO getArgs
  let parsed = runParserE argumentHelp parseArgs args
      globalFlags = either (const []) fst parsed

  stdoLogger <- liftIO createSimpleConcurrentStdoutBaseLogger
  fileLogger <- liftIO $ createFileLogger "meowbot.log"
  let baseLogger = stdoLogger <> fileLogger

  loggerInit <- pureEitherInWith id $ defaultLoggingFromArgs (simpleLogger True baseLogger.baseLogFunc) (Just baseLogger.cleanUpFunc) globalFlags
  coreDbInit <- meowPostgresFromEnv "CORE_" (Just "host=localhost dbname=meow_core_db user=meowbot password='meowbot' port=5432") migrateCore
  -- defaultPostgresFromArgs (Just "meowbot") migrateCore globalFlags
  dataDbInit <- meowPostgresFromEnv "DATA_" Nothing migrateData
  -- defaultPostgresFromArgs Nothing migrateData globalFlags
  promInit   <- pureEitherInWith id $ defaultPrometheusFromArgs Nothing (Just 6001) (Just ["metrics"]) globalFlags
  botGlobal  <- liftIO $ BotGlobalRead <$> newTVarIO [] <*> newTVarIO mempty

  flip effCatch (\(e :: ErrorText "database_print_migration") -> liftIO $ TIO.putStrLn (toText e)) $ withModule loggerInit $ withModule promInit $ withPrometheusMan $ withModule coreDbInit $ withModule dataDbInit $ withConnectionManager $ runBotGlobal botGlobal $ do
    $logDebug $ pack $ "Arguments: " ++ show args
    $logDebug $ pack $ "Parsed: " ++ show parsed
    case parsed of
      Left errMsg     -> $(logError) (pack errMsg)
      Right (_, [])   -> embedNoError $ runBots [BotInstance (RunClient "127.0.0.1" 3001) [] [] [] [] [] [] []] >> halt
      Right (_, bots) -> embedNoError $ runBots bots >> halt
  where halt = lift (threadDelay maxBound) >> halt
        argumentHelp =
          """
          Usage: MeowBot [+GLOBAL [other global flags] -GLOBAL] [--run-client <ip> <port> | --run-server <ip> <port>] [--name <name>] [--sys-msg <msg>] [--command <commandId>] [--debug-json] [--debug-cqmsg] [--proxy <address> <port>]
            --run-client <ip> <port>  : run the bot as a client connecting to the go-cqhttp WebSocket server
            --run-server <ip> <port>  : run the bot as a server, using reverse WebSocket connection
            --name <name>             : set the name of the bot, with empty default to \"喵喵\"
            --id <id : int>           : set the id of the bot, necessary if running separated bots, default to 0
            --sys-msg <msg>           : set the global system message of the bot
            --command <commandId>     : allow the bot to use the command with the given commandId, use multiple --command flags to allow multiple commands
                                        commandId must be one of " ++ show [minBound..maxBound :: CommandId
                                        if no --command flags are given, the bot will use all commands
            --debug-json              : print the JSON message received from the server
            --debug-cqmsg             : print the decoded CQMessage
            --proxy <address> <port>  : set the proxy server to connect to, use multiple --proxy flags to connect to multiple servers
            If no arguments are given, the bot will run as a client connecting to the go-cqhttp WebSocket server on 127.0.0.1:3001
          
          Multiple bots can be started by using multiple sets of flags, starting with a run flag followed by other flags.
          """

