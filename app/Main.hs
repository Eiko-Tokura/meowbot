{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Command
import MeowBot.BotStructure
import Parser.Run
import Parser.Except

import Data.List (isPrefixOf)
import Data.Either

import Control.Monad.Trans
import Control.Concurrent

import System.Environment
import System.Cat
import System.Logging

-- import GHC.Conc (forkIO)
-- import GHC.Debug.Stub
import Debug.Trace

-- | A tracing function that will only print the message when the flag is in the list.
traceModeWith :: DebugFlag -> RunningMode -> (a -> String) -> a -> a
traceModeWith flag ls f a
  | flag `elem` ls = trace (f a) a
  | otherwise      = a


parseArgs :: ParserE [String] String String [BotInstance]
parseArgs = many (do
  runFlag <- asum
    [ liftR1 just "--run-client" >> RunClient <$> withE "Usage: --run-client <ip> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
    , liftR1 just "--run-server" >> RunServer <$> withE "Usage: --run-server <ip> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
    ]
  restFlags <- many (identityParser |+| commandParser |+| debugParser |+| proxyParser |+| logParser |+| watchDogParser |+| unrecognizedFlag)
  return $ BotInstance runFlag (lefts restFlags) (lefts $ rights restFlags) (lefts $ rights $ rights restFlags) (lefts $ rights $ rights $ rights restFlags) (lefts $ rights $ rights $ rights $ rights restFlags) (lefts $ rights $ rights $ rights $ rights $ rights restFlags)
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
      debugParser = liftR $ asum [ just "--debug-json" >> return DebugJson, just "--debug-cqmsg" >> return DebugCQMessage ]
      proxyParser = do
        liftR1 just "--proxy"
        ProxyFlag <$> withE "Usage: --proxy <address> <port>" nonFlag <*> (readE "cannot read the port number" nonFlag)
      logParser = LogFlag <$> liftR1 just "--log" >> withE "--log needs a file path argument" (LogFlag <$> nonFlag)
      watchDogParser = liftR1 just "--watchdog" >> WatchDogFlag <$> readE "--watchdog <interval> <action>" nonFlag <*> withE "--watchdog <interval> <action>" nonFlag
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
main :: IO () --runLoggingConcurrent (myLogger "meowbot.log")
main = do
  args <- getArgs
  runLoggingConcurrent (myLogger True
      (  [DebugCQMessage | "--debug-cqmsg" `elem` args]
      <> [DebugJson | "--debug-json" `elem` args]
      )
      ["meowbot.log"]) $ do
    $(logDebug) $ pack $ "Arguments: " ++ show args
    case runParserE argumentHelp parseArgs args of
      Left errMsg -> $(logError) (pack errMsg)
      Right []    -> runBots allInitDataG [BotInstance (RunClient "127.0.0.1" 3001) [] [] [] [] [] []] >> halt
      Right bots  -> runBots allInitDataG bots >> halt
  where halt = lift (threadDelay maxBound) >> halt
        argumentHelp = unlines
              [ "Usage: MeowBot [--run-client <ip> <port> | --run-server <ip> <port>] [--name <name>] [--sys-msg <msg>] [--command <commandId>] [--debug-json] [--debug-cqmsg] [--proxy <address> <port>]"
              , "  --run-client <ip> <port>  : run the bot as a client connecting to the go-cqhttp WebSocket server"
              , "  --run-server <ip> <port>  : run the bot as a server, using reverse WebSocket connection"
              , "  --name <name>             : set the name of the bot, with empty default to \"喵喵\""
              , "  --id <id : int>           : set the id of the bot, necessary if running separated bots, default to 0"
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
