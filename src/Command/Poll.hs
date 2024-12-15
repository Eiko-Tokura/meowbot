{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeApplications, DerivingVia, DeriveAnyClass #-}
module Command.Poll where

import Command
import Command.Poll.PollData
import Control.Monad.Trans.ReaderState
import Control.Monad.Trans.Maybe
import Control.Monad.Logger
import Control.Monad
import Probability.Foundation
import Data.Additional
import Data.Maybe
import qualified Data.Text as T
import MeowBot.Parser
import MeowBot
import System.General
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

data PollEnv = PollGlobal | PollPrivate deriving (Show, Eq)

data PollCommand
  = CreatePoll PollEnv Text [Text] -- ^ title, options
  | Vote       PollId  [Int]       -- ^ pollId, optionId
  | Propose    PollId  Text        -- ^ pollId, option
  | ViewPoll   PollId
  | DeletePoll PollId
  | ListPoll
  deriving (Show, Eq)

pollParser :: (Chars sb) => Parser sb Char PollCommand
pollParser = do
  headCommand "poll"
  commandSeparator
  asumE
    [ createPollParser
    , voteParser
    , proposeParser
    , viewPollParser
    , deletePollParser
    , listPollParser
    ]
  where
    createPollParser = $(stringQ "create")  >> commandSeparator >>
      CreatePoll
        <$>
          ( ($(stringQ "global") >> commandSeparator >> return PollGlobal)
            <|> return PollPrivate
          )
        <*> word'
        <*> some (commandSeparator >> word')
    voteParser       = $(stringQ "vote")    >> commandSeparator >> Vote       <$> int <*> some (commandSeparator >> int)
    proposeParser    = $(stringQ "propose") >> commandSeparator >> Propose    <$> int <*> (commandSeparator >> some' item)
    viewPollParser   = $(stringQ "view")    >> commandSeparator >> ViewPoll   <$> int
    deletePollParser = $(stringQ "delete")  >> commandSeparator >> DeletePoll <$> int
    listPollParser   = $(stringQ "list")    >> return ListPoll

pollTreeParser :: (IsStream s CQMessage) => Parser s CQMessage PollCommand
pollTreeParser = do
  self <- satisfy $ \cqm -> eventType cqm == SelfMessage && (not . null $ getAdditionalDataSavedType @_ @PollId cqm)
  let pollId = head . getAdditionalDataSavedType @_ @PollId $ self
  umsg <- satisfy $ (`elem` [GroupMessage, PrivateMessage]) . eventType
  maybe zero return $ runParser (replyPollParser pollId) (maybe "" onlyMessage $ metaMessage umsg)
  where
    replyPollParser pollId
      = spaces0
      >>    (Vote pollId <$> intercalateBy commandSeparator int)
        <|> ($(stringQ "propose") >> commandSeparator >> Propose pollId <$> some' item)

commandPoll :: BotCommand
commandPoll = BotCommand Poll $ botT $ do
  ess@(str, _, _, _, _) <- MaybeT $ getEssentialContent <$> query
  tree <- lift $ getFirstTree <$> query
  pollParser' <- lift $ commandParserTransformByBotName pollParser
  case (runParser pollParser' str, runParser pollTreeParser tree) of
    (Just cmd, _) -> lift $ doPollCommand ess cmd
    (_, Just cmd) -> lift $ doPollCommand ess cmd
    _             -> return []

getPollMap :: (MonadIO m) => MeowT r mods m (M.Map PollId PollData)
getPollMap = do
  mPollMap <- listToMaybe . getAdditionalDataSavedType @_ @(M.Map PollId PollData) <$> query @OtherData
  case mPollMap of
    Just pm -> liftIO (print pm) >> return pm
    Nothing -> do
      let emptyMap = M.empty :: M.Map PollId PollData
      change @OtherData . modifyAdditionalData $ (:) $ AdditionalDataSaved emptyMap
      $(logInfo) "Poll map initialized!"
      s <- getAdditionalData <$> query @OtherData
      liftIO $ print s
      return emptyMap

doPollCommand :: InsertHistory r m => EssentialContent -> PollCommand -> MeowT r mods m [BotAction]
doPollCommand (_, cid, _, _, _) (CreatePoll env title options) = do
  pollMap <- getPollMap
  let newPollId = head [i | i <- [0..], i `notElem` M.keys pollMap] -- safe because of infinite list
      env' = case env of
        PollGlobal -> Nothing
        PollPrivate -> Just cid
      newPollMap = traceShowId $
        M.insert
          newPollId
          (PollData newPollId env' title (M.fromList $ zip [0..] options) M.empty)
          pollMap
  change @OtherData . modifyAdditionalDataSavedType $ const $ Just newPollMap
  meowSendToChatIdFull cid Nothing [AdditionalDataSaved newPollId] [] $ T.intercalate "\n" $
    [ "Poll created! owo"
    , "Poll ID: " <> tshow newPollId
    , "Title: " <> title
    ] ++ [ tshow @Int i <> ". " <> option | (i, option) <- zip [0..] options ]
    ++
    [ "---"
    ] ++
    [ "投票:回复此消息选项编号（空格分隔）"
    , "提议新选项:回复此消息propose <option>"
    ]
doPollCommand (_, cid, uid, _, _) (Vote pid optionIds) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
      random <- liftIO $ (<= 10) <$> getUniformR (0, 100 :: Int)
      let newVotes = M.insert uid (S.fromList optionIds) $ pollVotes poll
          newMap   = M.insert pid poll { pollVotes = newVotes } pollMap
          hint = if random then "使用 :poll view " <> T.pack (show pid) <> "查看" else ""
      change @OtherData . modifyAdditionalDataSavedType $ const $ Just newMap
      -- let statsStrs = intercalate "\n" [ option ++ ": " ++ show votes | (option, votes) <- pollStatistics poll ]
      return [baSendToChatId cid $ "记下来了！>w<" <> hint]
      else return [baSendToChatId cid "Poll not visible o.o!"]
doPollCommand (_, cid, _, _, _) (Propose pid option) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
        let newOptions = M.insert (M.size $ pollOptions poll) option $ pollOptions poll
            newMap     = M.insert pid poll { pollOptions = newOptions } pollMap
        change @OtherData . modifyAdditionalDataSavedType $ const $ Just newMap
        return [baSendToChatId cid "Option proposed! owo"]
      else return [baSendToChatId cid "Poll not visible o.o!"]
doPollCommand (_, cid, _, _, _) (ViewPoll pid) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
        let statsStrs = T.intercalate "\n" [ tshow oid <> ". " <> option <> ": " <> tshow votes | (oid, option, votes) <- pollStatistics poll ]
        meowSendToChatIdFull cid Nothing [AdditionalDataSaved (pollId poll)] [] $ "Poll: " <> pollTitle poll <> "\n" <> statsStrs
      else return [baSendToChatId cid "Poll not visible o.o!"]
doPollCommand (_, cid, _, _, _) ListPoll = do
  pollMap <- getPollMap
  let pollStrs = [ tshow pid <> ". " <> pollTitle poll | (pid, poll) <- M.toList pollMap, readablePoll cid poll ]
  return [baSendToChatId cid $ "Polls:\n" <> T.intercalate "\n" pollStrs]
doPollCommand (_, cid, _, _, _) (DeletePoll pid) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
        let newMap = M.delete pid pollMap
        change @OtherData . modifyAdditionalDataSavedType $ const $ Just newMap
        return [baSendToChatId cid "Poll deleted! owo"]
      else return [baSendToChatId cid "Poll not visible o.o!"]

readablePoll :: ChatId -> PollData -> Bool
readablePoll cid poll = case pollEnv poll of
  Nothing -> True
  Just cid' -> cid == cid'

updateWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateWhere _ _ [] = []
updateWhere p f (x:xs) = if p x then f x : xs else x : updateWhere p f xs

updateInsertWhere :: (a -> Bool) -> a -> (a -> a) -> [a] -> [a]
updateInsertWhere _ x0 f [] = [f x0]
updateInsertWhere p x0 f (x:xs) = if p x then f x : xs else x : updateInsertWhere p x0 f xs

helpPoll :: T.Text
helpPoll = T.pack $ unlines
  [ "Poll command:"
  , "  :poll create <title> <options> - create a poll"
  , "  :poll create global <title> <options> - create a global poll, visible in all groups"
  , "  :poll vote <pollId> <optionIds> - vote for options"
  , "  :poll propose <pollId> <option> - propose new options"
  , "  :poll view <pollId> - view the poll"
  , "  :poll list - list all (visible) polls"
  ]


pollParserTest :: IO ()
pollParserTest = do
  let testPairs =
        [ (":poll create test 1 2 3", CreatePoll PollPrivate "test" ["1", "2", "3"])
        , (":poll create global \"owo poll\" \"option 1\" \"option 2\"", CreatePoll PollGlobal "owo poll" ["option 1", "option 2"])
        ]
  forM_ testPairs $ \(input, expected) -> do
    let result = runParser pollParser input
    putStrLn $ "Input: " ++ input
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Match: " ++ show (result == Just expected)
    if result /= Just expected
      then putStrLn "Failed!" >> error "pollParserTest failed!"
      else putStrLn "Passed!"

