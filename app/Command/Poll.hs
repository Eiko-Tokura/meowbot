{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeApplications, DerivingVia, DeriveAnyClass #-}
module Command.Poll where

import Command
import Control.Monad.Trans.ReaderState
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Probability.Foundation
import Data.Additional
import Data.Maybe
import Data.Typeable
import qualified Data.Text as T
import MeowBot.Parser
import MeowBot.BotStructure
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

newtype PollId = PollId Int deriving (Show, Read, Num, Real, Enum, Integral, Eq, Ord, Typeable) via Int

instance IsAdditionalData PollId

data PollData = PollData
  { pollId      :: PollId
  , pollEnv     :: Maybe ChatId -- ^ ChatId if the poll is in a group, Nothing if the poll is global
  , pollTitle   :: Text
  , pollOptions :: M.Map Int Text
  , pollVotes   :: M.Map UserId (S.Set Int)
  } deriving (Show, Eq, Typeable, IsAdditionalData)

pollStatistics :: PollData -> [(Int, Text, Int)]
pollStatistics poll =
  let optionVotes = M.fromListWith (+) [ (optionId, 1) | optionId <- concatMap S.toList $ M.elems $ pollVotes poll ]
  in [ (optionId, option, M.findWithDefault 0 optionId optionVotes) | (optionId, option) <- M.toList $ pollOptions poll ]

instance IsAdditionalData (M.Map PollId PollData)

data PollEnv = PollGlobal | PollPrivate

data PollCommand
  = CreatePoll PollEnv Text [Text] -- ^ title, options
  | Vote       PollId  [Int]    -- ^ pollId, optionId
  | Propose    PollId  Text   -- ^ pollId, option
  | ViewPoll   PollId
  | ListPoll

pollParser :: (Chars sb) => Parser sb Char PollCommand
pollParser = do
  headCommand "poll"
  commandSeparator
  asumE
    [ createPollParser
    , voteParser
    , proposeParser
    , viewPollParser
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
    listPollParser   = $(stringQ "list")    >> return ListPoll

pollTreeParser :: (Stream s CQMessage) => Parser s CQMessage PollCommand
pollTreeParser = do
  self <- satisfy $ \cqm -> eventType cqm == SelfMessage && (not . null $ getAdditionalDataType @_ @PollId cqm)
  let pollId = head . getAdditionalDataType @_ @PollId $ self
  umsg <- satisfy $ (`elem` [GroupMessage, PrivateMessage]) . eventType
  maybe zero return $ runParser (replyPollParser pollId) (maybe "" onlyMessage $ metaMessage umsg)
  where
    replyPollParser pollId
      = spaces0
      >>    (Vote pollId <$> intercalateBy commandSeparator int)
        <|> ($(stringQ "propose") >> commandSeparator >> Propose pollId <$> some' item)

commandPoll :: BotCommand
commandPoll = BotCommand Poll $ botT $ do
  ess@(str, _, _, _) <- MaybeT $ getEssentialContent <$> ask
  tree <- lift $ getFirstTree <$> ask
  pollParser' <- lift $ commandParserTransformByBotName pollParser
  case (runParser pollParser' str, runParser pollTreeParser tree) of
    (Just cmd, _) -> lift $ doPollCommand ess cmd
    (_, Just cmd) -> lift $ doPollCommand ess cmd
    _             -> return []

getPollMap :: (MonadIO m) => ReaderStateT r OtherData m (M.Map PollId PollData)
getPollMap = do
  mPollMap <- listToMaybe . getAdditionalDataType @_ @(M.Map PollId PollData) <$> get
  case mPollMap of
    Just pm -> liftIO (print pm) >> return pm
    Nothing -> do
      let emptyMap = M.empty :: M.Map PollId PollData
      modify . modifyAdditionalData $ (:) $ AdditionalData emptyMap
      liftIO $ putStrLn "Poll map initialized!"
      s <- getAdditionalData <$> get
      liftIO $ print s
      return emptyMap

doPollCommand :: (MonadIO m) => EssentialContent -> PollCommand -> ReaderStateT r OtherData m [BotAction]
doPollCommand (_, cid, _, _) (CreatePoll env title options) = do
  pollMap <- getPollMap
  let newPollId = PollId $ M.size pollMap
      env' = case env of
        PollGlobal -> Nothing
        PollPrivate -> Just cid
      newPollMap = traceShowId $
        M.insert
          newPollId
          (PollData newPollId env' title (M.fromList $ zip [0..] options) M.empty)
          pollMap
  modify . modifyAdditionalDataType $ const $ Just newPollMap
  baSendToChatIdFull cid Nothing [AdditionalData newPollId] [] $ T.intercalate "\n" $
    [ "Poll created! owo"
    , "Poll ID: " <> tshow newPollId
    , "Title: " <> title
    ] ++ [ tshow i <> ". " <> option | (i, option) <- zip [0..] options ]
    ++
    [ "---"
    ] ++
    [ "投票:回复此消息选项编号（空格分隔）"
    , "提议新选项:回复此消息propose <option>"
    ]
doPollCommand (_, cid, uid, _) (Vote pid optionIds) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
      random <- liftIO $ (<= 10) <$> getUniformR (0, 100 :: Int)
      let newVotes = M.insert uid (S.fromList optionIds) $ pollVotes poll
          newMap   = M.insert pid poll { pollVotes = newVotes } pollMap
          hint = if random then "使用 :poll view " <> T.pack (show pid) <> "查看" else ""
      modify . modifyAdditionalDataType $ const $ Just newMap
      -- let statsStrs = intercalate "\n" [ option ++ ": " ++ show votes | (option, votes) <- pollStatistics poll ]
      return [baSendToChatId cid $ "记下来了！>w<" <> hint]
      else return [baSendToChatId cid "Poll not visible o.o!"]
doPollCommand (_, cid, _, _) (Propose pid option) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
        let newOptions = M.insert (M.size $ pollOptions poll) option $ pollOptions poll
            newMap     = M.insert pid poll { pollOptions = newOptions } pollMap
        modify . modifyAdditionalDataType $ const $ Just newMap
        return [baSendToChatId cid "Option proposed! owo"]
      else return [baSendToChatId cid "Poll not visible o.o!"]
doPollCommand (_, cid, _, _) (ViewPoll pid) = do
  pollMap <- getPollMap
  case M.lookup pid pollMap of
    Nothing -> return [baSendToChatId cid "Poll not found o.o!"]
    Just poll -> if readablePoll cid poll
      then do
        let statsStrs = T.intercalate "\n" [ tshow oid <> ". " <> option <> ": " <> tshow votes | (oid, option, votes) <- pollStatistics poll ]
        baSendToChatIdFull cid Nothing [AdditionalData (pollId poll)] [] $ "Poll: " <> pollTitle poll <> "\n" <> statsStrs
      else return [baSendToChatId cid "Poll not visible o.o!"]
doPollCommand (_, cid, _, _) ListPoll = do
  pollMap <- getPollMap
  let pollStrs = [ tshow pid <> ". " <> pollTitle poll | (pid, poll) <- M.toList pollMap, readablePoll cid poll ]
  return [baSendToChatId cid $ "Polls:\n" <> T.intercalate "\n" pollStrs]

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
