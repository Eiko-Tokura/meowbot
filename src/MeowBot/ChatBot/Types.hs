module MeowBot.ChatBot.Types where

import Data.Additional
import Data.Default
import Data.Maybe
import Data.Time.Clock
import External.ChatAPI hiding (SystemMessage)
import MeowBot
import Utils.List
import qualified Data.BSeq as BSeq
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import MeowBot.Data.CQHttp.CQCode
import MeowBot.Prelude
import Control.Lens

{- We wish to refactor the ChatBot pipeline in pure functions
   
   1. userInput :: CQMessage -> ChatState -> ChatState

   2. chatBotReaction :: ChatState -> [Message]

   3. merge :: [Message] -> ChatState -> ChatState
-}

newChatState = HM.empty :: HM.HashMap ChatId ChatState

data MeowStatus = MeowIdle | MeowBusy deriving (Show, Eq)

-- we will have to mantain a ChatState for each chat in the Chat command (not Cat command)
data ChatState = ChatState
  { chatStatus :: !ChatStatus
  , meowStatus :: !MeowStatus -- ^ avoids crafting too many messages simultaneously
  , activeTriggerOneOff :: !Bool
    -- ^ if set, this will override the active probability for this chat
    -- used to actively trigger a chat
    -- will be reset to Nothing after one chat run
  , replyTimes :: !(BSeq.BSeq 10 UTCTime)
  } deriving (Show, Eq)
makeLenses_ ''ChatStatus
makeLenses_ ''ChatState

_messages :: Lens' ChatState [Message]
_messages = _chatStatus . _chatStatusMessages

recordReplyTime :: UTCTime -> ChatState -> ChatState
recordReplyTime utcTime cs =
  cs { replyTimes = BSeq.bSeqCons utcTime (replyTimes cs) }

appendMessages :: [Message] -> ChatState -> ChatState
appendMessages msgs = _messages %~ (++ msgs)

type AllChatState = HM.HashMap ChatId ChatState -- since we are keeping it as state, use strict map
instance IsAdditionalData (HM.HashMap ChatId ChatState) where

instance Default MeowStatus where def = MeowIdle
instance Default ChatState where
  def = ChatState
    { chatStatus = ChatStatus 0 0 [] mempty
    , meowStatus = def
    , activeTriggerOneOff = False
    , replyTimes = BSeq.BSeq mempty
    }

-- | Adding the state for the cid if not exists, and applying f to it
updateChatState :: ChatId -> (ChatState -> ChatState) -> AllChatState -> AllChatState
updateChatState cid f = lensCidChatState cid %~ f

lensCidChatState :: Functor f => ChatId -> (ChatState -> f ChatState) -> AllChatState -> f AllChatState
lensCidChatState cid fs a = case HM.lookup cid a of
  Just cs -> (\newCs -> HM.insert cid newCs a) <$> fs cs
  Nothing -> (\newCs -> HM.insert cid newCs a) <$> fs def

markMeowStatus :: ChatId -> MeowStatus -> AllChatState -> AllChatState
markMeowStatus cid meowStat = lensCidChatState cid . _meowStatus .~ meowStat

mergeChatState :: Int -> [Message] -> ChatState -> ChatState
mergeChatState maxMessageInState newMsgs = _messages %~ (optimalMeowTakeTailKeepAvg maxMessageInState . (++ newMsgs))

clearChatState :: ChatState -> ChatState
clearChatState = const def

-- | Update the chat state map by inserting the new user message and checking active trigger.
--
-- In case of active trigger, it resets the flag and does not add the message.
updateAllChatStateTrigger :: Int -> ChatId -> ToUserMessageConfig -> CQMessage -> AllChatState -> (Bool, AllChatState)
updateAllChatStateTrigger maxMessageInState cid conf cqmsg = lensCidChatState cid $ \cs ->
  case activeTriggerOneOff cs of
    True -> ( True
            , cs
            & _activeTriggerOneOff .~ False
            )
    False -> ( False
             , mergeChatState maxMessageInState (maybeToList $ toMessage conf cqmsg) cs
             )

-- | If a text is empty, make it Nothing
nullify :: Maybe Text -> Maybe Text
nullify (Just s) | T.null s = Nothing
nullify x = x

selectedContent :: [Either CQCode Text] -> Text
selectedContent []                          = ""
selectedContent (Right t:rest)              = t <> selectedContent rest
selectedContent (Left (CQImage _):rest)     = "[CQ:image,url=<too_long>]"  <> selectedContent rest
selectedContent (Left (CQRecord _):rest)    = "[CQ:record,url=<too_long>]" <> selectedContent rest
selectedContent (Left cq@(CQAt {}):rest)    = embedCQCode cq <> selectedContent rest
selectedContent (Left cq@(CQReply {}):rest) = embedCQCode cq <> selectedContent rest
selectedContent (Left cq@(CQOther "face" _):rest)     = embedCQCode cq <> selectedContent rest
selectedContent (Left cq@(CQOther "markdown" _):rest) = embedCQCode cq <> selectedContent rest
selectedContent (Left cq@(CQOther "json" _):rest)     = embedCQCode cq <> selectedContent rest
selectedContent (Left (CQOther "image" meta):rest)
  = case filter (flip elem ["summary"] . fst) meta of
      []       -> "[CQ:image,data=<unknown_data>]"
      filtered -> embedCQCode (CQOther "image" filtered)
  <> selectedContent rest
selectedContent (Left (CQOther "mface" meta):rest)
  = case filter (flip elem ["summary"] . fst) meta of
      []       -> "[CQ:mface,data=<unknown_data>]"
      filtered -> embedCQCode (CQOther "mface" filtered)
  <> selectedContent rest
selectedContent (Left (CQOther cqtype _):rest)
  = "[CQ:" <> cqtype <> ",data=<unknown_data>]"
  <> selectedContent rest
selectedContent (Left _:rest) = selectedContent rest

newtype ToUserMessageConfig = ToUserMessageConfig
  { withUtcTime :: Bool
  }

toMessage :: ToUserMessageConfig -> CQMessage -> Maybe Message
toMessage conf cqm | (PrivateMessage; GroupMessage) <- eventType cqm = do
  ms <- selectedContent . mixedMessage <$> metaMessage cqm
  pure $ UserMessage $ mconcat $ catMaybes
    [ fmap (\r -> "<role>" <> r <> "</role>") (roleToText =<< senderRole =<< sender cqm)
    , fmap (\t -> "<msg_id>" <> toText t <> "</msg_id>") (messageId cqm)
    , fmap (\(UserId uid) -> "<user_id>" <> toText uid <> "</user_id>") (userId cqm)
    , fmap (\t -> "<username>" <> t <> "</username>") (senderNickname =<< sender cqm)
    , fmap (\t -> "<nickname>" <> t <> "</nickname>") (nullify $ senderCard =<< sender cqm)
    , fmap (\utc -> "<utc_time>" <> toText utc <> "</utc_time>") (if withUtcTime conf then utcTime cqm else Nothing)
    , Just ": "
    , Just ms
    ]
toMessage _ cqm | SelfMessage <- eventType cqm = do
  ms <- selectedContent . mixedMessage <$> cqm.metaMessage
  return $ AssistantMessage ms Nothing Nothing Nothing
toMessage _ _ = Nothing
