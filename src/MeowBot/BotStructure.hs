{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module MeowBot.BotStructure
  ( module MeowBot.Data
  , module Control.Monad.Readable
  , module Control.Monad.IOe
  --, Meow, MeowT(..), globalizeMeow
  --, Cat, CatT
  --, BotCommand(..)
  , BotModules(..), BotConfig(..)
  , GroupId(..), UserId(..), ChatId(..)
  --, BotAction(..)
  , AllData(..), OtherData(..), SavedData(..), Saved(..), SelfInfo(..)
  , UserGroup(..), GroupGroup(..)
  , MetaMessageItem(..)
  --, gIncreaseAbsoluteId, increaseAbsoluteId
  --, updateAllDataByMessage, updateAllDataByResponse, insertMyResponseHistory, updateSavedAdditionalData
  --, updateSelfInfo

  , CQMessage(..), ResponseData(..), CQEventType(..)

  , getEssentialContent, getEssentialContentAtN
  -- , sendIOeToChatId
  -- , sendIOeToChatIdAsync
  -- , baSendToChatId, sendToChatId, meowSendToChatIdFull
  , getFirstTree, getNewMsg, getNewMsgN
  , getTimeLine, getTimeLineCid

  , rseqWholeChat

  --, makeHeader
  , AdditionalData(..)
  ) where

import MeowBot.CommandRule
import MeowBot.Data
import Control.Parallel.Strategies
import Control.Monad.IOe
import Control.Monad.Readable
import Control.Monad.Trans.ReaderState
import Command.Aokana.Scripts
import Data.Additional
import Data.Maybe
import Data.Bifunctor
import Data.List (sortOn, maximumBy)
import Data.Additional.Saved
import Data.Ord (comparing, Down(..))
import MeowBot.Parser (ChatSetting(..))
import MeowBot.Data.Book
import MeowBot.Parser (Tree(..), flattenTree)
--import Database.Persist -- implement proper database later

-- newtype CatT r mods m a = CatT { runCatT :: SystemT r AllData mods m a }

data BotConfig = BotConfig
  { botModules  :: BotModules
  , runningMode :: RunningMode
  } deriving Show

data AllData = AllData
  { wholechat  :: WholeChat
  , botConfig  :: BotConfig
  , otherdata  :: OtherData
  } deriving Show

data SelfInfo = SelfInfo
  { selfId :: UserId
  } deriving Show

data OtherData = OtherData -- In the future one can add course data.. etc
  { message_number :: !Int -- ^ all messages, will be used to create an absolute message id number ordered by time of receipt or time of send.
  , selfInfo       :: !(Maybe SelfInfo)
  , sent_messages  :: ![CQMessage]
  , savedData      :: !SavedData
  , runningData    :: ![AdditionalData]  -- ^ additional data that is running, not saved.
  --, pendingProxies :: ![ProxyData]
  --, asyncActions   :: !(S.Set (Async (Meow [BotAction]))) -- ^ actions that are running asynchronously
  , aokana         :: [ScriptBlock]
  } deriving Show

instance {-# OVERLAPPABLE #-} Monad m => MonadReadable OtherData (ReaderStateT r (s0, AllData) m) where
  query = gets (otherdata . snd)
  {-# INLINE query #-}

instance {-# OVERLAPPABLE #-} Monad m => MonadModifiable OtherData (ReaderStateT r (s0, AllData) m) where
  change f = modify $ second $ \ad -> ad {otherdata = f $ otherdata ad}
  {-# INLINE change #-}

data SavedData = SavedData
  { chatSettings    :: [(ChatId, ChatSetting)]
  , userGroups      :: [(UserId, UserGroup)]
  , groupGroups     :: [(GroupId, GroupGroup)]
  , commandRules    :: [CommandRule]
  , books           :: [Book]
  , savedAdditional :: [Saved AdditionalData]
  } deriving (Show, Eq, Read)

-- data ReaderStateT r s m a = ReaderStateT {runReaderStateT :: r -> s -> m (a, s)}
-- CommandValue is a monadic value of the monad (Meow)

instance HasAdditionalData OtherData where
  getAdditionalData = runningData
  modifyAdditionalData f od = od {runningData = f $ runningData od}

rseqWholeChat :: Strategy AllData
rseqWholeChat (AllData wc m od) = do
  wc' <- evalList (evalTuple2 r0 rseq) wc
  od' <- rseq od
  return $ AllData wc' m od'

getNewMsg :: WholeChat -> CQMessage
getNewMsg [] = emptyCQMessage
getNewMsg wholechat = snd $ largestInTree (fromMaybe 0 . absoluteId) (getFirstTree wholechat)

getFirstTree :: WholeChat -> Tree CQMessage
getFirstTree wc =
  case wc of
    [] -> Node emptyCQMessage []
    (p:_) -> case p of
      (_, []) -> Node emptyCQMessage []
      (_, t0:_) -> t0

getNewMsgN :: Int -> WholeChat -> [CQMessage]
getNewMsgN _ [] = []
getNewMsgN n (headWholeChat:_) = take n $ concatMap flattenTree $ snd (headWholeChat)

getEssentialContent :: WholeChat -> Maybe EssentialContent
getEssentialContent wchat = cqmsgToEssentialContent (getNewMsg wchat)

getEssentialContentAtN :: Int -> WholeChat -> Maybe EssentialContent
getEssentialContentAtN n wchat = cqmsgToEssentialContent =<< (getNewMsgN n wchat !? (n-1))
  where (!?) :: [a] -> Int -> Maybe a
        (!?) [] _ = Nothing
        (!?) (x:_) 0 = Just x
        (!?) (_:xs) n = xs !? (n-1)

-- | get the timeline of the most recent chat, i.e. sort the chat room of the most recent message by time.
getTimeLine :: WholeChat -> [CQMessage]
getTimeLine ((_, forest):_) = sortOn (Down . time) $ concatMap flattenTree forest
getTimeLine [] = []

-- | Get the timeline of a chat id.
getTimeLineCid :: ChatId -> WholeChat -> [CQMessage]
getTimeLineCid cid wc = case lookup cid wc of
  Just forest -> sortOn (Down . time) . concatMap flattenTree $ forest
  Nothing -> []

largestInTree :: (Ord b) => (a -> b) -> Tree a -> (b, a)
largestInTree f (Node a children) =
  let currentValue = (f a, a)
      childValues = map (largestInTree f) children
  in case childValues of
              [] -> currentValue
              _  -> maximumBy (comparing fst) $ currentValue:childValues
largestInTree _ EmptyTree = error "largestInTree : EmptyTree"

