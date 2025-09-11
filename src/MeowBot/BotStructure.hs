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
  , BotModules(..), BotConfig(..), OverrideSettings(..)
  , GroupId(..), UserId(..), ChatId(..)
  --, BotAction(..)
  , AllData(..), OtherData(..), SavedData(..), Saved(..), SelfInfo(..), GroupInfo(..)
  , UserGroup(..), GroupGroup(..)
  , MetaMessageItem(..)
  --, gIncreaseAbsoluteId, increaseAbsoluteId
  --, updateAllDataByMessage, updateAllDataByResponse, insertMyResponseHistory, updateSavedAdditionalData
  --, updateSelfInfo

  , CQMessage(..), ResponseData(..), CQEventType(..)

  , getEssentialContent, getEssentialContentAtN, getEssentialContentChatId
  -- , sendIOeToChatId
  -- , sendIOeToChatIdAsync
  -- , baSendToChatId, sendToChatId, meowSendToChatIdFull
  , getFirstTree, getNewMsg, getNewMsgN, getNewMsgChatIdN
  , getTimeLine, getTimeLineCid

  , rseqWholeChat, rseqSavedData

  --, makeHeader
  , AdditionalData(..)
  ) where

import Command.Aokana.Scripts
import Control.Monad.IOe
import Control.Monad.Readable
import Control.Monad.Trans.ReaderState
import Control.Parallel.Strategies
import Data.Additional
import Data.Additional.Saved
import Data.Bifunctor
import Data.List (sortOn, maximumBy)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord (comparing, Down(..))
import Data.UpdateMaybe
import MeowBot.CommandRule
import MeowBot.Data
import MeowBot.Data.Book
import MeowBot.Parser ( ChatSetting(..), Tree(..) )

data BotConfig = BotConfig
  { botModules       :: BotModules
  , runningMode      :: RunningMode
  , overrideSettings :: Maybe OverrideSettings
  } deriving Show

data OverrideSettings = OverrideSettings
  { chatIdOverride :: Maybe ChatId
  } deriving Show

data AllData = AllData
  { wholechat  :: WholeChat
  , botConfig  :: BotConfig
  , otherdata  :: !OtherData
  } deriving Show

data SelfInfo = SelfInfo
  { selfId       :: UserId
  , selfInGroups :: UMaybeTime (Map GroupId (UMaybeTime GroupInfo))
  } deriving Show

data GroupInfo = GroupInfo
  { selfRole         :: Role
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

-- | We will now save this data to the database, no longer in a file.
data SavedData = SavedData
  { chatSettings    :: [(ChatId, ChatSetting)]
  , userGroups      :: [(UserId, UserGroup)]
  , groupGroups     :: [(GroupId, GroupGroup)]
  , commandRules    :: [CommandRule]
  , books           :: [Book]
  , savedAdditional :: [Saved AdditionalData]
  } deriving (Show, Eq, Read)

data UserAndGroupInfo = UserAndGroupInfo
  { userGroups :: [(UserId, UserGroup)]
  , groupGroups :: [(GroupId, GroupGroup)]
  } deriving (Show, Eq, Read)

rseqSavedData :: Strategy SavedData
rseqSavedData (SavedData cs ug gg cr b sa) = do
  cs' <- rseq cs
  ug' <- rseq ug
  gg' <- rseq gg
  cr' <- rseq cr
  b'  <- rseq b
  sa' <- evalList rseq sa
  return $ SavedData cs' ug' gg' cr' b' sa'

-- data ReaderStateT r s m a = ReaderStateT {runReaderStateT :: r -> s -> m (a, s)}
-- CommandValue is a monadic value of the monad (Meow)

instance HasAdditionalData OtherData where
  getAdditionalData = runningData
  modifyAdditionalData f od = od {runningData = f $ runningData od}

rseqWholeChat :: Strategy AllData
rseqWholeChat (AllData wc m od) = do
  wc' <- evalList (evalTuple2 r0 (evalTuple2 rseq rseq)) wc
  od' <- rseq od
  return $ AllData wc' m od'

getNewMsg :: WholeChat -> CQMessage
getNewMsg [] = emptyCQMessage
getNewMsg wholechat = snd $ largestInTree (fromMaybe 0 . absoluteId) (getFirstTree wholechat)

getFirstTree :: WholeChat -> Tree CQMessage
getFirstTree ((_cid, (tree0:_, _cqmsgs)):_) = tree0
getFirstTree _                              = Node emptyCQMessage []

-- | Get n most recent chat messages as a list of CQMessage
getNewMsgN :: Int -> WholeChat -> [CQMessage]
getNewMsgN _ [] = []
getNewMsgN n (headWholeChat:_) = take n $ snd $ snd headWholeChat

getNewMsgChatIdN :: Int -> WholeChat -> Maybe (ChatId, [CQMessage])
getNewMsgChatIdN _ [] = Nothing
getNewMsgChatIdN n (headWholeChat:_) = Just $ second (take n . snd) headWholeChat

-- | Get the most important informations from the most recent one chat message
getEssentialContent :: WholeChat -> Maybe EssentialContent
getEssentialContent wchat = cqmsgToEssentialContent (getNewMsg wchat)

getEssentialContentChatId :: ChatId -> WholeChat -> Maybe EssentialContent
getEssentialContentChatId cid wchat = listToMaybe (getTimeLineCid cid wchat) >>= cqmsgToEssentialContent

-- | Get the most important informations from the n-th chat message
getEssentialContentAtN :: Int -> WholeChat -> Maybe EssentialContent
getEssentialContentAtN n wchat = cqmsgToEssentialContent =<< (getNewMsgN n wchat !? (n-1))
  where (!?) :: [a] -> Int -> Maybe a
        (!?) [] _     = Nothing
        (!?) (x:_) 0  = Just x
        (!?) (_:xs) n = xs !? (n-1)

-- | get the timeline of the most recent chat, i.e. sort the chat room of the most recent message by time.
getTimeLine :: WholeChat -> [CQMessage]
getTimeLine ((_, (_, timeline)):_) = sortOn (Down . time) timeline
getTimeLine [] = []

-- | Get the timeline of a chat id.
getTimeLineCid :: ChatId -> WholeChat -> [CQMessage]
getTimeLineCid cid wc = case lookup cid wc of
  Just (_, tl) -> sortOn (Down . time) tl
  Nothing -> []

largestInTree :: (Ord b) => (a -> b) -> Tree a -> (b, a)
largestInTree f (Node a children) =
  let currentValue = (f a, a)
      childValues = map (largestInTree f) children
  in case childValues of
              [] -> currentValue
              _  -> maximumBy (comparing fst) $ currentValue:childValues
largestInTree _ EmptyTree = error "largestInTree : EmptyTree"
