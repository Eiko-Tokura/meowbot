{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module MeowBot.BotStructure
  ( module MeowBot.Data
  , MonadReadOnly(..), MonadReadable(..)
  , module MeowBot.BotStructure

  , BotModules(..), BotConfig(..), OverrideSettings(..)
  , GroupId(..), UserId(..), ChatId(..)

  , AllData(..), OtherData(..), SavedData(..), Saved(..), SelfInfo(..), GroupInfo(..)
  , UserGroup(..), GroupGroup(..)
  , MetaMessageItem(..)

  , CQMessage(..), ResponseData(..), CQEventType(..)
  , AdditionalData(..)
  ) where

import Command.Aokana.Scripts
import Control.Monad.RS.Class
import Module.RS
import Control.Monad.Effect
import Control.Parallel.Strategies
import Data.Additional
import Data.Additional.Saved
import Data.Bifunctor
import Data.List (sortOn, maximumBy)
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Ord (comparing, Down(..))
import Data.UpdateMaybe
import Module.Logging
import MeowBot.CommandRule
import MeowBot.Data
import MeowBot.Data.Book
import MeowBot.Parser ( ChatSetting(..), Tree(..) )
import Utils.Lens

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
  , selfInGroups :: !(UMaybeTime (HashMap GroupId (UMaybeTime GroupInfo)))
  } deriving Show

data GroupInfo = GroupInfo
  { selfRole         :: !Role
  } deriving Show

-- | We will now save this data to the database, no longer in a file.
data SavedData = SavedData
  { chatSettings    :: ![(ChatId, ChatSetting)]
  , userGroups      :: ![(UserId, UserGroup)]
  , groupGroups     :: ![(GroupId, GroupGroup)]
  , commandRules    :: ![CommandRule]
  , books           :: [Book]
  , savedAdditional :: ![Saved AdditionalData]
  } deriving (Show, Eq, Read)

data OtherData = OtherData -- In the future one can add course data.. etc
  { message_number :: !Int -- ^ all messages, will be used to create an absolute message id number ordered by time of receipt or time of send.
  , selfInfo       :: !(Maybe SelfInfo)
  , sent_messages  :: !(Seq CQMessage)
  , savedData      :: !SavedData
  , runningData    :: ![AdditionalData]  -- ^ additional data that is running, not saved.
  --, pendingProxies :: ![ProxyData]
  --, asyncActions   :: !(S.Set (Async (Meow [BotAction]))) -- ^ actions that are running asynchronously
  , aokana         :: [ScriptBlock]
  } deriving Show

makeLenses_ ''OtherData
makeLenses_ ''SavedData
makeLenses_ ''AllData
makeLenses_ ''SelfInfo
makeLenses_ ''GroupInfo

instance {-# OVERLAPPABLE #-} (SModule OtherData `In` mods, Monad m) => MonadReadOnly OtherData (EffT mods es m) where
  query = getS
  {-# INLINE query #-}
instance {-# OVERLAPPABLE #-} (SModule OtherData `In` mods, Monad m) => MonadReadable OtherData (EffT mods es m) where
  local = localByState
  {-# INLINE local #-}

localByState :: (SModule a `In` mods, Monad m) => (a -> a) -> EffT mods es m b -> EffT mods es m b
localByState f act = do
    a0 <- getS
    putS (f a0)
    r <- act
    putS a0
    pure r
{-# INLINE localByState #-}

instance {-# OVERLAPPABLE #-} (SModule OtherData `In` mods, Monad m) => MonadStateful OtherData (EffT mods es m) where
  get = getS
  {-# INLINE get #-}
  put = putS
  {-# INLINE put #-}
  modify = modifyS
  {-# INLINE modify #-}

type MeowAllData mods = (In (SModule BotConfig) mods, In (SModule WholeChat) mods, In (SModule OtherData) mods, In LoggingModule mods)

instance (Monad m, MeowAllData mods) => MonadReadOnly BotConfig (EffT mods es m) where
  query = getS
  {-# INLINE query #-}
instance (Monad m, MeowAllData mods) => MonadReadable BotConfig (EffT mods es m) where
  local = localByState
  {-# INLINE local #-}

instance (Monad m, MeowAllData mods) => MonadReadOnly BotModules (EffT mods es m) where
  query = queries botModules
  {-# INLINE query #-}

instance (Monad m, MeowAllData mods) => MonadReadOnly RunningMode (EffT mods es m) where
  query = queries runningMode
  {-# INLINE query #-}

instance (Monad m, MeowAllData mods) => MonadReadOnly WholeChat (EffT mods es m) where
  query = getS
  {-# INLINE query #-}
instance (Monad m, MeowAllData mods) => MonadReadable WholeChat (EffT mods es m) where
  local = localByState
  {-# INLINE local #-}

instance (Monad m, MeowAllData mods) => MonadReadOnly BotName (EffT mods es m) where
  query = queries nameOfBot
  {-# INLINE query #-}

instance (Monad m, MeowAllData mods) => MonadReadOnly BotId (EffT mods es m) where
  query = queries botId
  {-# INLINE query #-}

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

rseqWholeChat :: Strategy WholeChat
rseqWholeChat = evalList (evalTuple2 r0 (evalTuple2 rseq rseq))

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


makeLenses_ ''BotConfig
makeLenses_ ''OverrideSettings
