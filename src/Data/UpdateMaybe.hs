-- | This type represents the stages of an update process.
module Data.UpdateMaybe where

import Control.DeepSeq (NFData)
import Data.Time.Clock
import GHC.Generics (Generic)

data WithTime a = WithTime
  { timeStamp  :: !UTCTime
  , unWithTime :: !a
  }
  deriving (Show, Eq, Read, Generic, NFData)

data UMaybe a
  = NothingYet          -- ^ not sufficient data to compute yet
  | Updating            -- ^ not sufficient data to compute yet but an update is in progress
  | UpdatingOldValue !a -- ^ has an old value and an update is in progress
  | Updated !a          -- ^ fully updated value
  deriving (Show, Eq, Read, Generic, NFData)

type UMaybeTime a = UMaybe (WithTime a)

instance Functor UMaybe where
  fmap _ NothingYet  = NothingYet
  fmap _ Updating    = Updating
  fmap f (UpdatingOldValue a) = UpdatingOldValue (f a)
  fmap f (Updated a) = Updated (f a)
  {-# INLINE fmap #-}

instance Applicative UMaybe where
  pure = Updated

  NothingYet <*> _ = NothingYet
  _ <*> NothingYet = NothingYet

  Updating <*> _ = Updating
  _ <*> Updating = Updating

  UpdatingOldValue f <*> Updated a = UpdatingOldValue (f a)
  Updated f <*> UpdatingOldValue a = UpdatingOldValue (f a)
  UpdatingOldValue f <*> UpdatingOldValue a = UpdatingOldValue (f a)

  (Updated f) <*> (Updated a) = Updated (f a)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad UMaybe where
  return = pure
  NothingYet  >>= _          = NothingYet
  Updating    >>= _          = Updating
  (UpdatingOldValue a) >>= f = case f a of
      Updated v -> UpdatingOldValue v
      rest      -> rest
  (Updated a) >>= f          = f a
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

toUpdating :: UMaybe a -> UMaybe a
toUpdating (NothingYet; Updating) = Updating
toUpdating (UpdatingOldValue a)   = UpdatingOldValue a
toUpdating (Updated a)            = UpdatingOldValue a
{-# INLINE toUpdating #-}

toMaybe :: UMaybe a -> Maybe a
toMaybe (NothingYet; Updating) = Nothing
toMaybe (UpdatingOldValue a)   = Just a
toMaybe (Updated a)            = Just a
{-# INLINE toMaybe #-}

wToMaybe :: UMaybeTime a -> Maybe a
wToMaybe = toMaybe . fmap unWithTime
{-# INLINE wToMaybe #-}

updateUMaybeTime :: UTCTime -> a -> (a -> a) -> UMaybeTime a -> UMaybeTime a
updateUMaybeTime newTime defVal  _      (NothingYet; Updating)              = Updated $ newTime `WithTime` defVal
updateUMaybeTime newTime _       newVal (UpdatingOldValue (WithTime _ old)) = Updated $ WithTime newTime (newVal old)
updateUMaybeTime newTime _       newVal (Updated (WithTime _ old))          = Updated $ WithTime newTime (newVal old)
{-# INLINE updateUMaybeTime #-}

updateUMaybeTimeConst :: UTCTime -> a -> UMaybeTime a -> UMaybeTime a
updateUMaybeTimeConst t d = updateUMaybeTime t d (const d)
{-# INLINE updateUMaybeTimeConst #-}

updateUMaybeTimeWithoutTime :: UMaybeTime a -> (a -> a) -> UMaybeTime a
updateUMaybeTimeWithoutTime NothingYet                                _      = NothingYet
updateUMaybeTimeWithoutTime Updating                                  _      = Updating
updateUMaybeTimeWithoutTime (UpdatingOldValue (WithTime oldTime old)) newVal = Updated $ WithTime oldTime (newVal old)
updateUMaybeTimeWithoutTime (Updated (WithTime oldTime old))          newVal = Updated $ WithTime oldTime (newVal old)
{-# INLINE updateUMaybeTimeWithoutTime #-}

needUpdate :: NominalDiffTime -> UTCTime -> UMaybeTime a -> Bool
needUpdate _ _ NothingYet                                               = True
needUpdate diff now (Updated (WithTime t _)) | diffUTCTime now t > diff = True
needUpdate _ _ _                                                        = False
{-# INLINE needUpdate #-}

-- class UpdateAction m a where
--   firstUpdate :: m a
--   laterUpdate :: m (a -> a)

-- data UpdateAction m a = UpdateAction
--   { firstUpdate :: m a
--   , laterUpdate :: m (a -> a)
--   }
