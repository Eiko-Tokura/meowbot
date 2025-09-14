module Utils.Time
  ( module Data.Time
  , module Data.Time.Clock.POSIX
  , module Utils.Time
  ) where

import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)

newtype UnixSec = UnixSec { unUnixSec :: Int }
  deriving (Show, Eq, Read, Generic)
  deriving newtype (NFData, FromJSON, ToJSON)

unixSecToUTCTime :: UnixSec -> UTCTime
unixSecToUTCTime (UnixSec s) = posixSecondsToUTCTime $ fromIntegral s

