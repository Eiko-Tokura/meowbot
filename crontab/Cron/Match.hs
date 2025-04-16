{-# LANGUAGE OverloadedStrings #-}
module Cron.Match 
  ( -- * Main functionalities
    timeMatchesCron
  , timeUntilNext

    -- * Re-export cron data types for convenience
  , CronSchedule(..)
  , FieldExpr(..)
  , RangeExpr(..)
  , parseCron
  ) where

import Cron.Parser -- (the module we defined previously)
import Data.Time
-- import Text.Parsec (ParseError)

-------------------------------------------------------------------------------
-- | 1. Function to check if a given time matches a given cron schedule.
--
--   This function extracts (minute, hour, day-of-month, month, day-of-week)
--   from a 'UTCTime' and compares them against the fields in 'CronSchedule'.
--
--   Note that a special convention in cron is that if both day-of-month and
--   day-of-week are specified, the job should run if any of them matches.
-------------------------------------------------------------------------------
timeMatchesCron :: UTCTime -> CronSchedule -> Bool
timeMatchesCron utcTime cron =
  let
    -- Break down the UTCTime into calendar components in UTC.
    -- If you need a local timezone, you'd convert UTCTime -> LocalTime with e.g. 'utcToLocalTime'.
    timeInUTC        = utcToZonedTime utc utcTime
    localTime        = zonedTimeToLocalTime timeInUTC
    (year, mon, dom) = toGregorian (localDay localTime)
    tod              = localTimeOfDay localTime
    hour             = todHour tod
    minute           = todMin tod
    dayOfWeekNum    = dayOfWeekGregorian year mon dom  -- 0=Sunday, 1=Monday, ... 6=Saturday
  in
    -- Now check each cron field
    let minuteOk = matchesField (fromIntegral minute) (cronMinute cron)
        hourOk   = matchesField (fromIntegral hour)   (cronHour cron)
        monOk    = matchesField (fromIntegral mon)    (cronMonth cron)
        doMOk    = matchesField (fromIntegral dom)    (cronDayOfMonth cron)
        doWOk    = matchesField (dayOfWeekNum)        (cronDayOfWeek cron)

    in minuteOk && hourOk && monOk && case (cronDayOfMonth cron, cronDayOfWeek cron) of
          (FieldAny, FieldAny) -> True
          (FieldAny, _)        -> doWOk
          (_, FieldAny)        -> doMOk
          (_, _)               -> doMOk || doWOk

-------------------------------------------------------------------------------
-- | 2. Function to compute how many seconds from the given time
--   until the next matching cron time.
--
--   The naive approach here is to increment minute-by-minute until we find
--   a time that matches. This can be expensive if the next trigger is far
--   away. In practice, you'd want a more direct calculation or a smaller
--   stepping approach. But for demonstration, it's straightforward.
-------------------------------------------------------------------------------
timeUntilNext :: UTCTime      -- ^ Current time
              -> CronSchedule -- ^ Cron schedule
              -> NominalDiffTime
timeUntilNext start cron =
  let nextT = findNextMatchingTime start cron
  in diffUTCTime nextT start

-- | Find the next UTCTime at or after 'start' that matches the given cron.
--   We do this by checking if 'start' itself matches. If not, we add 1 minute
--   and recurse.  This is a naive BFS approach.
findNextMatchingTime :: UTCTime -> CronSchedule -> UTCTime
findNextMatchingTime t cron
  | timeMatchesCron t cron = t
  | otherwise              = findNextMatchingTime (addUTCTime 60 t) cron

-------------------------------------------------------------------------------
-- | Helper to decide if an integer value matches a 'FieldExpr' (e.g., for minute).
-------------------------------------------------------------------------------
matchesField :: (Ord e, Enum e) => e -> FieldExpr e -> Bool
matchesField _   FieldAny             = True
matchesField val (FieldRanges ranges) = any (matchesRange val) ranges

-------------------------------------------------------------------------------
-- | Does the given integer value fall within a single 'RangeExpr'?
-------------------------------------------------------------------------------
matchesRange :: (Ord e, Enum e) => e -> RangeExpr e -> Bool
matchesRange v (FixedValue x)      = (v == x)
matchesRange v (Range low high)    = (v >= low && v <= high)
matchesRange v (SteppedRange low high step) =
  v >= low && v <= high && ((fromEnum v - fromEnum low) `mod` fromEnum step == 0)

-------------------------------------------------------------------------------
-- | Compute the day-of-week as an Int in [0..6], with 0=Sunday, 1=Monday, etc.
-------------------------------------------------------------------------------
dayOfWeekGregorian :: Integer -> Int -> Int -> DayOfWeek
dayOfWeekGregorian year month day =
  let date  = fromGregorian year month day
      -- Data.Time has Sunday=0, Monday=1, Tuesday=2, ... Sunday=7
      wd    = dayOfWeek date
  in wd
