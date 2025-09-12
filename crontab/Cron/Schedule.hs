{-# LANGUAGE FlexibleInstances, DerivingVia, DeriveFunctor #-}
module Cron.Schedule where

import Data.Time
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Database.Persist.Sql (PersistField, PersistFieldSql)

hourlySchedule :: CronSchedule
hourlySchedule = CronSchedule
  { cronMinute     = FieldRanges (FixedValue (Minute 0) :| [])
  , cronHour       = FieldAny
  , cronDayOfMonth = FieldAny
  , cronMonth      = FieldAny
  , cronDayOfWeek  = FieldAny
  }

newtype CronText = CronText { unCronText :: Text }
  deriving (PersistField, PersistFieldSql, Show, Eq, Ord) via Text

newtype Minute        = Minute Int        deriving (Eq, Show, Num, Ord, Real, Integral, Enum) via Int
newtype Hour          = Hour Int          deriving (Eq, Show, Num, Ord, Real, Integral, Enum) via Int
newtype DayOfMonthNT  = DayOfMonthNT  Int deriving (Eq, Show, Num, Ord, Real, Integral, Enum) via Int
newtype MonthOfYearNT = MonthOfYearNT Int deriving (Eq, Show, Num, Ord, Real, Integral, Enum) via Int
newtype IntP          = IntP Int          deriving (Eq, Show, Num, Ord, Real, Integral, Enum) via Int

--------------------------------------------------------------------------------
-- | A CronSchedule holds the 5 classical crontab fields:
--   * minute
--   * hour
--   * day of month
--   * month
--   * day of week
--------------------------------------------------------------------------------
data CronSchedule = CronSchedule
  { cronMinute     :: FieldExpr Minute
  , cronHour       :: FieldExpr Hour
  , cronDayOfMonth :: FieldExpr DayOfMonthNT
  , cronMonth      :: FieldExpr MonthOfYearNT
  , cronDayOfWeek  :: FieldExpr DayOfWeek
  }
  deriving (Eq, Show)

data CronTab a = CronTab CronSchedule a
  deriving (Eq, Show, Functor)

--------------------------------------------------------------------------------
-- | FieldExpr describes what a single cron field can contain:
--   - A wildcard '*' means "any value".
--   - One or more ranges (which might just be single values).
--------------------------------------------------------------------------------
data FieldExpr e
  = FieldAny
  | FieldRanges (NonEmpty (RangeExpr e))
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | RangeExpr describes a single range or single value within a field:
--     - 'FixedValue 5' means "the literal value 5".
--     - 'Range 1 7' means "any value from 1 to 7 inclusive".
--     - 'SteppedRange 1 30 2' means "1 to 30 in steps of 2".
--------------------------------------------------------------------------------
data RangeExpr e
  = FixedValue e
  | Range e e
  | SteppedRange e e IntP
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | BoundedField: supply min and max for each cron field, so that '*/15' etc.
--   can be parsed into SteppedRange from min..max.
--------------------------------------------------------------------------------
class (Enum e, Ord e) => BoundedField e where
  fieldMin :: e
  fieldMax :: e

--------------------------------------------------------------------------------
-- Instances for each field: (these match normal cron constraints)
--------------------------------------------------------------------------------
instance BoundedField Minute where
  fieldMin = Minute 0
  fieldMax = Minute 59

instance BoundedField Hour where
  fieldMin = Hour 0
  fieldMax = Hour 23

instance BoundedField DayOfMonthNT where
  fieldMin = DayOfMonthNT 1
  fieldMax = DayOfMonthNT 31

instance BoundedField MonthOfYearNT where
  fieldMin = MonthOfYearNT 1
  fieldMax = MonthOfYearNT 12

-- DayOfWeek is trickier: typically 0..6, but let's define it:
-- data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
instance BoundedField DayOfWeek where
  fieldMin = Sunday
  fieldMax = Saturday

