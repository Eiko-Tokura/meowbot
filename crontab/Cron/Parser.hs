{-# LANGUAGE OverloadedStrings, ViewPatterns, FlexibleInstances, DerivingVia #-}
module Cron.Parser
  ( -- * Data Types
    CronSchedule(..)
  , FieldExpr(..)
  , RangeExpr(..)
  , CronText -- no constructor

    -- * Field types
  , Minute(..)
  , Hour(..)
  , DayOfMonthNT(..)
  , MonthOfYearNT(..)
  , DayOfWeek(..)
  , IntP(..)

    -- * Parser
  , parseCron
  , cronScheduleParser
  , cronTextToCronSchedule

    -- * Validation
  , isValidCron
  , validateCronText
  ) where

import Cron.Schedule
import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text (Parser)
import Data.Text (Text)
import Data.Time
import Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------
-- Top-level cron parser
--------------------------------------------------------------------------------

-- | Parse a 5-field cron expression (minute hour dayOfMonth month dayOfWeek).
--   Return either an error message or a 'CronSchedule'.
--
--   Example usage:
--
-- > case parseCron "*/15 0 1,15 * 1-5" of
-- >   Left err   -> putStrLn $ "Error: " ++ show err
-- >   Right cron -> print cron
--
parseCron :: Text -> Either ParseError CronSchedule
parseCron = parse (cronScheduleParser <* skipSpaces <* eof) "cron"

isValidCron :: Text -> Bool
isValidCron (parseCron -> Left _) = False
isValidCron _                     = True

validateCronText :: Text -> Either ParseError CronText
validateCronText (parseCron -> Left err) = Left err
validateCronText cronText                = Right (CronText cronText)

cronTextToCronSchedule :: CronText -> CronSchedule
cronTextToCronSchedule (CronText cronText) =
  case parseCron cronText of
    Left err  -> error $ "Invalid cron text: " ++ show err
    Right cron -> cron

--------------------------------------------------------------------------------
-- Parsers for each of the 5 fields
--------------------------------------------------------------------------------

-- | The main cronSchedule parser that consumes exactly five fields, does not require EOF so it can be combined with other parsers.
cronScheduleParser :: Parser CronSchedule
cronScheduleParser = do
  skipSpaces
  fMin   <- fieldParser
  skipSpaces1
  fHour  <- fieldParser
  skipSpaces1
  fDom   <- fieldParser
  skipSpaces1
  fMon   <- fieldParser
  skipSpaces1
  fDow   <- fieldParser
  return $ CronSchedule fMin fHour fDom fMon fDow

-- | Skip zero or more spaces.
skipSpaces :: Parser ()
skipSpaces = skipMany (oneOf " \t")

-- | Skip one or more spaces.
skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 (oneOf " \t")

--------------------------------------------------------------------------------
-- Validation of field values
--------------------------------------------------------------------------------

class ValidatableField e where
  validateField :: Parser e

instance ValidatableField Minute where
  validateField = do
    val <- intParser
    if val >= 0 && val < 60
      then return (Minute val)
      else fail $ "Invalid minute value: " ++ show val

instance ValidatableField Hour where
  validateField = do
    val <- intParser
    if val >= 0 && val < 24
      then return (Hour val)
      else fail $ "Invalid hour value: " ++ show val

instance ValidatableField DayOfMonthNT where
  validateField = do
    val <- intParser
    if val >= 1 && val <= 31
      then return (DayOfMonthNT val)
      else fail $ "Invalid day of month value: " ++ show val

instance ValidatableField MonthOfYearNT where
  validateField = do
    val <- intParser
    if val >= 1 && val <= 12
      then return (MonthOfYearNT val)
      else fail $ "Invalid month value: " ++ show val

instance ValidatableField DayOfWeek where
  validateField = do
    val <- intParser
    if val >= 0 && val <= 7
      -- accept 0 to 7, both 0 and 7 means Sunday!
      -- but never use 0-7, this is going to mean just Sunday
      then return (toEnum val)
      else fail $ "Invalid day of week value: " ++ show val

instance ValidatableField IntP where
  validateField = do
    val <- intParser
    if val > 0
      then return (IntP val)
      else fail $ "Invalid integer value (need to be positive): " ++ show val

--------------------------------------------------------------------------------
-- Field parser: either a '*' (FieldAny) or a comma-separated list of ranges.
--------------------------------------------------------------------------------
fieldParser :: (ValidatableField e, BoundedField e) => Parser (FieldExpr e)
fieldParser =
  (FieldRanges <$> commaSep rangeParser)
  <|> (char '*' >> return FieldAny)

-- | A comma-separated parser combinator. This tries to parse one or more p
commaSep :: Parser a -> Parser (NonEmpty a)
commaSep p = (:|) <$> p <*> many (char ',' >> p)

--------------------------------------------------------------------------------
-- Handling expressions like:
--
--   "42"         => FixedValue 42
--   "1-10"       => Range 1 10
--   "1-10/2"     => SteppedRange 1 10 2
--   "*/15"       => SteppedRange fieldMin fieldMax 15
--   "2/15"       => SteppedRange 2 fieldMax 15
--
-- We'll unify them in a single approach:
--------------------------------------------------------------------------------
rangeParser :: (ValidatableField e, BoundedField e) => Parser (RangeExpr e)
rangeParser =
  try starSlashParser
    <|> try rangeDashParser
    <|> singleValueParser

--------------------------------------------------------------------------------
-- Case 1: handle '*/step' or '2/step' => parse either '*' or an integer,
--         then a slash, then step.
--
--   '*/15' => (SteppedRange fieldMin fieldMax 15)
--   '2/15' => (SteppedRange 2 fieldMax 15)
--------------------------------------------------------------------------------
starSlashParser :: (ValidatableField e, BoundedField e) => Parser (RangeExpr e)
starSlashParser = do
  startOrStar <- (char '*' >> return Nothing)
             <|> (Just <$> validateField)
  _ <- char '/'
  stepVal <- validateField  -- e.g., IntP
  case startOrStar of
    Nothing     -> return $ SteppedRange fieldMin fieldMax stepVal
    Just lowVal -> return $ SteppedRange lowVal   fieldMax stepVal

--------------------------------------------------------------------------------
-- Case 2: handle normal "A-B" or "A-B/step" or just "A-B"
--   e.g. "1-10/2" => SteppedRange 1 10 2
--                   "1-10" => Range 1 10
--------------------------------------------------------------------------------
rangeDashParser :: (ValidatableField e, BoundedField e) => Parser (RangeExpr e)
rangeDashParser = do
  startVal <- validateField
  _ <- char '-'
  endVal <- validateField
  -- optional slash
  mSlash <- optionMaybe (char '/')
  case mSlash of
    Nothing -> return $ Range startVal endVal
    Just _  -> do
      step <- validateField
      return $ SteppedRange startVal endVal step

--------------------------------------------------------------------------------
-- Case 3: single integer => e.g. "42"
--------------------------------------------------------------------------------
singleValueParser :: (ValidatableField e) => Parser (RangeExpr e)
singleValueParser = FixedValue <$> validateField
--------------------------------------------------------------------------------
-- | A very simple integer parser that consumes one or more digits and
--   converts them to Int.
--------------------------------------------------------------------------------
intParser :: Parser Int
intParser = do
  ds <- many1 digit
  return (read ds)
