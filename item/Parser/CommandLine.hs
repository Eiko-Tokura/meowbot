-- | Some utilities for parsing command line arguments.
module Parser.CommandLine
  ( ReadArg (..)
  , ReadFromArgs (..)
  , ReadArgsError (..)
  , ReadArgUsingRead (..)
  , runParserE
  , readOptionalField

  ) where

import Control.Monad.Item
import Control.Exception
import Data.Typeable
import Data.String (IsString, fromString)
import Data.Either (fromRight)
import Parser.Except
import Parser.Run
import Data.Text (Text, pack)
import Data.List.NonEmpty (NonEmpty(..))

newtype ReadArgsError = ReadArgsError String
  deriving newtype (IsString, Semigroup, Monoid)
  deriving stock (Show, Typeable)

instance Exception ReadArgsError

class ReadArg a where
  readArg :: ParserE [String] String ReadArgsError a

newtype ReadArgUsingRead a = ReadArgUsingRead { unReadArgUsingRead :: a }
  deriving newtype (Show, Read)

class ReadFromArgs a where
  readFromArgs :: String -> ParserE [String] String ReadArgsError a
  -- ^ e.g. readFromArgs "--database-port" :: ParserE [String] String ReadArgsError Int

instance ReadArg () where
  readArg = pure ()

instance ReadArg Int where
  readArg = readE "Expect an Int type argument" nonFlagString

instance ReadArg Integer where
  readArg = readE "Expect an Integer type argument" nonFlagString

instance ReadArg Double where
  readArg = readE "Expect a Double type argument" nonFlagString

instance ReadArg String where
  readArg = withE "Expect a String type argument" nonFlagString

instance ReadArg Text where
  readArg = withE "Expect a String type argument" (pack <$> nonFlagString)

instance Read a => ReadArg (ReadArgUsingRead a) where
  readArg = readE "Expect a Read type argument" nonFlagString

-- instance {-# OVERLAPPABLE #-} ReadArg a => ReadFromArgs [a] where
--   readFromArgs argName = do
--     liftR $ manyTill (just argName) getItem >> just argName
--     prependE ("Error when reading " <> fromString argName <> " : ") $ do
--       many readArg

readOptionalField :: Read a => String -> [String] -> Maybe a
readOptionalField str = fromRight Nothing . runParserE "" (fmap (fmap unReadArgUsingRead) (readFromArgs str) )

instance ReadArg a => ReadFromArgs (NonEmpty a) where
  readFromArgs argName = do
    liftR $ manyTill (just argName) getItem >> just argName
    prependE ("Error when reading " <> fromString argName <> " : ") $ do
      someNE readArg

-- | Maybe a means the argument is optional
instance {-# OVERLAPPING #-} ReadFromArgs a => ReadFromArgs (Maybe a) where
  readFromArgs argName = optional (readFromArgs argName)

instance ReadArg a => ReadFromArgs a where
  readFromArgs argName = do
    liftR $ manyTill (just argName) getItem >> just argName
    prependE ("Error when reading " <> fromString argName <> " : ") readArg

instance (ReadArg a, ReadArg b) => ReadFromArgs (a, b) where
  readFromArgs argName = do
    liftR $ manyTill (just argName) getItem >> just argName
    prependE ("Error when reading " <> fromString argName <> " : ") $ do
      a <- prependE " in the first argument : " readArg
      b <- prependE " in the second argument : " readArg
      return (a, b)

instance (ReadArg a, ReadArg b, ReadArg c) => ReadFromArgs (a, b, c) where
  readFromArgs argName = do
    liftR $ manyTill (just argName) getItem >> just argName
    prependE ("Error when reading " <> fromString argName <> " : ") $ do
      a <- prependE " in the first argument : " readArg
      b <- prependE " in the second argument : " readArg
      c <- prependE " in the third argument : " readArg
      return (a, b, c)
