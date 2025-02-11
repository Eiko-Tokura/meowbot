{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE TemplateHaskell, DerivingStrategies, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, AllowAmbiguousTypes, DeriveAnyClass #-}
module MeowBot.Parser
  ( module Parser.Run
  , cqmsg
  , positiveInt
  , Tree(..)
  , flattenTree
  , htmlDecode, htmlDecodeFunction
  , cqcodeExceptFace
  , cqother
  , cqcodeLenient, cqcodeFix
  , filterOutputTags
  , htmlCodes
  , item
  , commandSeparator, commandSeparator2
  , headCommand
  , canBeEmpty
  , parseByRead
  , onlyMessage
  , MetaMessage(..)
  , CQCode(..)
  , ChatSetting(..)
  , Text

  ) where

import Parser.Run
import Parser.Definition
import MeowBot.CQCode
import MeowBot.MetaMessage
import External.ChatAPI (ChatSetting(..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either(lefts, fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy)

data Tree a = EmptyTree | Node a [Tree a] deriving Show
  deriving (Generic, NFData)

instance Functor Tree where
  fmap _   EmptyTree = EmptyTree
  fmap fab (Node a subTrees) = Node (fab a) (fmap (fmap fab) subTrees)
  {-# INLINE fmap #-}

instance IsStream (Tree a) a where
  uncons EmptyTree = []
  uncons (Node a []) = [(a, EmptyTree)]
  uncons (Node a subTrees) = (a, ) <$> subTrees
  {-# INLINE uncons #-}

-- | Flatten a tree to a list, putting the root node first, then the children.
flattenTree :: Tree a -> [a]
flattenTree EmptyTree = []
flattenTree (Node a children) = a : concatMap flattenTree children
{-# INLINE flattenTree #-}

-- flattenTree :: Tree a -> [a]
-- flattenTree = concat . flatten

-- | Identicial to 'getItem'.
item :: (MonadItem i m) => m i
item = getItem
{-# INLINE item #-}

-- | Parse HTML codes to a Char, only `&amp;`, `&#44;`, `&#91;`, `&#93;`.
htmlCodes :: (Chars sb) => Parser sb Char Char
htmlCodes = just '&' >> asumE
  [ $(stringQ_ "amp;") >> pure '&'
  , just '#' >> asumE
    [ $(stringQ_ "44;") >> pure ','
    , just '9' >> asumE
      [ $(stringQ_ "1;") >> pure '['
      , $(stringQ_ "3;") >> pure ']'
      ]
    ]
  ]
{-# INLINE htmlCodes #-}

-- | Using 'htmlCodes' to continuously decode HTML codes.
htmlDecode :: (Chars sb) => Parser sb Char String
htmlDecode = many $ htmlCodes <|> getItem
{-# INLINE htmlDecode #-}

-- | Using 'htmlDecode' to decode HTML codes, the function version.
htmlDecodeFunction :: (Chars sb) => sb -> String
htmlDecodeFunction = fromMaybe "" . runParser htmlDecode
{-# INLINE htmlDecodeFunction #-}

cqother :: (Chars sb) => Text -> Parser sb Char CQCode
cqother str = CQOther str <$> intercalateBy ($(itemInQ ",;"))
  ((,) <$> some' (htmlCodes <|> itemNot '=')   <* just '='
       <*> many' (htmlCodes <|> $(itemNotInQ ",;]"))
  )
{-# INLINE cqother #-}

cqcodeExceptFace :: (Chars sb) => Parser sb Char CQCode
cqcodeExceptFace = do
  $(stringQ "[CQ:")
  cqtype :: Text <- some' $ itemNot ','
  just ','
  case cqtype of
    "at"    -> $(stringQ_ "qq=") >> CQAt <$> int <*> optMaybe ($(stringQ_ ",name=") >> T.pack <$> many item)
    "reply" -> $(stringQ_ "id=") >> CQReply <$> int
    "face"  -> zero
    str     -> cqother str
  <* just ']'
{-# INLINE cqcodeExceptFace #-}

-- | This function is used to fix inaccurate output of Meowmeow
cqcodeLenient :: (Chars sb) => Parser sb Char CQCode
cqcodeLenient = do
  $(stringQ "[CQ:")
  cqtype :: Text <- some' $ itemNot ','
  just ',' >> spaces0
  case cqtype of
    "at"    -> $(stringQ_ "qq=") >> CQAt <$> int <*> optMaybe ($(stringQ_ ",name=") >> T.pack <$> many item)
    "reply" -> $(stringQ_ "id=") >> CQReply <$> int
    str     -> cqother str
  <* just ']'
{-# INLINE cqcodeLenient #-}

-- | This function is used to fix inaccurate output of Meowmeow
cqcodeFixP :: (Chars sb) => Parser sb Char Text
cqcodeFixP = fmap (T.concat . map (either embedCQCode id) . collectRightCharsText) $ many $ cqcodeLenient |+| getItem @Char
{-# INLINE cqcodeFixP #-}

collectRightCharsText :: [Either a Char] -> [Either a Text]
collectRightCharsText 
  = concat
  . fmap (\case
      l@(Left _:_)  -> [ Left x | Left x <- l ]
      r@(Right _:_) -> [ Right $ packable $ htmlDecodeFunction [ c | Right c <- r ] ]
      []            -> []
    )
  . groupBy
      (\x y -> case (x, y) of
        (Left _, Left _) -> True
        (Right _, Right _) -> True
        _ -> False
      )
{-# INLINE collectRightCharsText #-}

-- | This function is used to fix inaccurate output of Meowmeow
cqcodeFix :: Text -> Text
cqcodeFix x = fromMaybe x $ runParser cqcodeFixP x
{-# INLINE cqcodeFix #-}

filterOutputTags :: [Text] -> Text -> Text
filterOutputTags tags x = fromMaybe x $ flip runParser x $ do
  many $ asum [ tag (T.unpack t) | t <- tags ]
  many' item
  where tag s = string "<" >> string s >> string ">" >> manyTill (string "</" >> string s >> string ">") item >> (string "</" >> string s >> string ">")
{-# INLINABLE filterOutputTags #-}

cqmsg :: (Chars sb) => Parser sb Char MetaMessage
cqmsg = do
  leither <- many $ cqcodeExceptFace |+| getItem
  return MetaMessage
    -- onlyMessage = packable $ fromMaybe "" $ runParser htmlDecode $ rights leither
    { cqcodes = lefts leither
    , mixedMessage = collectRightCharsText leither
    , replyTo = listToMaybe [id | CQReply id <- lefts leither]
    , metaMessageItems = []
    , additionalData = []
    }
{-# INLINE cqmsg #-}

positiveInt :: (Chars sb, Integral i, Read i) => Parser sb Char i
positiveInt = require (>0) nint
{-# INLINE positiveInt #-}

commandSeparator :: (Chars sb) => Parser sb Char String
commandSeparator = fmap concat . some $ asumE $ string <$> ["\r\n", "\r", "\n", " "]
{-# INLINE commandSeparator #-}

commandSeparator2 :: (Chars sb) => Parser sb Char (Either Char [String])
commandSeparator2 = $(itemInQ ['～', '~', ',', '，', '!', '！', '?', '？']) |+| some (asumE $ string <$> ["\r\n", "\r", "\n", " "])
{-# INLINE commandSeparator2 #-}

headCommand :: (Chars sb) => String -> Parser sb Char String
headCommand cmd = spaces0 >> just ':' <|> just '：' >> string cmd
{-# INLINE headCommand #-}

canBeEmpty :: forall sb b a. (IsStream sb b) => Parser sb b a -> Parser sb b (Maybe a)
canBeEmpty p = fromRight Nothing <$> (end @b |+| (Just <$> p))
{-# INLINE canBeEmpty #-}

parseByRead :: (Chars sb) => Read a => Parser sb Char a
parseByRead = do
  str <- many item
  case reads str of
    [(x, "")] -> return x
    _ -> zero
{-# INLINE parseByRead #-}
