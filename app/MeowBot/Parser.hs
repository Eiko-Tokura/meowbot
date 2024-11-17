{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# LANGUAGE TemplateHaskell, DerivingStrategies, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, AllowAmbiguousTypes, DeriveAnyClass #-}
module MeowBot.Parser 
  ( module Parser.Run
  , cqmsg
  , positiveInt
  , Tree(..)
  , flattenTree
  , htmlDecode
  , cqcodeExceptFace
  , cqother
  , htmlCodes
  , end, item
  , commandSeparator, commandSeparator2
  , headCommand
  , canBeEmpty
  , parseByRead
  , MetaMessage(..)
  , CQCode(..)
  , ChatSetting(..)
  , Text

  , tshow

  ) where

import Parser.Run
import Parser.Definition
import MeowBot.CQCode
import MeowBot.MetaMessage
import External.ChatAPI (ChatSetting(..))
import Control.Monad (when)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either(lefts, rights, fromRight)
import Data.Text (Text)
import qualified Data.Text as T 

tshow :: Show a => a -> Text
tshow = T.pack . show

data Tree a = EmptyTree | Node a [Tree a] deriving Show
  deriving (Generic, NFData)

instance Functor Tree where
  fmap _   EmptyTree = EmptyTree
  fmap fab (Node a subTrees) = Node (fab a) (fmap (fmap fab) subTrees)
  {-# INLINE fmap #-}

instance Stream (Tree a) a where
  uncons EmptyTree = []
  uncons (Node a []) = [(a, EmptyTree)]
  uncons (Node a subTrees) = (a, ) <$> subTrees
  {-# INLINE uncons #-}

flattenTree :: Tree a -> [a]
flattenTree EmptyTree = []
flattenTree (Node a children) = a : concatMap flattenTree children

-- flattenTree :: Tree a -> [a]
-- flattenTree = concat . flatten

item :: (MonadItem i m) => m i
item = getItem

htmlCode :: String -> Char -> Parser Char Char
htmlCode code char = string code >> return char

htmlCodes :: Parser Char Char
htmlCodes = foldr1 (<|>)
  [ htmlCode "&amp;" '&'
  , htmlCode "&#91;" '['
  , htmlCode "&#93;" ']'
  , htmlCode "&#44;" ','
  ]

htmlDecode :: Parser Char String
htmlDecode = many $ htmlCodes <|> getItem

cqother :: Text -> Parser Char CQCode
cqother str = CQOther str <$> intercalateBy (itemIn ",;") 
  ((,) <$> some' (htmlCodes <|> itemNotIn "=")   <* just '='
       <*> many' (htmlCodes <|> itemNotIn ",;]")
  )

cqcodeExceptFace :: Parser Char CQCode
cqcodeExceptFace = do
  $(stringQ "[CQ:")
  cqtype :: Text <- some' $ itemNotIn ","
  just ','
  case cqtype of
    "at"    -> $(stringQ "qq=") >> CQAt <$> int
    "reply" -> $(stringQ "id=") >> CQReply <$> int
    "face"  -> zero
    str     -> cqother str
  <* just ']'

cqmsg :: Parser Char MetaMessage
cqmsg = do
  leither <- many $ cqcodeExceptFace |+| getItem
  return MetaMessage
    { onlyMessage = pack $ fromMaybe "" $ runParser htmlDecode $ rights leither
    , cqcodes = lefts leither
    , replyTo = listToMaybe [id | CQReply id <- lefts leither] 
    , withChatSetting = Nothing
    , additionalData = []
    }

positiveInt :: (Integral i, Read i) => Parser Char i
positiveInt = require (>0) nint

end :: forall i m. (MonadZero m, MonadTry m, MonadItem i m) => m ()
end = do
  hasItem <- tryBool (getItem @i)
  when hasItem zero

commandSeparator :: Parser Char String
commandSeparator = some $ itemIn " \t\n"

commandSeparator2 :: Parser Char (Either Char [String])
commandSeparator2 = itemIn ['～', '~', ',', '，', '!', '！', '?', '？'] |+| many (foldr1 (<|>) $ string <$> ["\r\n", "\r", "\n", " "])

headCommand :: String -> Parser Char String
headCommand cmd = spaces0 >> just ':' <|> just '：' >> string cmd
 
canBeEmpty :: forall b a. Parser b a -> Parser b (Maybe a)
canBeEmpty p = fromRight Nothing <$> (end @b |+| (Just <$> p))

parseByRead :: Read a => Parser Char a
parseByRead = do
  str <- many item
  case reads str of
    [(x, "")] -> return x
    _ -> zero
