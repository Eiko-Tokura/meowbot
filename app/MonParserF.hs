-- author : Eiko chan
{-# LANGUAGE RankNTypes #-}
module MonParserF 
  ( ParserF(..), Parsable(..)
  , MetaMessage(..)
  , Tree(..), flatten
  , end, many, many0, tryMaybe, tryMaybe0, try0, tryBool
  , zero, item, satisfy, just, itemIn, itemNotIn, itemsIn, itemsNotIn, items0In, items0NotIn
  , string, word, nonFlagWord, insideBrackets, till, inBraket, collectItemsUntil, collectItemsInBracket, collectInBigBracket, collectItemsUntilLevel0
  , htmlDecode, lower, upper, letter, space, spaceOrEnter, commandSeparator, commandSeparator2, spaces0, spaces, identifier
  , digit, digits, int, nonNegative, positive, nonNegativeInt, positiveInt, float, positiveFloat  -- numbers
  , cqcode, cqcodeExceptFace, cqmsg, intercalateP, intercalateP0, eitherParse, htmlCodes
  , headCommand
  , parseByRead
  , canBeEmpty

  , (<+>), (<:>)

  , mRunParserF
  ) where

import MeowBot.CQCode
import External.ChatAPI (Message(..), ChatSetting(..))
import Control.Monad (when)
import Control.Applicative (liftA2, Alternative(empty, (<|>)))
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Additional 
import Data.Either(lefts, rights, fromRight)

class (Functor f) => Parsable f where -- parsable functors would include [] and Tree
  node :: f a -> Maybe a
  childs :: f a -> [f a]

instance Parsable [] where
  node [] = Nothing
  node (x:_) = Just x
  childs [] = []
  childs (_:xs) = [xs]
  {-# INLINE node #-}
  {-# INLINE childs #-}

newtype ParserF b a = CreateParserF { runParserF :: forall f. Parsable f => f b -> [(a, f b)] }

data Tree a = EmptyTree | Node a [Tree a] deriving Show

instance Functor Tree where
  fmap _   EmptyTree = EmptyTree
  fmap fab (Node a subTrees) = Node (fab a) (fmap (fmap fab) subTrees)
  {-# INLINE fmap #-}

instance Parsable Tree where
  node EmptyTree = Nothing
  node (Node x _) = Just x
  childs EmptyTree = []
  childs (Node _ []) = [EmptyTree]
  childs (Node _ y) = y
  {-# INLINE node #-}
  {-# INLINE childs #-}

flatten :: Tree a -> [a]
flatten EmptyTree = []
flatten (Node a children) = a : concatMap flatten children
--newtype Parser a = CreateParser { run :: String -> [(a, String)] }

--mRunParserF :: Parsable f => (ParserF b a) -> f b -> Maybe a
mRunParserF parser = fmap fst . listToMaybe . runParserF parser

instance Functor (ParserF b) where
  fmap f p = CreateParserF (\str -> [(f v, res) | (v, res) <- runParserF p str])
  {-# INLINE fmap #-}

-- The functor Parser is a monad, with unit (natural transform):
unit :: a -> ParserF b a 
unit v = CreateParserF $ \x -> [(v, x)]
{-# INLINE unit #-}
-- the unit (eta), returns a Parser with a given value without doing anything

-- and composition mu: Parser (Parser a) \to Parser a (another natural transform).
-- then the binding operator >>= is defined to be mu . Parser, i.e. 
-- p1 >>= f = mu (fmap_Parser f) p1
--          = mu (Parser a -> Parser Parser b) p1 
--          = Parser b
-- In haskell, in order to make a monad, first of all you need to make it a functor.
-- then you must make it applicative. This is just a requirement in haskell, it does not mean that any monad induces applicative structure.
instance Applicative (ParserF b) where
  pure = unit
  pf <*> p = CreateParserF (\str -> [(f v, res2) | (f, res) <- runParserF pf str, (v, res2) <- runParserF p res] )
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (ParserF b) where
  return = pure
  p1 >>= f = CreateParserF $ \str -> concat [runParserF (f v) res |(v, res)<- runParserF p1 str]
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

cup :: ParserF a b -> ParserF a b -> ParserF a b
(CreateParserF p1) `cup` (CreateParserF p2) = CreateParserF $ \str -> p1 str ++ p2 str
{-# INLINE cup #-}

instance Semigroup (ParserF b a) where
  (<>) = cup
  {-# INLINE (<>) #-}

instance Alternative (ParserF b) where
  empty = zero
  (<|>) = cup
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

instance Monoid (ParserF b a) where
  mempty = zero
  {-# INLINE mempty #-}

-- | Always fail to parse anything
zero :: ParserF a b
zero = CreateParserF $ const []
{-# INLINE zero #-}

-- | Always success (as long as input is nonempty), parse the first char.
item :: ParserF b b
item = CreateParserF fitem
  where
    fitem inp = case node inp of
      Nothing -> []
      Just xb -> [(xb, child) | child <- childs inp]
{-# INLINE item #-}

satisfy :: (b -> Bool) -> ParserF b b
satisfy f = do
  xb <- item
  if f xb then return xb else zero
{-# INLINE satisfy #-}

just c = satisfy ( == c)
{-# INLINE just #-}
isNot c = satisfy (/= c)
{-# INLINE isNot #-}

itemIn list      = satisfy (`elem` list)
itemNotIn list   = satisfy (not . (`elem` list))
itemsIn list     = many $ satisfy (`elem` list)
itemsNotIn list  = many $ satisfy (not . (`elem` list))
items0In list    = many0 $ satisfy (`elem` list)
items0NotIn list = many0 $ satisfy (not . (`elem` list))

-- | Parse inside a bracket, ignoring the brackets
insideBrackets :: (Char, Char) -> ParserF Char String
insideBrackets (l, r) = do
  _ <- just l
  many (isNot r) <* just r

-- | Parse a non-empty word, which is a continuous string of characters that are not spaces, or a string inside a pair of quotes.
word :: ParserF Char String
word = foldr1 (<>)
      [ insideBrackets ('\'', '\'')
      , insideBrackets ('"', '"') 
      , itemsNotIn [' ']
      ]

-- | Parse a word that is not a flag, i.e. a word that does not start with a dash.
nonFlagWord :: ParserF Char String
nonFlagWord = foldr1 (<>)
      [ insideBrackets ('\'', '\'')
      , insideBrackets ('"', '"') 
      , itemsNotIn [' ', '-']
      ]

-- | Parse a string until a specific character
till :: (Eq b) => b -> ParserF b [b]
till char = many $ isNot char

-- | the primitive parser for parsing a string inside a bracket, does not count the bracket levels.
inBraket :: (String, String) -> ParserF Char String
inBraket (bra, ket) = string bra *> till (head ket) <* string ket

-- | This function does not eat the end parser
collectItemsUntil :: ParserF b e -> ParserF b b -> ParserF b ([b], e) 
collectItemsUntil endParser itemP = do
  endMaybe <- tryMaybe0 endParser
  case endMaybe of
    Just end -> return ([], end)
    Nothing  -> do  getItem <- itemP
                    (rest, end') <- collectItemsUntil endParser itemP
                    return (getItem : rest, end')

-- | A customizable version of collectItemsUntil, where you can specify the bracket parsers and the item parser, properly counts the bracket levels.
collectItemsInBracket :: Int -> (ParserF b [b], ParserF b [b]) -> ParserF b [b] -> ParserF b [b]
collectItemsInBracket level (bra, ket) itemP = do
  step <- itemP `eitherParse` (bra `eitherParse` ket)   
  case step of
    Left i -> do
      sameLevel <- collectItemsInBracket level (bra, ket) itemP
      return $ i ++ sameLevel
    Right (Left b) -> do
      higherLevel <- collectItemsInBracket (level+1) (bra, ket) itemP
      return $ b ++ higherLevel
    Right (Right k) -> do
      if level == 0 
      then return []
      else do
        lowerLevel <- collectItemsInBracket (level-1) (bra, ket) itemP
        return $ k ++ lowerLevel 

-- | Collects items in a big bracket, i.e. { ... } (not including the brackets)
-- properly handles nested brackets and escapes
collectInBigBracket :: ParserF Char [Char]
collectInBigBracket = collectItemsInBracket 0 (string "{", string "}") (string "\\{" <> string "\\}" <> sequence [just '\\', itemNotIn "{}"] <> (pure <$> itemNotIn "\\{}"))

-- | This function does not eat the end parser
collectItemsUntilLevel0 :: (Eq b) => Int -> b -> b -> ParserF b a -> ParserF b b -> ParserF b ([b], a) 
collectItemsUntilLevel0 level l r endParser itemP = do
  endMaybe <- if level == 0 then tryMaybe0 endParser else return Nothing
  case endMaybe of
    Just end -> return ([], end)
    Nothing  -> do  getItem <- itemP
                    (rest, end') <- if getItem == l 
                                    then collectItemsUntilLevel0 (level+1) l r endParser itemP
                                    else if getItem == r
                                    then collectItemsUntilLevel0 (level-1) l r endParser itemP
                                    else collectItemsUntilLevel0 level l r endParser itemP
                    return (getItem : rest, end')


htmlDecode :: ParserF Char String
htmlDecode = many $ htmlCodes <> item

-- Todo : ExceptT s (ParserF b) a ~ ParserF b (Either s a)
-- i.e. create a wrapper for parsers String -> ParserF b a -> ParserF b (Either String a)
-- where ParserF b (Either String a) is a monad given by monad transformer

digit = itemIn ['0'..'9']
digits = many digit

parseByRead :: Read a => ParserF Char a
parseByRead = do
  str <- many0 item
  case reads str of
    [(x, "")] -> return x
    _ -> zero

-- | Polymorphic parser for integers
int :: (Integral a, Read a) => ParserF Char a
int =  (just '-' >> negate . read <$> digits)
    <> (read <$> digits)

require cond p = do
  x <- p
  if cond x then return x else zero

nonNegative :: (Num n, Ord n) => ParserF a n -> ParserF a n
nonNegative = require (>= 0)

positive :: (Num n, Ord n) => ParserF a n -> ParserF a n
positive = require (> 0)

nonNegativeInt :: ParserF Char Int
nonNegativeInt = read <$> digits

positiveInt :: ParserF Char Int
positiveInt = positive nonNegativeInt

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

float :: (Floating a, Read a) => ParserF Char a
float = do
  part1 <- (just '-' <:> digits) <> digits 
  mpart2 <- tryMaybe (just '.' <:> digits)
  case mpart2 of
    Nothing    -> return $ read part1
    Just part2 -> return $ read $ part1 ++ part2

positiveFloat :: ParserF Char Double
positiveFloat = positive float

lower = itemIn ['a'..'z'] 

upper = itemIn ['A'..'Z']

letter = lower <> upper

space = just ' '

string :: String -> ParserF Char String
string "" = unit ""
string (x:xs) = just x <:> string xs

canBeEmpty :: ParserF b a -> ParserF b (Maybe a)
canBeEmpty p = fromRight Nothing <$> (end <+> (Just <$> p))

tryMaybe :: ParserF b a -> ParserF b (Maybe a)
tryMaybe (CreateParserF p) = CreateParserF $ \str -> let r = p str in
  case r of
    [] -> [(Nothing, str)]
    _ -> [ (Just x, xs) | (x, xs) <- r]

-- | try to parse something, but even if successful, does not eat it
tryMaybe0 :: ParserF b a -> ParserF b (Maybe a) 
tryMaybe0 (CreateParserF p) = CreateParserF $ \str -> let r = p str in
  case r of
    [] -> [(Nothing, str)]
    _ -> [ (Just x, str) | (x, _) <- r]

try0 :: ParserF b [a] -> ParserF b [a]
try0 (CreateParserF p) = CreateParserF $ \str -> let r = p str in
  case r of
    [] -> [([], str)]
    _ -> r 

tryBool :: ParserF b a -> ParserF b Bool
tryBool parser = do
  m <- tryMaybe parser
  case m of
    Nothing -> return False
    Just _ -> return True

end :: ParserF b ()
end = do
  hasItem <- tryBool item
  when hasItem zero

many :: ParserF b a -> ParserF b [a]
many p = do
  r <- p
  rs <- try0 $ many p
  return (r:rs)

many0 p = try0 $ many p

spaceOrEnter = mconcat $ fmap string ["\r\n", "\r", "\n", " "]

commandSeparator = many spaceOrEnter

commandSeparator2 = itemIn ['～', '~', ',', '，', '!', '！', '?', '？'] 
  <+> many (mconcat $ string <$> ["\r\n", "\r", "\n", " "])

spaces0 = many0 space
spaces = many space

identifier = many (letter <> digit <> just '_')

headCommand cmd = spaces0 >> just ':' <> just '：' >> string cmd

cqcode :: ParserF Char CQCode
cqcode = do
  string "[CQ:"
  cqtype <- itemsNotIn ","
  just ','
  case cqtype of
    "at"    -> string "qq=" >> CQAt <$> int
    "reply" -> string "id=" >> CQReply <$> int
    str     -> cqother str 
  <* just ']'

cqother str = CQOther str <$> intercalateP (itemIn ",;") 
  ((,) <$> many  (htmlCodes <> itemNotIn "=")   <* just '='
       <*> many0 (htmlCodes <> itemNotIn ",;]")
  )

intercalateP :: ParserF b s -> ParserF b a -> ParserF b [a]
intercalateP sep p = p <:> many0 (sep *> p)

intercalateP0 :: ParserF b s -> ParserF b a -> ParserF b [a]
intercalateP0 sep p = try0 $ intercalateP sep p

cqcodeExceptFace :: ParserF Char CQCode
cqcodeExceptFace = do
  string "[CQ:"
  cqtype <- itemsNotIn ","
  just ','
  case cqtype of
    "at"    -> string "qq=" >> CQAt <$> int
    "reply" -> string "id=" >> CQReply <$> int
    "face"  -> zero
    str     -> cqother str 
  <* just ']'

data MetaMessage = MetaMessage 
  { onlyMessage :: String
  , cqcodes :: [CQCode]
  , replyTo :: Maybe Int
  , withChatSetting :: Maybe ChatSetting
  , additionalData :: [AdditionalData]
  } deriving (Show, Eq)

instance HasAdditionalData MetaMessage where
  getAdditionalData = additionalData
  modifyAdditionalData f meta = meta { additionalData = f $ additionalData meta }

eitherParse :: ParserF t a -> ParserF t b -> ParserF t (Either a b)
eitherParse f g = do
  rf <- tryMaybe f
  case rf of
    Nothing -> Right <$> g
    Just x -> return $ Left x

infixr 6 `eitherParse`

(<+>) = eitherParse
infixr 6 <+>

htmlCode :: String -> Char -> ParserF Char Char
htmlCode code char = string code >> return char

htmlCodes = mconcat
  [ htmlCode "&amp;" '&'
  , htmlCode "&#91;" '['
  , htmlCode "&#93;" ']'
  , htmlCode "&#44;" ','
  ]

cqmsg :: ParserF Char MetaMessage
cqmsg = do
  leither <- many $ eitherParse cqcodeExceptFace item
  return MetaMessage
    { onlyMessage = fromMaybe "" $ mRunParserF htmlDecode $ rights leither
    , cqcodes = lefts leither
    , replyTo = listToMaybe [id | CQReply id <- lefts leither] 
    , withChatSetting = Nothing
    , additionalData = []
    }
