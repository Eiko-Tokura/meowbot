{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Command.Aokana.Scripts where

import MeowBot.Parser as MP
import Control.Monad
import qualified Data.Text as T

-- read and parse scripts
newtype Speaker = Speaker Text deriving (Show, Eq)

data AokanaCharacter
     = Asuka
     | Misaki
     | Mashiro
     | Rika
  deriving (Show, Eq)

data AokanaQuery
  = Keyword Text
  | Character AokanaCharacter
  | List
  deriving (Show, Eq)

data MultiLangString = MultiLangString
  { en :: (Maybe Speaker, Text) -- ^ English
  , jp :: (Maybe Speaker, Text) -- ^ Japanese
  , zh :: (Maybe Speaker, Text) -- ^ Chinese
  , tr :: (Maybe Speaker, Text) -- ^ Traditional Chinese
  } deriving (Show, Eq)

getMultiLangString :: MultiLangString -> Maybe Text -- concat all the strings
getMultiLangString (MultiLangString (_, en) (_, jp) (_, zh) (_, tr)) = 
  let strs = [en, jp, zh, tr]
  in case filter (not . T.null) strs of
    [] -> Nothing
    xs -> Just $ T.intercalate "\n" xs

data AssociatedData
  = Voice Text
  | Scene Text
  | BGM   Text
  | Title Text
  | Other Text
  | Episode Text
  deriving (Show, Eq)

data ScriptBlock = ScriptBlock
  { associatedData  :: [AssociatedData]
  , scriptContent   :: Maybe MultiLangString
  , scriptCharacter :: Maybe AokanaCharacter
  } deriving (Show, Eq)

scriptBlockToSpeaker :: ScriptBlock -> Maybe Speaker
scriptBlockToSpeaker = scriptContent >=> fst . jp
-- >=> can be used to extract nested Maybe data structures, because they are Kleisli arrows

newtype Line = Line {unLine :: String}

lines' :: String -> [Line]
lines' = map Line . lines

unlines' :: [Line] -> String
unlines' = unlines . map unLine

lineShape :: Parser Char a -> Parser Line a
lineShape p = maybe MP.zero return . MP.runParser p . unLine =<< MP.getItem

comments :: Parser Line ()
comments = lineShape $ do
  MP.spaces0
  $(MP.stringQ "//")
  return ()

simplify :: (MultiLangString -> (a, Text)) -> ScriptBlock -> Text
simplify f block = maybe "" (snd . f) $ scriptContent block

emptyLine :: Parser Line ()
emptyLine = lineShape $ do
  MP.spaces0
  MP.end

paragraphToScript :: String -> Maybe [ScriptBlock]
paragraphToScript = MP.runParser parseSingleScriptFile . lines'

parseSingleScriptFile :: Parser Line [ScriptBlock]
parseSingleScriptFile = MP.some contigousBlock

contigousBlock :: Parser Line ScriptBlock
contigousBlock = do
  some (comments <|> emptyLine)
  associatedData <- MP.some associatedDataParser 
  multiLangString <- MP.tryMaybe multiLangStringParser
  let mchar = determineCharacter associatedData multiLangString
  return $ ScriptBlock associatedData multiLangString mchar
  where
    -- if asds contains a Voice str term, determine the character by the string completely. If not, consider reading the en part of mls to determine.
    determineCharacter :: [AssociatedData] -> Maybe MultiLangString -> Maybe AokanaCharacter
    determineCharacter _ Nothing = Nothing
    determineCharacter asds (Just mls) = do 
      let voiceStrs = [str | Voice str <- asds]
      if null voiceStrs
      then case en mls of
        (Just (Speaker str), _) -> case T.words str of
          "Asuka"   : _ -> Just Asuka
          "Misaki"  : _ -> Just Misaki
          "Mashiro" : _ -> Just Mashiro
          "Rika"    : _ -> Just Rika
          _ -> Nothing
        _ -> Nothing
      else 
        if "ASUKA"        `T.isPrefixOf` head voiceStrs then Just Asuka
        else if "MISAKI"  `T.isPrefixOf` head voiceStrs then Just Misaki
        else if "MASHIRO" `T.isPrefixOf` head voiceStrs then Just Mashiro
        else if "RIKA"    `T.isPrefixOf` head voiceStrs then Just Rika
        else Nothing

spacesOrTabular :: Parser Char String
spacesOrTabular = some $ MP.itemIn [' ', '\t']

associatedDataParser :: Parser Line AssociatedData
associatedDataParser = lineShape $ foldr1 (<|>) 
  [ $(stringQ "voice0") >> Voice <$> (spacesOrTabular >> some' item)
  , $(stringQ "scene")  >> Scene <$> (spacesOrTabular >> some' item)
  , $(stringQ "bgm0")   >> BGM   <$> (spacesOrTabular >> some' item)
  , $(stringQ "title")  >> Title <$> (spacesOrTabular >> some' item)
  , Other . pack <$> itemsNotIn [specialSymbol]
  ]

specialSymbol :: Char
specialSymbol = '␂'  -- this is a special symbol that marks the start of every content

speakerBrackets = ('【', '】')
contentBrackets = ('「', '」')

multiLangStringParser :: Parser Line MultiLangString
multiLangStringParser = lineShape $ MultiLangString
  <$> singleLangParser
  <*> singleLangParser
  <*> singleLangParser
  <*> singleLangParser

singleLangParser :: Parser Char (Maybe Speaker, Text)
singleLangParser = do
  MP.just specialSymbol
  name <- tryMaybe $ insideBrackets speakerBrackets <* MP.just '：'
  str <- insideBrackets contentBrackets <|> MP.itemsNotIn [specialSymbol]
  return (Speaker . T.pack <$> name, T.pack str)

