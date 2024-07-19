module Command.Aokana.Scripts where

import MonParserF as MP
import Data.List
import Control.Monad

-- read and parse scripts
newtype Speaker = Speaker String deriving (Show, Eq)

data AokanaCharacter
     = Asuka
     | Misaki
     | Mashiro
     | Rika
  deriving (Show, Eq)

data AokanaQuery
  = Keyword String
  | Character AokanaCharacter
  | List
  deriving (Show, Eq)

data MultiLangString = MultiLangString
  { en :: (Maybe Speaker, String) -- ^ English
  , jp :: (Maybe Speaker, String) -- ^ Japanese
  , zh :: (Maybe Speaker, String) -- ^ Chinese
  , tr :: (Maybe Speaker, String) -- ^ Traditional Chinese
  } deriving (Show, Eq)

getMultiLangString :: MultiLangString -> Maybe String -- concat all the strings
getMultiLangString (MultiLangString (_, en) (_, jp) (_, zh) (_, tr)) = 
  let strs = [en, jp, zh, tr]
  in case filter (not . null) strs of
    [] -> Nothing
    xs -> Just $ intercalate "\n" xs

data AssociatedData
  = Voice String
  | Scene String
  | BGM   String
  | Title String
  | Other String
  | Episode String
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

lineShape :: ParserF Char a -> ParserF Line a
lineShape p = maybe MP.zero return . MP.mRunParserF p . unLine =<< MP.item

comments :: ParserF Line ()
comments = lineShape $ do
  MP.spaces0
  MP.string "//"
  return ()

simplify :: (MultiLangString -> (a, String)) -> ScriptBlock -> String
simplify f block = maybe "" (snd . f) $ scriptContent block

emptyLine :: ParserF Line ()
emptyLine = lineShape $ do
  MP.spaces0
  MP.end

paragraphToScript :: String -> Maybe [ScriptBlock]
paragraphToScript = MP.mRunParserF parseSingleScriptFile . lines'

parseSingleScriptFile :: ParserF Line [ScriptBlock]
parseSingleScriptFile = MP.many0 contigousBlock

contigousBlock :: ParserF Line ScriptBlock
contigousBlock = do
  many0 (comments <> emptyLine)
  associatedData <- MP.many associatedDataParser 
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
        (Just (Speaker str), _) -> case words str of
          "Asuka"   : _ -> Just Asuka
          "Misaki"  : _ -> Just Misaki
          "Mashiro" : _ -> Just Mashiro
          "Rika"    : _ -> Just Rika
          _ -> Nothing
        _ -> Nothing
      else 
        if "ASUKA"        `isPrefixOf` head voiceStrs then Just Asuka
        else if "MISAKI"  `isPrefixOf` head voiceStrs then Just Misaki
        else if "MASHIRO" `isPrefixOf` head voiceStrs then Just Mashiro
        else if "RIKA"    `isPrefixOf` head voiceStrs then Just Rika
        else Nothing

spacesOrTabular :: ParserF Char String
spacesOrTabular = MP.itemsIn [' ', '\t']

associatedDataParser :: ParserF Line AssociatedData
associatedDataParser = lineShape $ foldr1 (<>) 
  [ string "voice0" >> Voice <$> (spacesOrTabular >> many item)
  , string "scene"  >> Scene <$> (spacesOrTabular >> many item)
  , string "bgm0"   >> BGM   <$> (spacesOrTabular >> many item)
  , string "title"  >> Title <$> (spacesOrTabular >> many item)
  , Other <$> itemsNotIn [specialSymbol]
  ]

specialSymbol :: Char
specialSymbol = '␂'  -- this is a special symbol that marks the start of every content

speakerBrackets = ('【', '】')
contentBrackets = ('「', '」')

multiLangStringParser :: ParserF Line MultiLangString
multiLangStringParser = lineShape $ MultiLangString
  <$> singleLangParser
  <*> singleLangParser
  <*> singleLangParser
  <*> singleLangParser

singleLangParser :: ParserF Char (Maybe Speaker, String)
singleLangParser = do
  MP.just specialSymbol
  name <- tryMaybe $ insideBrackets speakerBrackets <* MP.just '：'
  str <- insideBrackets contentBrackets <> MP.itemsNotIn [specialSymbol]
  return (Speaker <$> name, str)

