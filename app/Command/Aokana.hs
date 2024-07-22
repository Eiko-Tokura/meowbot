{-# LANGUAGE OverloadedStrings, BlockArguments, TypeApplications #-}
module Command.Aokana where

import MeowBot.BotStructure
import Command
import Command.Aokana.Scripts
import qualified Command.User as U

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

import MonParserF (ParserF(..))
import qualified MonParserF as MP

import External.ChatAPI (Message(..))
import MeowBot.CQCode

import Data.Either
import Data.Maybe 
import Data.List
import Data.Char(toLower)
import Probability.Foundation
import System.Directory
import System.FilePath
import qualified Data.Text as T


aokanaRelPath :: FilePath
aokanaRelPath = "aokana"

voiceRelPath :: FilePath
voiceRelPath = "voice"

scriptRelPath :: FilePath
scriptRelPath = "scripts"

aokanaParser :: ParserF Char [AokanaQuery]
aokanaParser = do
  _ <- MP.headCommand "aokana"
  MP.many0 $ foldr1 (<>) $ (MP.commandSeparator >>) <$>
    [ queryCharacter
    , queryKeyword
    , MP.string "-list" >> return List
    ]
  where
    queryCharacter = foldr1 (<>) 
                [ MP.string "-asuka"   >> return (Character Asuka)
                , MP.string "-misaki"  >> return (Character Misaki)
                , MP.string "-mashiro" >> return (Character Mashiro)
                , MP.string "-rika"    >> return (Character Rika)
                ]
    queryKeyword = Keyword <$> MP.nonFlagWord

searchScripts :: [AokanaQuery] -> [ScriptBlock] -> [ScriptBlock]
searchScripts queries = filter $ \block -> all (queryBlock block) queries
  where
    queryBlock :: ScriptBlock -> AokanaQuery -> Bool
    queryBlock block q
      | Just _ <- scriptContent block , Keyword keyword <- q = maybe False (keyword `isInfixOf`) $ scriptContent block >>= getMultiLangString
      | Just _ <- scriptContent block , Character char <- q  = (Just char ==) $ scriptCharacter block
      | Just _ <- scriptContent block , List <- q            = True
      | otherwise = False

commandAokana :: BotCommand
commandAokana = botT $ do
  (msg, cid, _, mid) <- MaybeT $ getEssentialContent <$> ask
  queries <- pureMaybe $ MP.mRunParserF aokanaParser msg
  other_data <- lift get
  let sd = savedData other_data
  cid <- pureMaybe $ U.checkCidInAllowedGroups sd cid
  let scripts = aokana other_data
      results = searchScripts queries scripts
      hasVoice = filter (\block -> not $ null [ () | Voice _ <- associatedData block ]) results
  lift $ sendResults cid mid results hasVoice queries
  where 
    sendResults cid mid results hasVoice queries 
      | null results        = return [baSendToChatId cid "啥也没有找到！o.o"]
      | List `elem` queries = return [baSendToChatId cid $ T.pack $ intercalate "\n" $ restrictNumber 5 $ simplify zh <$> results]
      | null hasVoice       = return [baSendToChatId cid $ T.pack $ ("这段话没有语音owo\n" ++) $ simplify zh $ head results]
      | otherwise           = do
          ranBlock <- lift $ (hasVoice !!) <$> getUniformR (0, length hasVoice - 1)
          cd <- lift getCurrentDirectory
                       
          other_data <- get
          let voice = head [v | Voice v <- associatedData ranBlock]
              simplifiedBlock = intercalate "\n"
                                  [ simplify jp ranBlock
                                  , simplify zh ranBlock
                                  ]
          charPrompt <- mT $ (`aokanaCharacterPrompts` aokana other_data) <$> scriptCharacter ranBlock 
          case charPrompt of
            Nothing -> return ()
            Just charPrompt -> lift $ putStrLn $ T.unpack $ content charPrompt
          modify $ insertMyResponse cid  -- this will make the message repliable, potentially much more fun!
                       (generateMetaMessage simplifiedBlock (MReplyTo mid : maybeToList (MSysMessage <$> charPrompt)) ) 
          return [ baSendToChatId cid $ T.pack simplifiedBlock
                 , baSendToChatId cid $ T.pack $ embedCQCode $ CQRecord $ voicePath cd voice
                 ]
    restrictNumber :: Int -> [String] -> [String]
    restrictNumber n xs =  [show i ++ " " ++ x | (i, x) <- zip [1 :: Int ..] $ take n xs]
                        ++ ["(显示了前" ++ show (min n (length xs)) ++ "/" ++ show (length xs) ++ "条)"]

voicePath :: FilePath -> String -> FilePath
voicePath cd voiceId = cd </> aokanaRelPath </> voiceRelPath </> (toLower <$> voiceId ++ ".ogg")

getAllVoices :: IO [(AokanaCharacter, String, FilePath)]
getAllVoices = do
  files <- listDirectory (aokanaRelPath </> voiceRelPath)
  -- filter all '.ogg' files
  let oggFiles = filter (isSuffixOf ".ogg") files
  cd <- getCurrentDirectory
  let voicePath = cd </> aokanaRelPath </> voiceRelPath
  return (MP.mRunParserF (aokanaVoiceFileParser voicePath) `mapMaybe` oggFiles)
  where
    aokanaVoiceFileParser :: FilePath -> ParserF Char (AokanaCharacter, String, FilePath)
    aokanaVoiceFileParser vdir = do
      (characterStr, character) <- 
                   (MP.string "asuka"   >> return ("asuka", Asuka))
                <> (MP.string "misaki"  >> return ("misaki", Misaki))
                <> (MP.string "mashiro" >> return ("mashiro", Mashiro))
                <> (MP.string "rika"    >> return ("rika", Rika))
      void $ MP.just '_'
      voiceId <- MP.till '.'
      extension <- MP.string ".ogg"
      return (character, voiceId, vdir </> (characterStr ++ "_" ++ voiceId ++ extension))

getAllScripts :: IO [ScriptBlock]
getAllScripts = fmap (fromRight []) $ runExceptT $ do
  files <- ExceptT $ try @SomeException $ listDirectory (aokanaRelPath </> scriptRelPath)
  let scriptFiles = filter (isSuffixOf ".bs5") files
  cd <- lift getCurrentDirectory
  let scriptPath file = cd </> aokanaRelPath </> scriptRelPath </> file
  scriptBlocks <- forM scriptFiles $ \file -> do
    script <- ExceptT $ try $ readFile $ scriptPath file
    return $ paragraphToScript script
  return $ concat $ catMaybes scriptBlocks

aokanaCharacterPrompts :: (MonadUniform m) => AokanaCharacter -> [ScriptBlock] -> m Message
aokanaCharacterPrompts Asuka scripts = do
  examples <- examplesToString <$> findCharacterExampleBlocks Asuka scripts
  return $ Message "system" $ T.pack $ intercalate "\n" 
    ["你是仓科明日香，一个阳光开朗的女孩，来自《苍之彼方的四重奏》。你有着一头柔顺的棕色长发，灿烂的笑容，总是充满了活力和正能量。你热爱飞行，梦想成为一名顶级飞行员。你性格乐观，有时有点天然呆，但总是用积极的态度面对生活中的挑战。你喜欢和朋友们在一起，尤其是在飞行部，大家互相支持，共同进步。"
    , ""
    , "你经常鼓励别人，也喜欢接受新的挑战。你相信只要努力，就一定能实现梦想。无论遇到什么困难，你总是带着自信和勇气去面对，感染身边的每一个人。"
    , ""
    , "展现出你的乐观和坚定，以及对飞行的热爱吧！以下是一些明日香的台词："
    , ""
    , examples
    ]
aokanaCharacterPrompts Misaki scripts = do
  examples <- examplesToString <$> findCharacterExampleBlocks Misaki scripts
  return $ Message "system" $ T.pack $ intercalate "\n" 
    [ "你是美咲，一个活泼而机智的女孩，来自《苍之彼方的四重奏》。你有着一头飘逸的蓝色长发，明亮的笑容，总是充满了活力和热情。你性格开朗，喜欢开玩笑，有时显得有些调皮。你对飞行充满了热情，喜欢挑战极限，是飞行部的中坚力量。"
    , "你喜欢交朋友，乐于助人，总是能在团队中营造轻松愉快的氛围。尽管你有时显得有些轻佻，但在关键时刻，你会展现出强大的决心和毅力。你相信只要有热情和努力，没有什么是不可能的。"
    , ""
    , "展现出你的机智和活力，以及对飞行的激情吧！以下是一些美咲的台词："
    , ""
    , examples
    ]
aokanaCharacterPrompts Mashiro scripts = do
  examples <- examplesToString <$> findCharacterExampleBlocks Mashiro scripts
  return $ Message "system" $ T.pack $ intercalate "\n" 
    [ "你是有坂真白，一个内向而害羞的女孩，来自《苍之彼方的四重奏》。你有着一头白色的短发，紫色的眼睛，给人一种神秘而可爱的感觉。你性格内向，但内心非常细腻和温柔。你喜欢猫，常常和自己心爱的猫咪在一起。你对飞行也有着很大的热情，尽管有时会缺乏自信，但你总是努力去克服自己的恐惧和不安。"
    , ""
    , "在与他人的互动中，你可能会显得有些害羞，但一旦熟悉之后，你会展现出非常温柔和体贴的一面。你对朋友非常重视，愿意为他们付出一切。"
    , ""
    , "展现出你的温柔，内向害羞！以下是一些真白的台词："
    , ""
    , examples
    ]
aokanaCharacterPrompts Rika scripts = do
  examples <- examplesToString <$> findCharacterExampleBlocks Rika scripts
  return $ Message "system" $ T.pack $ intercalate "\n" 
    [ "你是市之濑莉佳，一个聪明而独立的女孩，来自《苍之彼方的四重奏》。你有着一头整齐的黑色长发，深邃的眼神中透露出坚定和自信。你是飞行部的王牌，技术高超，性格冷静且理智。你总是以严谨的态度对待飞行和训练，追求完美，不断挑战自我。"
    , ""
    , "尽管你表面上显得有些冷漠，但实际上你非常关心你的朋友和团队。你相信团队合作的重要性，也愿意在关键时刻帮助和指导他人。你拥有强烈的责任感和领导能力，是大家信赖的伙伴。"
    , ""
    , "展现出你的冷静和自信，以及对团队的责任感吧！以下是一些莉佳的台词："
    , ""
    , examples
    ]

findCharacterExampleBlocks :: (MonadUniform m) => AokanaCharacter -> [ScriptBlock] -> m [ScriptBlock]
findCharacterExampleBlocks _ [] = return []
findCharacterExampleBlocks char sb = do
  let psb = zip [0..] sb
      charTable = [ (i, b) | (i, b) <- psb, scriptCharacter b == Just char ]
  (i, _) <- (charTable !!) <$> getUniformR (0, length charTable - 1)
  return [ snd $ psb !! j | j <- [max 0 (i - 5) .. min (length psb - 1) (i + 5)] ]

examplesToString :: [ScriptBlock] -> String
examplesToString = intercalate "\n\n" . map (\s -> maybeSpeaker s ++ simplify jp s ++ "\n" ++ simplify zh s)
  where maybeSpeaker s = case scriptBlockToSpeaker s of
                            Just (Speaker sp) -> sp ++ "："
                            Nothing -> ""

