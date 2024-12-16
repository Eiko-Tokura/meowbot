{-# LANGUAGE OverloadedStrings, DerivingVia #-}
module Command.Hangman.Model
  ( hangmanScoring

  , HangmanMod(..), HangmanState(..), AllHangmanStates
  , HangmanAction(..), HangmanPrompt(..)

  , generateDisplayText
  , gameEnded

  , updateHangman

  ) where

import Data.Text (Text)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Time
import Data.Maybe
import Data.Either
import Data.Typeable
import Data.Additional
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Command.Hangman.Data
import Control.Monad.IO.Class
import Control.Monad.State

import Utils.Persist

import Probability.Foundation

-- | the initial HP of the hangman game
hangmanInitialHP :: S.Set HangmanMod -> Int
hangmanInitialHP mods
  | HangmanModEasy `S.member` mods = 8
  | otherwise                      = 7

data HangmanMod
  = HangmanModEasy    -- ^ the mode that hints you the second letter and gives you an extra life
  | HangmanModHidden  -- ^ the mode that hides the underlines
  | HangmanModDark    -- ^ the mode that hides all the spaces
  -- | HangmanModPrecise -- ^ the mode that you must guess the word precisely, if there are repeating letters you need to repeat them
  | HangmanModInitial -- ^ the mode that the first letter will always not be shown
  | HangmanLanguageExpert -- ^ the mode that the word goes wild, difficult words will appear
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow HangmanMod)

data HangmanState = HangmanState
  { hangmanWord      :: Text             -- ^ the word to guess
  , hangmanGuessed   :: [Char]           -- ^ the letters that are guessed, in reverse order, the head is the newest
  , hangmanMods      :: S.Set HangmanMod -- ^ modes that are enabled
  , hangmanHP        :: Int              -- ^ the remaining HP
  , hangmanStartTime :: UTCTime          -- ^ the time when the game starts
  , hangmanEnded     :: Bool             -- ^ whether the game has ended
  , hangmanScore     :: Maybe Double     -- ^ the score of the game
  } deriving (Show, Read, Eq, Ord)
  -- deriving (PersistField, PersistFieldSql) via (PersistUseShow HangmanState)

-- | u is UserId
type AllHangmanStates u = M.Map u HangmanState

instance Typeable u => IsAdditionalData (AllHangmanStates u)

generateDisplayText :: (MonadUniform m) => HangmanState -> m Text
generateDisplayText s = T.pack . concat <$> sequence (
  [ (<>) <$> displayChar s c <*> displaySpace s | c <- T.unpack $ hangmanWord s ]
  <> [ return "\n", displayHP s, return " ", displayMods s ]
  <> [ return "\n", displayHistory s ]
  )
  where displaySpace s
          | HangmanModDark   `S.member` hangmanMods s = return ""
          | otherwise                                 = return " "
        displayChar s c
          | c `elem` hangmanGuessed s                 = return [c]
          | HangmanModDark   `S.member` hangmanMods s = return ""
          | HangmanModHidden `S.member` hangmanMods s = uniformElemS [" ", "  "]
          | otherwise = return "_"
        displayMods s = return $ concatMap (\case 
            HangmanModEasy        -> "E"
            HangmanModHidden      -> "H"
            HangmanModDark        -> "D"
            --HangmanModPrecise     -> "P"
            HangmanModInitial     -> "I"
            HangmanLanguageExpert -> "L"
          ) $ S.toList $ hangmanMods s
        displayHP s = return $ show (hangmanHP s) <> "♥"
        displayHistory s = return $ "Used: " <> reverse (hangmanGuessed s)

----------------------------------------------------------------------------------------------------
-- Stateful

data HangmanAction
  = HangmanGuess Char
  | HangmanGiveup
  | HangmanNewGame (S.Set HangmanMod)

data HangmanPrompt
  = HangmanContinue Text -- ^ display string
  | HangmanEnd (Text, HangmanState) -- ^ can be used to update to database

-- | The only funciton you should use to update AllHangmanStates
updateHangman :: (Ord u, MonadIO m, MonadUniform m) => (u, HangmanAction) -> StateT (AllHangmanStates u) m (Either Text HangmanPrompt)
updateHangman (u, HangmanGuess c) = gets (M.lookup u) >>= \case
  Nothing -> return $ Left "You have not started a game yet o.o"
  Just s  -> do
    case guessChar c s of
      Left  e -> return $ Left e
      Right s -> do
        if gameEnded s 
        then do
          let score = fromMaybe (hangmanScoring s) $ hangmanScore s
              win = not $ uncompletedPlay s
              text | win       = "好厉害！猜对啦！这个单词是" <> hangmanWord s <> "!\n" <> "你的分数是" <> T.pack (printf "%.4f" score) <> " owo"
                   | otherwise = "Oh no, 机会用光了>.< 这个单词其实是" <> hangmanWord s <> "哦 owo" <> "\n" <> "不过你仍然获得了" <> T.pack (printf "%.4f" score) <> "分！"
          modify $ M.delete u
          return $ Right $ HangmanEnd (text, s { hangmanScore = Just score })
        else do
          modify $ M.insert u s
          Right . HangmanContinue <$> generateDisplayText s
updateHangman (u, HangmanGiveup) = gets (M.lookup u) >>= \case
  Nothing -> return $ Left "You have not started a game yet o.o"
  Just s  -> do
    modify $ M.delete u
    let s' = s { hangmanEnded = True, hangmanScore = Just (hangmanScoring s) }
    return $ Right $ HangmanEnd ("好可惜，你放弃了owo 这个单词是" <> hangmanWord s, s')
updateHangman (u, HangmanNewGame mods) = gets (M.lookup u) >>= \case
  Nothing -> do
    s <- lift $ newHangman mods
    modify $ M.insert u s
    Right . HangmanContinue <$> generateDisplayText s
  Just _  -> return $ Left "You have already started a game o.o"

guessChar :: Char -> HangmanState -> Either Text HangmanState
guessChar c s
  | c `notElem` ['a'..'z'] = Left "Please guess a character from a to z o.o!"
  | c `elem` hangmanGuessed s = Left "You have already guessed this character o.o" -- && not (HangmanModPrecise `S.member` hangmanMods s) 
  | c `S.member` wordDedup (hangmanWord s) = Right $ markIfEnded $ s { hangmanGuessed = c : hangmanGuessed s }
  | otherwise = Right $ markIfEnded $ s { hangmanHP = hangmanHP s - 1, hangmanGuessed = c : hangmanGuessed s }

gameEnded :: HangmanState -> Bool
gameEnded s = hangmanHP s <= 0 || not (uncompletedPlay s)

markIfEnded :: HangmanState -> HangmanState
markIfEnded s = s { hangmanEnded = gameEnded s, hangmanScore = if gameEnded s then Just (hangmanScoring s) else hangmanScore s }

newHangman :: (MonadUniform m, MonadIO m) => S.Set HangmanMod -> m HangmanState
newHangman mods = do
  let range | HangmanLanguageExpert `S.member` mods = V.length wordLib
            | otherwise                             = V.length wordLib `div` 2
  wid <- getUniformR (0, range - 1)
  time <- liftIO getCurrentTime
  let s = HangmanState
        { hangmanWord      = wordLib V.! wid
        , hangmanGuessed   = []
        , hangmanMods      = validateMods mods
        , hangmanHP        = hangmanInitialHP mods
        , hangmanStartTime = time
        , hangmanEnded     = False
        , hangmanScore     = Nothing
        }
  if HangmanModEasy `S.member` mods
  then return $ fromRight s $ guessChar (T.index (hangmanWord s) 1) s
  else return s

validateMods :: S.Set HangmanMod -> S.Set HangmanMod
validateMods mods
  | HangmanModDark `S.member` mods && HangmanModHidden `S.member` mods = S.delete HangmanModHidden mods
  | otherwise                                                          = mods

----------------------------------------------------------------------------------------------------
-- Scoring

-- | the deduplicated word
-- apple -> aple
wordDedup :: Text -> S.Set Char
wordDedup = S.fromList . T.unpack
{-# INLINE wordDedup #-}

-- | the same as wordDedup except that remove the **second** letter 
-- apple -> ale
wordDedupEZ :: Text -> S.Set Char
wordDedupEZ w = S.delete (T.index w 1) $ wordDedup w
{-# INLINE wordDedupEZ #-}

wordDedupMod :: HangmanState -> S.Set Char
wordDedupMod s
  | HangmanModEasy `S.member` hangmanMods s = wordDedupEZ $ hangmanWord s
  | otherwise                               = wordDedup $ hangmanWord s
{-# INLINE wordDedupMod #-}

missCount :: HangmanState -> Int
missCount s = length (hangmanGuessed s) - S.size (wordDedup $ hangmanWord s)
{-# INLINE missCount #-}

uncompletedPlay :: HangmanState -> Bool
uncompletedPlay s = not $ wordDedup (hangmanWord s) `S.isSubsetOf` S.fromList (hangmanGuessed s)
{-# INLINE uncompletedPlay #-}

completedPlay :: HangmanState -> Bool
completedPlay = not . uncompletedPlay
{-# INLINE completedPlay #-}

--historyMod :: HangmanState -> S.Set Char

modsMultiplier :: [HangmanMod] -> Double
modsMultiplier [] = 1
modsMultiplier (HangmanModHidden:ms) = 1.12 * modsMultiplier ms
modsMultiplier (HangmanModDark:ms) = 1.3 * modsMultiplier ms
--modsMultiplier (HangmanModPrecise:ms) = 1.25 * modsMultiplier ms -- need to determine later
modsMultiplier (_:ms) = modsMultiplier ms

extraPoints :: [HangmanMod] -> Double
extraPoints [] = 0
extraPoints (HangmanModHidden:ms)  = 4 + extraPoints ms
extraPoints (HangmanModDark:ms)    = 8 + extraPoints ms
--extraPoints (HangmanModPrecise:ms) = 8 + extraPoints ms
extraPoints (HangmanModInitial:ms) = 5 + extraPoints ms
extraPoints (_:ms) = extraPoints ms

hangmanScoring :: HangmanState -> Double
hangmanScoring s 
  = difficultyPoints (hangmanWord s) `onlyIf` completedPlay s
  + extraPoints (S.toList $ hangmanMods s) `onlyIf` completedPlay s
  + modsMultiplier (S.toList $ hangmanMods s) * (-57) 
    * log 
        (subStringSum 
          (filter (`S.member` wordDedupMod s) $ hangmanGuessed s)
          S.empty 
          (if uncompletedPlay s 
            then hangmanInitialHP (hangmanMods s)
            else missCount s + S.size (wordDedupMod s)
          )
        )
    / fromIntegral (S.size $ wordDedup (hangmanWord s))
  where onlyIf x True  = x
        onlyIf _ False = 0

difficultyPoints :: Text -> Double
difficultyPoints w = fromMaybe 0 $ do
  freq <- M.lookup w wordFreqMap
  return $ -7 * log (1 - exp(-7000 * fromIntegral freq / 1_000_000_000))

subStringSum :: [Char] -> S.Set Char -> Int -> Double
subStringSum []     s n = if even (S.size s) then (1-p)^n else -(1-p)^n
  where p = sum [ charFreq U.! (fromEnum c - fromEnum 'a') | c <- S.toList s ]
subStringSum (c:cs) s n = subStringSum cs s n + subStringSum cs (S.insert c s) n
