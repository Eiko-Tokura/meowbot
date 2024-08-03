{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs #-}
module Command.Retract 
  ( commandRetract
  ) where

import Command
import MeowBot.BotStructure
import MeowBot.CQCode
import MonParserF (ParserF(..), cqcodes)
import qualified MonParserF as MP

import Data.List
import Data.Maybe

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

--data Pattern a where
--  PString      :: String    -> Pattern String
--  PFollowLeft  :: Pattern a -> Pattern b -> Pattern a
--  PFollowRight :: Pattern a -> Pattern b -> Pattern b
--  PVoid        :: Pattern a -> Pattern ()
--  PInt         :: Pattern Int

-- | This command is used to automatically retract emoticons that contain certain keywords owo
commandRetract :: BotCommand
commandRetract = BotCommand Retract $ botT $ do
  cqs <- cqcodes <$> MaybeT (metaMessage . getNewMsg <$> ask)
  (_, _, _, mid) <- MaybeT $ getEssentialContent <$> ask
  pureMaybe $ do
    props <- listToMaybe [ props | CQOther "mface" props <- cqs ]
    summary <- lookup "summary" props
    if any (`isInfixOf` summary) ["哭哭", "呜呜"]
      then return [BARetractMsg mid] else return []
  
