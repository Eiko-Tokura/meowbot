{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs, TypeApplications #-}
module Command.Retract 
  ( commandRetract
  ) where

import Command
import MeowBot.BotStructure
import MeowBot.CQCode
import MonParserF as MP

import Data.List
import Data.Maybe

import Control.Monad
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
  (msg, cid, uid, mid) <- MaybeT $ getEssentialContent <$> ask
  (msg1, cid1, uid1, mid1) <- MaybeT $ getEssentialContentAtN 2 <$> ask
  pureMaybe $ listToMaybe $ catMaybes
    [ do
        props   <- listToMaybe [ props | CQOther "mface" props <- cqs ]
        summary <- lookup "summary" props
        if any (`isInfixOf` summary) ["哭哭", "呜呜"]
          then return [BARetractMsg mid] else return []
    , do
        props <- listToMaybe [ props | CQOther "image" props <- cqs ]
        fn <- lookup "file" props
        fs <- lookup "file_size" props
        boolToMaybe $ or
          [ fs == "34780"
          , fn == "47292F10974A403AE3C2A01D977879FF.png"
          , fn == "45A7FB5B532C98723658B1A138F4EDAE.png"
          , fn == "DC3C2C1982B3404796FE4470D7E0600C.png"
          ]
        return [BARetractMsg mid]
    , do -- managing the bahavior of another bot
        boolToMaybe $ uid `elem` chinoBotIds
        listToMaybe . catMaybes $ 
          [ void $ mRunParserF 
            ( string "bid:" >> int @Integer >> 
              spaceOrEnter  >> string "捡到来自" >> many item
            ) msg
          , boolToMaybe ( "渣男" `isInfixOf` msg)
          ]
        return [BARetractMsg mid]
    , do
        boolToMaybe $ any (`isInfixOf` msg1) ["上号", "网抑云", "到点了"]
        boolToMaybe $ uid `elem` chinoBotIds
        return [BARetractMsg mid]
    , do
        boolToMaybe $ uid `elem` asllIds
        _ <- listToMaybe [ props | CQOther "image" props <- cqs ]
        return [BARetractMsg mid]
    ]
    where chinoBotIds = [UserId 3287727775, UserId 3055323571, UserId 1714828270]
          asllIds     = [UserId 1102028091]

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

