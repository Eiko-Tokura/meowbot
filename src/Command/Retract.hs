{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, OverloadedStrings, GADTs, TypeApplications #-}
module Command.Retract
  ( commandRetract
  ) where

import Command
import MeowBot
import MeowBot.CQCode
import MeowBot.Parser as MP

import Data.Maybe
import qualified Data.Text as T

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
  cqs <- cqcodes <$> MaybeT (metaMessage . getNewMsg <$> query)
  (msg, _, uid, mid, _) <- MaybeT $ getEssentialContent <$> query
  (msg1, _, _, _, _) <- MaybeT $ getEssentialContentAtN 2 <$> query
  pureMaybe $ listToMaybe $ catMaybes
    [ do
        props   <- listToMaybe [ props | CQOther "mface" props <- cqs ]
        summary <- lookup "summary" props
        if any (`T.isInfixOf` summary) ["哭哭", "呜呜"]
          then return [BARetractMsg mid] else return []
    , do
        props <- listToMaybe [ props | CQOther "image" props <- cqs ]
        fn <- lookup "file" props
        fs <- lookup "file_size" props
        guard $ or
          [ fs == "34780"
          , fn == "47292F10974A403AE3C2A01D977879FF.png"
          , fn == "45A7FB5B532C98723658B1A138F4EDAE.png"
          , fn == "DC3C2C1982B3404796FE4470D7E0600C.png"
          ]
        return [BARetractMsg mid]
    , do -- managing the bahavior of another bot
        guard $ uid `elem` chinoBotIds
        listToMaybe . catMaybes $
          [ void $ runParser
            ( $(stringQ "bid:") >> int @Integer >>
              spaceOrEnter  >> $(stringQ "捡到来自") >> some item
            ) msg
          , guard ( "渣男" `T.isInfixOf` msg)
          ]
        return [BARetractMsg mid]
    , do
        guard $ any (`T.isInfixOf` msg1) ["上号", "网抑云", "到点了"]
        guard $ uid `elem` chinoBotIds
        return [BARetractMsg mid]
    , do
        guard $ any (`T.isInfixOf` msg1) ["日记"]                     -- "日记" is a keyword
        guard $ uid `elem` chinoBotIds                                -- the bot is chino
        guard $ not . null $ [ props | CQOther "image" props <- cqs ] -- there is an image
        return [BARetractMsg mid]                                     -- retract the message
    , do
        guard $ uid `elem` asllIds
        guard . not . null
          $  [ props | CQOther "image" props <- cqs ]
          <> [ props | CQOther "json" props <- cqs ]
        return [BARetractMsg mid]
    ]
    where chinoBotIds = [UserId 3287727775, UserId 3055323571, UserId 1714828270]
          asllIds     = [UserId 1102028091]

