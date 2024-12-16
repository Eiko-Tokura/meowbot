{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Command.Hangman where

import Command.Hangman.Model
import Data.PersistModel
import qualified Data.Set as S
import MeowBot.Parser
import Data.Typeable
import Data.Additional
import Data.Additional.Default
import Command
import Control.Monad
import Control.Monad.State
import Control.Monad.Readable
import Control.Monad.Trans.Maybe
import MeowBot.Data
import MeowBot
import Utils.RunDB
import qualified Data.Map as M

data HangmanUnit = HangmanUnit deriving (Show, Eq, Read, Typeable)

instance IsAdditionalData HangmanUnit

hangmanParser :: (Chars sb) => Parser sb Char HangmanAction
hangmanParser = asum 
  [ do
      headCommand "hangman" <|> headCommand "hm" <|> headCommand "猜单词"
      (opt_ (commandSeparator >> $(stringQ_ "new"))) >> opt_ commandSeparator >> HangmanNewGame . S.fromList <$> 
        (many (asumE
          [ $(stringQ_ "easy")     <|> void (itemIn "eE") >> return HangmanModEasy
          , $(stringQ_ "hidden")   <|> void (itemIn "hH") >> return HangmanModHidden
          , $(stringQ_ "dark")     <|> void (itemIn "dD") >> return HangmanModDark
          , $(stringQ_ "initial")  <|> void (itemIn "iI") >> return HangmanModInitial
          , $(stringQ_ "language") <|> void (itemIn "lL") >> return HangmanLanguageExpert
          ] <* opt_ commandSeparator
          ) <* end
        )
  , do
      headCommand "hangman" <|> headCommand "hm" <|> headCommand "猜单词"
      opt_ commandSeparator
      $(stringQ_ "giveup") <|> $(stringQ_ "end") <|> $(stringQ_ "quit") <|> $(stringQ_ "放弃") <|> $(stringQ_ "结束") <|> $(stringQ_ "退出")
      return HangmanGiveup
  , do
      headCommand "猜" <|> headCommand "guess" <|> headCommand "g" <|> headCommand ""
      opt_ commandSeparator
      HangmanGuess <$> itemIn ['a'..'z'] <* end
  ]

commandHangman :: BotCommand
commandHangman = BotCommand Hangman $ botT $ do
  (str, cid, uid, mid, _) <- MaybeT $ getEssentialContent <$> query
  tree <- lift $ getFirstTree <$> query
  hangmanParser' <- lift $ commandParserTransformByBotName hangmanParser
  case (runParser hangmanParser' str, runParser hangmanTreeParser tree) of
    (Just cmd, _) -> lift $ doHangman cid mid uid cmd
    (_, Just cmd) -> lift $ doHangman cid mid uid cmd
    _             -> return []

doHangman :: ChatId -> MessageId -> UserId -> HangmanAction -> Meow [BotAction]
doHangman cid _ uid hmact = do
  allHangman <- getTypeWithDef (M.empty :: AllHangmanStates UserId)
  runStateT (updateHangman (uid, hmact)) allHangman >>= \case
    (Left  txt, _) -> return [baSendToChatId cid txt]
    (Right (HangmanContinue txt), alls) -> do
      change @OtherData $ modifyAdditionalDataType @_ @(AllHangmanStates UserId) (const $ Just alls)
      meowSendToChatIdFull cid Nothing [AdditionalData HangmanUnit] [] txt
    (Right (HangmanEnd (txt, s)), alls) -> do
      runDB $ insert_ (hangmanStateToRecord uid s)
      change @OtherData $ modifyAdditionalDataType @_ @(AllHangmanStates UserId) (const $ Just alls)
      return [baSendToChatId cid txt]

hangmanTreeParser :: (IsStream s CQMessage) => Parser s CQMessage HangmanAction
hangmanTreeParser = do
  self <- satisfy $ \cqm -> eventType cqm == SelfMessage && (not . null $ getAdditionalDataType @_ @HangmanUnit cqm)
  let _ = head . getAdditionalDataSavedType @_ @HangmanUnit $ self
  umsg <- satisfy $ (`elem` [GroupMessage, PrivateMessage]) . eventType
  maybe zero return $ runParser (replyHangmanParser) (maybe "" onlyMessage $ metaMessage umsg)
  where
    replyHangmanParser
      = spaces0 >> HangmanGuess <$> 
          (opt_ (asum $ headCommand <$> ["hm", "hangman", ""]) >> opt_ (asum $ string <$> ["猜", "guess", "g"]) >> itemIn ['a'..'z'] <* end)
