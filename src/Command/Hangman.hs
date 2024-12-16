{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Command.Hangman where

import Command.Hangman.Model
import Command.Hangman.Ranking
import Data.PersistModel
import qualified Data.Set as S
import MeowBot.Parser
import qualified Data.Text as T
import Text.Printf
import Data.Typeable
import Data.Additional
import Data.Additional.Default
import Command
import Control.Monad
import Control.Monad.State
import Control.Monad.Readable
import Control.Monad.Trans.Maybe
import Data.Maybe
import MeowBot.Data
import MeowBot
import Utils.RunDB
import qualified Data.Map as M

data HangmanUnit = HangmanUnit deriving (Show, Eq, Read, Typeable)

instance IsAdditionalData HangmanUnit

hangmanParser :: (Chars sb) => Parser sb Char (Either HangmanAction ViewRanking)
hangmanParser = Left <$> asum 
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
  <|> Right <$> do
    headCommand "hangman" <|> headCommand "hm" <|> headCommand "猜单词"
    commandSeparator
    asumE [ ($(stringQ_ "ranking") <|> $(stringQ_ "rank")) >> return ViewGlobalRanking
          , $(stringQ_ "info") >> return ViewPersonalRanking
          ]

helpHangman :: Text
helpHangman = T.unlines
  [ "猜单词游戏"
  , ":hangman [new] [easy/E|hidden/H|dark/D|initial/I|language/L]... - 开始新游戏"
  , "可以使用等价命令:hm 和 :猜单词"
  , ""
  , ":hangman giveup - 放弃游戏"
  , ""
  , ":猜[字母] - 猜字母"
  , "可以使用:[字母] / :guess [letter]"
  , "也可以回复消息进行猜字母"
  , ""
  , "Mods简介"
  , "easy(E): 增加1HP，开局提示你第二个字母 (分数降低)"
  , "hidden(H): 下划线变成(不稳定的)空格 (分数×1.12 +4)"
  , "dark(D): 不显示任何下划线或者空格 (分数×1.3 +8)"
  , "initial(I): 不显示第一个字母 (分数 +5)"
  , "language(L): 开启后可以遇到更多单词"
  , "可以同时使用多个mods"
  , ""
  , ":hangman info - 查看个人排行榜"
  , ":hangman ranking - 查看全局排行榜"
  ]

commandHangman :: BotCommand
commandHangman = BotCommand Hangman $ botT $ do
  (str, cid, uid, _, _) <- MaybeT $ getEssentialContent <$> query
  tree <- lift $ getFirstTree <$> query
  nickname <- MaybeT $ queries $ senderNickname <=< sender . getNewMsg
  hangmanParser' <- lift $ commandParserTransformByBotName hangmanParser
  case (runParser hangmanParser' str, runParser hangmanTreeParser tree) of
    (Just cmd, _) -> lift $ doHangman cid nickname uid cmd
    (_, Just cmd) -> lift $ doHangman cid nickname uid (Left cmd)
    _             -> return []

doHangman :: ChatId -> Text -> UserId -> Either HangmanAction ViewRanking -> Meow [BotAction]
doHangman cid _ uid (Left hmact) = do
  allHangman <- getTypeWithDef (M.empty :: AllHangmanStates UserId)
  runStateT (updateHangman (uid, hmact)) allHangman >>= \case
    (Left  txt, _) -> return [baSendToChatId cid txt]
    (Right (HangmanContinue txt), alls) -> do
      change @OtherData $ modifyAdditionalDataType @_ @(AllHangmanStates UserId) (const $ Just alls)
      meowSendToChatIdFull cid Nothing [AdditionalData HangmanUnit] [] txt
    (Right (HangmanEnd (txt, s)), alls) -> do
      moldUserRank <- getUserRank uid
      runDB $ insert_ (hangmanStateToRecord uid s)
      nickname <- queries $ senderNickname <=< sender . getNewMsg
      (newpp, newrank) <- updateTotalPP uid (fromMaybe "" nickname)
      change @OtherData $ modifyAdditionalDataType @_ @(AllHangmanStates UserId) (const $ Just alls)
      return [baSendToChatId cid (txt <> rankChange moldUserRank newpp newrank)]
doHangman cid _ _ (Right ViewGlobalRanking) = do
  ranking <- getRanking
  return [baSendToChatId cid $ T.unlines $ restrictNumber 20 $ showRanking <$> ranking]
doHangman cid nickName uid (Right ViewPersonalRanking) = do
  (totalPP, rank) <- updateTotalPP uid nickName
  return [baSendToChatId cid $ "PP: " <> tshow totalPP <> ", Rank: " <> tshow rank]

showRanking :: HangmanRanking -> Text
showRanking r = hangmanRankingUserNickName r <> " " <> tshow (hangmanRankingUserId r) <> " " <> tshow (hangmanRankingTotalPP r) <> "pp"
  -- "#" <> tshow (hangmanRankingRank r) <> " " <> 

tshowfloat :: Double -> Text
tshowfloat = T.pack . printf "%.4f"

rankChange :: Maybe (Double, Int) -> Double -> Int -> Text
rankChange Nothing newpp newrank = "\nPP: " <> tshowfloat newpp <> ", Rank: " <> tshow newrank
rankChange (Just (oldpp, oldrank)) newpp newrank
  | oldpp == newpp && oldrank == newrank = ""
  | otherwise = "\nPP: " <> tshowfloat newpp <> (if newpp > oldpp then " (+" <> tshowfloat (newpp - oldpp) <> ")" else "") <> ", Rank: " <> tshow newrank <> (if newrank < oldrank then " (+" <> tshow (oldrank - newrank) <> ")" else "")

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
