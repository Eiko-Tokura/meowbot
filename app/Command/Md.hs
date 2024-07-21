module Command.Md where

import Command
import Data.Bifunctor 
import External.MarkdownImage (markdownToImage)
import MeowBot.BotStructure
import MeowBot.CQCode
import MonParserF (ParserF(..))
import qualified MonParserF as MP
import qualified Data.Text as T

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.ReaderState

commandMd :: BotCommand
commandMd = do
  mess <- getEssentialContent <$> ask
  case mess of
    Nothing -> return []
    Just (msg, cid, _, _) -> do
      mEitherStrings <- lift $ mT $ markdownToImage <$> MP.mRunParserF mdParser msg
      case mEitherStrings of
        Nothing -> return []
        Just (Left err) -> return [baSendToChatId cid (T.pack $ "Error o.o occurred while rendering markdown pictures o.o " ++ show err)]
        Just (Right fps) -> return [baSendToChatId cid (T.pack $ concat $ [embedCQCode $ CQImage outPath | outPath <- fps])]
  where 
    mdParser :: ParserF Char String
    mdParser = do
      MP.headCommand "md"
      MP.commandSeparator
      MP.many MP.item
    

turnMdCQCode :: String -> ExceptT String IO String
turnMdCQCode md = fmap (\fps -> concat [embedCQCode (CQImage filepath) | filepath <- fps]) $ ExceptT $ first ((++ "\n Showing the original message: " ++ md) . ("Error o.o occurred while rendering markdown pictures o.o " ++) . show) <$> markdownToImage md


sendIOeToChatIdMd :: EssentialContent -> ExceptT String IO String -> OtherData -> IO ([BotAction], OtherData)
sendIOeToChatIdMd (_, cid, _, mid) ioess other_data = do
  ess <- runExceptT ioe_ess
  return ( [either 
               (baSendToChatId cid . T.pack . ("喵~出错啦：" ++ )) 
               (baSendToChatId cid . T.pack . snd) ess 
             ], 
              either 
                (const other_data) 
                (\(str,_) -> insertMyResponse cid (generateMetaMessage str [MReplyTo mid]) other_data ) ess
          )
  where ioe_ess = do {res <- ioess; mdcq <- turnMdCQCode res; return (res, mdcq)}

