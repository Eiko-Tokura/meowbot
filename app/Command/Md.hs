{-# LANGUAGE OverloadedStrings #-}
module Command.Md where

import Command
import Data.Bifunctor 
import Data.FilePathFor
import External.MarkdownImage (markdownToImage)
import MeowBot.BotStructure
import MeowBot.CQCode
import MeowBot.Parser (Parser, tshow)
import qualified MeowBot.Parser as MP
import qualified Data.Text as T
import Data.Text (Text)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.ReaderState

commandMd :: BotCommand
commandMd = BotCommand Md $ do
  mess <- getEssentialContent <$> ask
  mdParser' <- commandParserTransformByBotName mdParser
  case mess of
    Nothing -> return []
    Just (msg, cid, _, _) -> do
      mEitherStrings <- lift $ mT $ runExceptT . markdownToImage <$> MP.runParser mdParser' msg
      case mEitherStrings of
        Nothing -> return []
        Just (Left err) -> return [baSendToChatId cid (T.pack $ "Error o.o occurred while rendering markdown pictures o.o " ++ show err)]
        Just (Right fps) -> return [baSendToChatId cid (T.concat $ [embedCQCode $ CQImage $ T.pack $ useAbsPath outPath | outPath <- fps])]
  where 
    mdParser :: Parser Char Text
    mdParser = do
      MP.headCommand "md"
      MP.commandSeparator
      MP.some' MP.item
    

turnMdCQCode :: Text -> ExceptT Text IO Text
turnMdCQCode md = fmap 
  (\fps -> T.concat [embedCQCode (CQImage filepath) | filepath <- fps]) $ 
    ExceptT $ 
      first ((<> "\n Showing the original message: " <> md) . ("Error o.o occurred while rendering markdown pictures o.o " <>) . tshow) <$> 
        runExceptT 
          (map (T.pack . useAnyPath) <$> markdownToImage md)


sendIOeToChatIdMd :: EssentialContent -> ExceptT Text IO Text -> ReaderStateT r OtherData IO [BotAction]
--OtherData -> IO ([BotAction], OtherData)
sendIOeToChatIdMd (_, cid, _, mid) ioess = do
  ess <- lift $ runExceptT ioe_ess
  case ess of
    Right (str, mdcq) -> do
      modify $ insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid mdcq ]
    Left err -> do
      return [ baSendToChatId cid . ("喵~出错啦：" <> ) $ err ]
   where ioe_ess = do {res <- ioess; mdcq <- turnMdCQCode res; return (res, mdcq)}

