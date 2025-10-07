{-# LANGUAGE OverloadedStrings #-}
module Command.Md where

import Command
import Data.Bifunctor
import Data.FilePathFor
import External.MarkdownImage (markdownToImage)
import MeowBot
import MeowBot.Data.CQHttp.CQCode
import MeowBot.Async
import MeowBot.Parser (Parser, Chars)
import qualified MeowBot.Parser as MP
import qualified Data.Text as T
import Utils.Base64
import External.ChatAPI

import Control.Monad.Effect
import Control.Monad.Trans
import Control.Monad.Trans.Except

commandMd :: BotCommand
commandMd = BotCommand Md $ do
  mess <- getEssentialContent <$> query
  mdParser' <- commandParserTransformByBotName mdParser
  case mess of
    Nothing -> return []
    Just (msg, cid, _, _, _) ->
      case MP.runParser mdParser' msg of
        Nothing -> return []
        Just md -> do
          asyncPureIOBotAction $ do
            mEitherStrings <- runExceptT . markdownToImage $ md
            case mEitherStrings of
              (Left err) -> return [baSendToChatId cid (T.pack $ "Error o.o occurred while rendering markdown pictures o.o " ++ show err)]
              (Right fps) -> do
                eb64s <- mapM (readFileBase64 . useAbsPath) fps
                return [baSendToChatId cid (T.concat $ [either pack (embedCQCode . CQImage64) b64 | b64 <- eb64s])]
  where
    mdParser :: (Chars sb) => Parser sb Char Text
    mdParser = do
      MP.headCommand "md"
      MP.commandSeparator
      MP.some' MP.item

-- | Turn a markdown text into a cq code image, base64 encoded.
turnMdCQCode :: Text -> ExceptT Text IO Text
turnMdCQCode md = do
  fps <-
    ExceptT $
      first ((<> "\n Showing the original message: " <> md) . ("Error o.o occurred while rendering markdown pictures o.o " <>) . tshow) <$>
        runExceptT
          (map (T.pack . useAnyPath) <$> markdownToImage md)
  eb64s <- lift $ mapM (readFileBase64 . unpack) fps
  return $ T.concat [either pack (embedCQCode . CQImage64) b64 | b64 <- eb64s]

sendIOeToChatIdMd :: EssentialContent -> ExceptT Text IO Text -> Meow [BotAction]
sendIOeToChatIdMd (_, cid, _, mid, _) ioess = do
  ess <- lift $ runExceptT ioe_ess
  case ess of
    Right (str, mdcq) -> do
      embedEffT $ insertMyResponseHistory cid (generateMetaMessage str [] [MReplyTo mid])
      return [ baSendToChatId cid mdcq ]
    Left err -> do
      return [ baSendToChatId cid . ("喵~出错啦：" <> ) $ err ]
   where ioe_ess = do {res <- ioess; mdcq <- turnMdCQCode res; return (res, mdcq)}

sendIOeToChatIdMdAsync :: EssentialContent -> ExceptT Text IO Message -> IO (Async (Meow [BotAction]))
--OtherData -> IO ([BotAction], OtherData)
sendIOeToChatIdMdAsync (_, cid, _, mid, _) ioess = async $ do
  ess <- runExceptT ioe_ess
  case ess of
    Right (msg, mdcq) -> return $ do
      embedEffT $ insertMyResponseHistory cid (generateMetaMessage (content msg) [] [MReplyTo mid, MMessage msg])
      return [ baSendToChatId cid mdcq ]
    Left err -> return $ do
      return [ baSendToChatId cid . ("喵~出错啦：" <> ) $ err ]
   where ioe_ess = do
          res <- ioess
          mdcq <- turnMdCQCode (content res)
          return (res, mdcq)

