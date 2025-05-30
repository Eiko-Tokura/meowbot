{-# LANGUAGE OverloadedStrings, TypeOperators, TypeApplications, DataKinds, TemplateHaskell, FlexibleContexts #-}
module Command.Study
  ( commandStudy
  , commandBookMan
  , helpStudy
  , makeBook
  , studyParser
  ) where

-- this command involves:
-- reading pdf files
-- uploading pdf files
-- potentially more fun stuffs like learning languages with the help of GPT.

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.ReaderState

import Data.Maybe (listToMaybe, fromMaybe, catMaybes, mapMaybe)
import Data.List
import Data.Coerce
import Data.FilePathFor
import qualified Data.Text as T
import Safe (atMay)

import Command
import MeowBot
import MeowBot.CQCode
import MeowBot.Data.Book
import MeowBot.Parser as MP
import Probability.Foundation (uniformElemS)
import External.MarkdownImage

-- we want to implement the following functions, implement according to the order below:
--
-- 1. there is a list of books, book data
-- books are pdf files, they will be converted to images and available to be accessed by commands
--
-- 2. users are allowed to upload books by commands

-- we will utilize the database package persistent with sqlite3 to store the book data
-- this is an excellent opportunity to learn how to use the database package persistent with sqlite3! owo

--import Database.Persist.Sqlite
--import Database.Persist.TH

newtype PageInsideType = PageInsideType Int deriving Show

type (<+>) = Either
infixr 6 <+>

data StudyQuery
  = SearchBook [SearchQuery]
  | ReadBook BookName
      (   ( PageType
          , [ PageInsideType <+> PageNumber <+> AbsolutePageNumber ]
          )
      <+> [ PageNumber <+> AbsolutePageNumber ]
      )
  | InfoEdit BookName Action (Maybe BookInfoType)
  deriving Show
-- next plan: write a type level combinator for parser
-- in that way the studyParser will be expressed as
-- type StudyParser
--    =    HeadCommand "study" :-: [SearchQuery]
--    :<>: HeadCommand "read"  :-: Word BookName :-: (PageNumber <+> AbsolutePageNumber)

data BookManagement
  = Upload BookName (FilePathFor Abs File PDF)
  | Delete BookName
  | LocalMakeBook BookName (FilePathFor Rel File PDF)
  | LocalAddBook BookName (FilePathFor Rel Directory Image)

data Action = Set | Remove | Show deriving Show

data BookInfoType
  = Author Text
  | Offset PageNumberOffset
  | Tags [BookTag]
  | PageTypeInfo [PageNumber <+> AbsolutePageNumber] (Maybe PageType)
  deriving Show

newtype SearchQuery = Keyword Text deriving Show

helpStudy :: T.Text
helpStudy = T.unlines
  [ "study search <keywords> : 搜索书籍"
  , "study read <bookname> <pages/[abs-pages]> : 阅读书籍"
  , "study read <bookname> <pageType> <+pageInType/pages/[abs-pages]> : 阅读书籍指定类型页面"
  , "study info <bookname> show : 查看书籍信息"
  , "study info <bookname> show author/offset/tags/pagetype: 查看书籍信息"
  , "study info <bookname> set/remove/show author/offset/tags : 修改书籍信息"
  , "study info <bookname> set/remove/show <pageType> <pages/[abs-pages]> menu/content/exercise/cover/foreword/<any string> : 修改页面类型"
  , "pageType : menu/content/exercise/cover/foreword/<any string>"
  ]

ePageNumber :: (Chars sb) => Parser sb Char (Either PageNumber AbsolutePageNumber)
ePageNumber = (PageNumber <$> int) |+| (AbsolutePageNumber <$> (just '[' *> int <* just ']'))

pageTypeP :: (Chars sb) => Parser sb Char PageType
pageTypeP = MP.asumE
  [ $(stringQ "menu")     >> return Menu
  , $(stringQ "chapter")  >> return Chapter
  , $(stringQ "exercise") >> return Exercise
  , $(stringQ "cover")    >> return Cover
  , $(stringQ "foreword") >> return Foreword
  -- , MarkedAs <$> word
  ]

studyParser :: (Chars sb) => Parser sb Char StudyQuery
studyParser = headCommand "study" >> commandSeparator >> MP.asumE
  [ $(stringQ "search") >>
      SearchBook <$> many (commandSeparator >> (Keyword <$> word'))
  , $(stringQ "read") >> commandSeparator >>
      ReadBook <$> word'
               <*> ( ((,) <$> (commandSeparator >> pageTypeP)
                          <*> many (commandSeparator >> (just '+' *> (PageInsideType <$> positiveInt) |+| ePageNumber)))
                   |+| many (commandSeparator >> ePageNumber)
                   )
  , $(stringQ "info") >> commandSeparator >>
      InfoEdit <$> word'
               <*> (commandSeparator
                   >> ($(stringQ "set") <|> $(stringQ "add") >> return Set)
                   <|> ($(stringQ "remove") >> return Remove)
                   <|> ($(stringQ "show")   >> return Show)
                   )
               <*> (commandSeparator
                   >> ($(stringQ "author") >> canBeEmpty (commandSeparator >> (Author <$> word')))
                   <|> ($(stringQ "offset") >> canBeEmpty (commandSeparator >> (Offset . PageNumberOffset <$> int)))
                   <|> ($(stringQ "tags")   >> canBeEmpty (commandSeparator >> (Tags <$> intercalateBy0 commandSeparator (BookTag <$> word'))))
                   <|> ($(stringQ "pagetype") >> canBeEmpty (commandSeparator >> PageTypeInfo <$> intercalateBy0 commandSeparator ePageNumber
                          <*> canBeEmpty
                              ( commandSeparator
                              >> ($(stringQ "menu")     >> return Menu)
                              <|> ($(stringQ "exercise") >> return Exercise)
                              <|> ($(stringQ "chapter")  >> return Chapter)
                              <|> ($(stringQ "cover")    >> return Cover)
                              <|> ($(stringQ "foreword") >> return Foreword )
                              <|> (MarkedAs <$> word)
                              )
                          )
                      )
                   )
  ]

restrictPages :: [a] -> [a]
restrictPages = take 5

commandStudy :: BotCommand
commandStudy = BotCommand Study $ botT $ do
  (msg, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  studyParser' <- lift $ commandParserTransformByBotName studyParser
  squery <- pureMaybe $ MP.runParser studyParser' msg
  other_data <- query
  let allBooks = books $ savedData other_data
  case squery of
    SearchBook searchQs     ->
      let searchResult = searchBooks searchQs allBooks
      in return [ baSendToChatId cid $ T.intercalate "\n" $ restrictNumber 5 $ map simplifiedListing searchResult ]

    ReadBook bookname (Right pages) -> do
      match <- pureMaybe $ listToMaybe [ book | book <- allBooks, bookname `T.isInfixOf` book_name book ]
      let offset = fromMaybe 0 $ bookInfo_pageNumberOffset $ book_info match
          absPageNumbersToView = map (offsetPageNumber offset `either` id) pages  :: [AbsolutePageNumber]
          pagesToView = [ page | page <- book_pages match, page_absoluteNumber page `elem` absPageNumbersToView ]
      if null pagesToView then do
        randomPage <- uniformElemS $ book_pages match
        let offset = fromMaybe 0 $ bookInfo_pageNumberOffset $ book_info match
        return
          [ baSendToChatId cid $ T.intercalate "\n"
            [ book_name match
            , tshow (coerce @_ @Int $ reverseOffsetPageNumber offset (page_absoluteNumber randomPage)) <> ", " <> tshow (coerce @_ @Int $ page_absoluteNumber randomPage) <> "/" <> tshow (length $ book_pages match)
            , embedCQCode $ CQImage $ page_imagePath randomPage
            ]
          ]
      else
        return [ baSendToChatId cid $ T.concat $ restrictPages $ map (embedCQCode . CQImage . page_imagePath) pagesToView ]

    ReadBook bookname (Left (pageType, pages')) -> do
      match <- pureMaybe $ listToMaybe [ book | book <- allBooks, bookname `T.isInfixOf` book_name book ]
      let offset = fromMaybe 0 $ bookInfo_pageNumberOffset $ book_info match
          pagesOfGivenType = [ page | page <- book_pages match, page_type page == Just pageType ]
          absPageNumbersToView =
            if null pages' then map page_absoluteNumber pagesOfGivenType -- if no pages are given, view all pages of the given type
            else mapMaybe ((fmap page_absoluteNumber . atMay pagesOfGivenType . coerce) `either` ((Just . offsetPageNumber offset) `either` Just)) pages'  :: [AbsolutePageNumber]
          pagesToView = [ page | page <- book_pages match, page_absoluteNumber page `elem` absPageNumbersToView ]
      return [ baSendToChatId cid $ T.concat $ restrictPages $ map (embedCQCode . CQImage . page_imagePath) pagesToView ]

    InfoEdit bookname Set (Just infoType) -> do
      match <- pureMaybe $ listToMaybe [ book | book <- allBooks, bookname `T.isInfixOf` book_name book ]
      let offset = fromMaybe 0 $ bookInfo_pageNumberOffset $ book_info match
      let newInfo = case infoType of
            Author author -> (book_info match) { bookInfo_author = Just author }
            Offset offset -> (book_info match) { bookInfo_pageNumberOffset = Just offset }
            Tags tags     -> (book_info match) { bookInfo_tags = tags }
            _ -> book_info match
          newPages = case infoType of
            PageTypeInfo pages pageType -> [ if page_absoluteNumber page `elem` map (either (offsetPageNumber offset) id) pages then page { page_type = pageType } else page | page <- book_pages match ]
            _ -> book_pages match
          newBooks = [ if book == match
                       then book { book_info = newInfo
                                 , book_pages = newPages
                                 }
                       else book
                     | book <- allBooks ]
      lift $ change $ \other_data -> other_data { savedData = (savedData other_data) {books = newBooks} }
      return [ baSendToChatId cid $ T.pack $ "修改成功！\n" ++ show newInfo ]

    InfoEdit bookname Remove (Just infoType) -> do -- removing some information
      match <- pureMaybe $ listToMaybe [ book | book <- allBooks, bookname `T.isInfixOf` book_name book ]
      let offset = fromMaybe 0 $ bookInfo_pageNumberOffset $ book_info match
      let newInfo = case infoType of
            Author _ -> (book_info match) { bookInfo_author = Nothing }
            Offset _ -> (book_info match) { bookInfo_pageNumberOffset = Nothing }
            Tags _   -> (book_info match) { bookInfo_tags = [] }
            _ -> book_info match
          newPages = case infoType of
            PageTypeInfo pages (Just pageType) -> -- removing a given pageType on given pages
              [ if page_absoluteNumber page `elem` map (either (offsetPageNumber offset) id) pages
                   && page_type page == Just pageType
                then page { page_type = Nothing } else page | page <- book_pages match
              ]
            _ -> book_pages match
          newBooks = [ if book == match
                       then book { book_info = newInfo
                                 , book_pages = newPages
                                 }
                       else book
                     | book <- allBooks ]
      lift $ change $ \other_data -> other_data { savedData = (savedData other_data) {books = newBooks} }
      return [ baSendToChatId cid $ T.pack $ "修改成功！\n" ++ show newInfo ]

    InfoEdit bookname Show (Just infoType) -> do
      match <- pureMaybe $ listToMaybe [ book | book <- allBooks, bookname `T.isInfixOf` book_name book ]
      let info = case infoType of
            Author _ -> bookInfo_author $ book_info match
            Offset _ -> Just $ tshow $ bookInfo_pageNumberOffset $ book_info match
            Tags _   -> Just $ tshow $ bookInfo_tags $ book_info match
            PageTypeInfo pages _ -> Just $ T.intercalate "\n" $ [ tshow (page_absoluteNumber page, page_type page) | page <- book_pages match, page_absoluteNumber page `elem` map (either (offsetPageNumber $ fromMaybe 0 $ bookInfo_pageNumberOffset $ book_info match) id) pages ]
      return [ baSendToChatId cid $ T.pack $ show info ]

    InfoEdit bookname Show Nothing -> do
      match <- pureMaybe $ listToMaybe [ book | book <- allBooks, bookname `T.isInfixOf` book_name book ]
      return [ baSendToChatId cid $ T.pack $ show $ book_info match ]
    _ -> return [ baSendToChatId cid "o.o?" ]

searchBooks :: [SearchQuery] -> [Book] -> [Book]
searchBooks sq books =
  [ book | book <- books
         , let bookinfo = book_info book
         , all (\(Keyword kw) -> any ((T.toLower kw `T.isInfixOf`) . T.toLower) $ catMaybes
                  [ bookInfo_author bookinfo ]
                  ++ [ book_name book ]
                  ++ ( useBookTag <$> bookInfo_tags bookinfo )
               ) sq
  ]

simplifiedListing :: Book -> Text
simplifiedListing book = T.unwords $ catMaybes
     [ Just $ book_name book
     , (<> ")") . ("(" <> ) <$> bookInfo_author (book_info book)
     ]

makeBook :: BookName -> BookInfo -> FilePathFor anyPathType File PDF -> ExceptT SomeException IO Book
makeBook bookname bookinfo pdf = do
  page_images <- sortOn fst . coerce <$> pdfToImageWithPageNumber pdf
  pdfAbFp <- toAbsPath pdf
  bookPages <- forM page_images $ \(absPageNum, imgFp) -> do
    let pageType = Nothing
    imgAbFp <- toAbsPath imgFp
    return $ BookPage (pack $ useAnyPath imgAbFp) absPageNum pageType
  return $ Book bookname (Just $ useAbsPath pdfAbFp) bookPages bookinfo

makeBookFromImageDir :: (ComposablePath anyPathType Rel) => BookName -> BookInfo -> Maybe (FilePathFor anyPathType' File PDF) -> FilePathFor anyPathType Directory Image -> ExceptT SomeException IO Book
makeBookFromImageDir bookname bookinfo mpdfFile imgDir = do
  page_images <- sortOn fst . map (\image -> (AbsolutePageNumber . readPageNumber . useRelPath . takeBaseName $ image , imgDir </> image)) <$> listDirectory imgDir
  mpdfAbFp <- case mpdfFile of
    Just pdfFile -> Just <$> toAbsPath pdfFile
    Nothing -> return Nothing
  bookPages <- forM page_images $ \(absPageNum, imgFp) -> do
    let pageType = Nothing
    imgAbFp <- toAbsPath imgFp
    return $ BookPage (pack $ useAnyPath imgAbFp) absPageNum pageType
  return $ Book bookname (useAbsPath <$> mpdfAbFp) bookPages bookinfo

readPageNumber = fromMaybe (error "page number un-readable") . runParser ($(stringQ "page_") *> positiveInt <* canBeEmpty $(stringQ ".png"))

commandBookMan :: BotCommand
commandBookMan = BotCommand BookMan $ botT $ do
  (msg, cid, _, _, _) <- MaybeT $ getEssentialContent <$> query
  bquery <- pureMaybe $ MP.runParser bookParser msg
  other_data <- lift $ query @OtherData
  case bquery of
    Upload bookname pdf -> do
      return [ baSendToChatId cid $ T.pack "not supported yet o.o" ]
      --let bookinfo = BookInfo Nothing Nothing [] "uploader"
      --book <- lift . lift $ runExceptT $ makeBook bookname bookinfo pdf
      --case book of
      --  Left err -> return [ baSendToChatId cid $ T.pack $ "上传失败！\n" ++ show err ]
      --  Right book -> do
      --    let newBooks = book : books (savedData other_data)
      --    lift $ put $ other_data { savedData = (savedData other_data) {books = newBooks} }
      --    return [ baSendToChatId cid $ T.pack $ "上传成功！\n" ++ show book ]
    Delete bookname -> do
      let newBooks = filter ((/= bookname) . book_name) $ books (savedData other_data)
      lift $ change $ \other_data -> other_data { savedData = (savedData other_data) {books = newBooks} }
      return [ baSendToChatId cid "全部忘掉啦owo!" ]
    LocalMakeBook bookname pdf -> do
      let bookinfo = BookInfo Nothing Nothing [] "喵喵"
      book <- lift . lift $ runExceptT $ makeBook bookname bookinfo pdf
      case book of
        Left err -> return [ baSendToChatId cid $ "制作书书的时候出错了o.o\n" <> tshow err ]
        Right book -> do
          let newBooks = book : books (savedData other_data)
          lift $ change $ \other_data -> other_data { savedData = (savedData other_data) {books = newBooks} }
          return [ baSendToChatId cid $ "书书制作好啦owo\n" <> bookStats book ]
    LocalAddBook bookname imagesDir -> do
      let bookinfo = BookInfo Nothing Nothing [] "喵喵"
      book <- lift . lift $ runExceptT $ makeBookFromImageDir bookname bookinfo Nothing imagesDir
      case book of
        Left err -> return [ baSendToChatId cid $ "制作书书的时候遇到了麻烦o.o\n" <> tshow err ]
        Right book -> do
          let newBooks = book : books (savedData other_data)
          lift $ change $ \other_data -> other_data { savedData = (savedData other_data) {books = newBooks} }
          return [ baSendToChatId cid $ "书书制作好啦owo\n" <> bookStats book ]
  where
    bookParser :: (Chars sb) => Parser sb Char BookManagement
    bookParser = headCommand "book" >> commandSeparator >> asumE
      --[ $(stringQ "upload") >> commandSeparator >> Upload <$> word <*> (commandSeparator >> AbsPath <$> word)
      [ $(stringQ "delete") >> commandSeparator >> Delete <$> word'
      , $(stringQ "localmake") >> commandSeparator >> LocalMakeBook <$> word' <*> (commandSeparator >> RelPath <$> word)
      , $(stringQ "localadd") >> commandSeparator >> LocalAddBook <$> word' <*> (commandSeparator >> RelPath <$> word)
      ]

bookStats :: Book -> Text
bookStats book = T.intercalate "\n"
  [ book_name book
  , "作者：" <> fromMaybe "Nothing" (bookInfo_author $ book_info book)
  , "页数：" <> tshow (length $ book_pages book)
  , "标签：" <> tshow (bookInfo_tags $ book_info book)
  ]
