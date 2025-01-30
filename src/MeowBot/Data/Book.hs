{-# LANGUAGE DerivingVia #-}
module MeowBot.Data.Book where
import Data.Coerce
import Data.Text (Text)
import Utils.Persist

newtype PageNumber = PageNumber Int deriving (Show, Eq, Ord, Read) via Int
newtype AbsolutePageNumber = AbsolutePageNumber Int deriving (Show, Eq, Ord, Read) via Int
newtype PageNumberOffset = PageNumberOffset Int deriving (Show, Eq, Ord, Read, Num) via Int

offsetPageNumber :: PageNumberOffset -> PageNumber -> AbsolutePageNumber
offsetPageNumber offset pn = AbsolutePageNumber (coerce pn + coerce offset)

reverseOffsetPageNumber :: PageNumberOffset -> AbsolutePageNumber -> PageNumber
reverseOffsetPageNumber offset apn = PageNumber (coerce apn - coerce offset)

type BookName = Text
-- we use a new naming convention that, a data field start with the data itself, using _ to indicate that it is a data field function, differentialting from ordinary functions.

data Book = Book
  { book_name    :: BookName
  , book_pdfPath :: Maybe FilePath
  , book_pages   :: [BookPage]
  , book_info    :: BookInfo
  } deriving (Show, Eq, Ord, Read)

data BookPage = BookPage
  { page_imagePath      :: Text
  , page_absoluteNumber :: AbsolutePageNumber
  , page_type           :: Maybe PageType
  } deriving (Show, Eq, Ord, Read)
    deriving (PersistField, PersistFieldSql) via PersistUseShow BookPage

data PageType = Cover | Foreword | Menu | Chapter | Exercise | MarkedAs String deriving (Show, Eq, Ord, Read)

newtype BookTag = BookTag { useBookTag :: Text } deriving (Show, Eq, Ord, Read) via Text

data BookInfo = BookInfo
  { bookInfo_author           :: Maybe Text
  , bookInfo_pageNumberOffset :: Maybe PageNumberOffset
  , bookInfo_tags             :: [BookTag]
  , bookInfo_uploader         :: Text
  } deriving (Show, Eq, Ord, Read)
    deriving (PersistField, PersistFieldSql) via PersistUseShow BookInfo
