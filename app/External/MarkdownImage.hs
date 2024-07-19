module External.MarkdownImage 
  (
    markdownToImage
  ) where

import System.Process (system)
import System.Directory 
import System.FilePath
import Data.Time
import Data.List (sort)
import Control.Exception (try, IOException)
import Control.Monad.Trans
import Control.Monad.Trans.Except

tempInputFile fn = "md/" ++ fn ++ ".md"

pandocPart = "pandoc --pdf-engine=xelatex -V CJKmainfont=\"Noto Sans CJK JP\" -V geometry:margin=0.5in -V fontsize=12pt "
mdToPdfCommand fn = pandocPart ++ tempInputFile fn ++ " -o \"pdf/output" ++ fn ++ ".pdf\""

pdfToImageCommand fn = "python pdf2png.py \"pdf/output" ++ fn ++ ".pdf\" images/" ++ fn

markdownToImage :: String -> IO (Either IOException [FilePath])
markdownToImage markdownString = runExceptT $ do
    -- get current time as folder name
    unixTime <- lift getCurrentTime
    let timeString = formatTime defaultTimeLocale "%s" unixTime

    -- write markdown to file
    lift $ writeFile (tempInputFile timeString) markdownString

    -- convert markdown to pdf
    ExceptT $ try $ system (mdToPdfCommand timeString)

    -- convert pdf to image
    ExceptT $ try $ system $ pdfToImageCommand timeString

    cd <- lift getCurrentDirectory

    -- get list of images
    ExceptT $ try $ fmap (fmap ((cd </> "images" </> timeString) </>) . sort) $ listDirectory $ "images/" ++ timeString

