{-# LANGUAGE DataKinds #-}
module External.MarkdownImage 
  ( markdownToImage
  , pdfToImage
  , pdfToImageWithPageNumber
  , markdownToPdf
  ) where

import System.Process (callProcess)
import Data.Time
import Data.Maybe
import Data.FilePathFor
import Data.List (sort)
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import MonParserF

-- | write markdown text to a temp file in md/{unix-time}.md based on the current unix time, return the file path.
writeMarkdownToTempFile :: String -> ExceptT SomeException IO (FilePathFor Rel Markdown)
writeMarkdownToTempFile markdownString = do
    unixTime <- lift getCurrentTime
    let timeString = formatTime defaultTimeLocale "%s" unixTime
    ExceptT $ try $ writeFile (tempInputFile timeString) markdownString
    return $ RelPath (tempInputFile timeString)
    where tempInputFile fn = "md/" ++ fn ++ ".md"

-- | This function uses pandoc to convert markdown file to pdf file in the ./output directory, uses the same base name as the input file.
markdownToPdf :: FilePathFor anyPathType Markdown -> ExceptT SomeException IO (FilePathFor Rel PDF)
markdownToPdf md = do
  ExceptT $ try $ callProcess "pandoc" 
    ["--pdf-engine=xelatex"
    , "-V", "CJKmainfont=Noto Sans CJK JP"
    , "-V", "geometry:margin=0.5in"
    , "-V", "fontsize=12pt"
    , useAnyPath md
    , "-o", useAnyPath $ pdfOutputFileOf md
    ]
  return $ pdfOutputFileOf md
  where pdfOutputFileOf md = RelPath "output" </> addExtension (takeBaseName md) ".pdf"

-- | A fully type safe function, input a file path for PDF, output a list of image filepaths generated.
-- the pdf2png.py is required and you need to install pdf2image python package
pdfToImageWithPageNumber :: FilePathFor anyPathType PDF -> ExceptT SomeException IO [(Int, FilePathFor Rel Image)]
pdfToImageWithPageNumber pdf = do
  ExceptT $ try $ callProcess "python" ["pdf2png.py", useAnyPath pdf, useAnyPath $ pdfImageDirectory pdf]
  ExceptT $ try $ do
    imagepaths <- listDirectory (pdfImageDirectory pdf)
    return [ (readPageNumber . useRelPath . takeBaseName $ imagepath, pdfImageDirectory pdf </> imagepath) | imagepath <- imagepaths ]
  where pdfImageDirectory :: FilePathFor anyPathType PDF -> FilePathFor Rel Image
        pdfImageDirectory pdf = RelPath "images" </> changeUsage (takeBaseName pdf)
        readPageNumber = fromMaybe (error "page number un-readable") . mRunParserF (string "page_" *> positiveInt <* string ".png")

pdfToImage :: FilePathFor anyPathType PDF -> ExceptT SomeException IO [FilePathFor Rel Image] 
pdfToImage = fmap (fmap snd) . pdfToImageWithPageNumber

-- | Input a markdown string, output a sorted list of absolute file paths of images generated
markdownToImage :: String -> ExceptT SomeException IO [FilePathFor Abs Image]
markdownToImage = writeMarkdownToTempFile >=> markdownToPdf >=> pdfToImage >=> toAbsoluteFilePaths >=> (return . sort)

