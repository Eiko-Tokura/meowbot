{-# LANGUAGE DataKinds, OverloadedStrings #-}
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
writeMarkdownToTempFile :: String -> ExceptT SomeException IO (FilePathFor Rel File Markdown)
writeMarkdownToTempFile markdownString = do
    unixTime <- lift getCurrentTime
    let timeString = formatTime defaultTimeLocale "%s" unixTime
    ExceptT $ try $ writeFile (useAnyPath $ tempInputFile timeString) markdownString
    return $ tempInputFile timeString
    where tempInputFile fn = mdPath </> relFile (fn ++ ".md")
          mdPath = "md"

-- | This function uses pandoc to convert markdown file to pdf file in the ./output directory, uses the same base name as the input file.
markdownToPdf :: FilePathFor anyPathType File Markdown -> ExceptT SomeException IO (FilePathFor Rel File PDF)
markdownToPdf md = do
  checkMkdir outputDir
  ExceptT $ try $ callProcess "pandoc" 
    ["--pdf-engine=xelatex"
    , "-V", "CJKmainfont=Noto Sans CJK JP"
    , "-V", "geometry:margin=0.5in"
    , "-V", "fontsize=12pt"
    , useAnyPath md
    , "-o", useAnyPath $ pdfOutputFileOf md
    ]
  return $ pdfOutputFileOf md
  where pdfOutputFileOf md = outputDir </> addExtension (takeBaseName md) ".pdf"
        outputDir = "output"

-- | A fully type safe function, input a file path for PDF, output a list of image filepaths generated.
-- the pdf2png.py is required and you need to install pdf2image python package
pdfToImageWithPageNumber :: FilePathFor anyPathType File PDF -> ExceptT SomeException IO [(Int, FilePathFor Rel File Image)]
pdfToImageWithPageNumber pdf = do
  checkMkdir imageDir
  ExceptT $ try $ callProcess "python" ["pdf2png.py", useAnyPath pdf, useAnyPath $ pdfImageDirectory pdf]
  ExceptT $ try $ do
    imagepaths <- listDirectory (pdfImageDirectory pdf)
    return [ (readPageNumber . useRelPath . takeBaseName $ imagepath, pdfImageDirectory pdf </> imagepath) | imagepath <- imagepaths ]
  where pdfImageDirectory :: FilePathFor anyPathType File PDF -> FilePathFor Rel Directory Image
        pdfImageDirectory pdf = imageDir </> changeUsage (takeBaseName pdf)
        readPageNumber = fromMaybe (error "page number un-readable") . mRunParserF (string "page_" *> positiveInt <* string ".png")
        imageDir = "images"

pdfToImage :: FilePathFor anyRef File PDF -> ExceptT SomeException IO [FilePathFor Rel File Image] 
pdfToImage = fmap (fmap snd) . pdfToImageWithPageNumber

-- | Input a markdown string, output a sorted list of absolute file paths of images generated
markdownToImage :: String -> ExceptT SomeException IO [FilePathFor Abs File Image]
markdownToImage = writeMarkdownToTempFile >=> markdownToPdf >=> pdfToImage >=> toAbsoluteFilePaths >=> (return . sort)
