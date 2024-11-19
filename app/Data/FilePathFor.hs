{-# LANGUAGE GADTs, FlexibleInstances, DataKinds, LambdaCase, TypeFamilies, DerivingVia, StandaloneDeriving, MultiParamTypeClasses, ScopedTypeVariables #-}
module Data.FilePathFor
  ( ComposablePath(..)
  , toAbsoluteFilePath, toAbsoluteFilePaths
  , toAbsPath, toAbsPaths
  , getCurrentDirectory
  , listDirectoriesAndFiles, listDirectory, checkMkdir
  , FilePathFor(..), absDir, absFile, relDir, relFile
  , PathRef(..), PathType(..)
  , useAbsPath, useRelPath, useAnyPath
  , takeBaseName, addExtensionTo, addExtension
  , changeUsage
  , PDF, Image, Markdown, FileType, NoExtension -- these are type constuctors, not data constructors
  ) where

import Control.Monad.IO.Class
import qualified System.Directory as D
import qualified System.FilePath as FP
import GHC.TypeLits
import GHC.Exts
import Data.Bifunctor

-- | This type constructor is used to specify the path type
data PathRef = Rel | Abs
data PathType = Directory | File

-- | the type variable a is used to enhance type-safety. GADT is used to provide two constructors for different path types
-- using this type safe design we can avoid the following errors:
-- mixing file path with directory path
-- mixing absolute path with relative path
-- mixing paths of different purpose
data
  FilePathFor
    (pr :: PathRef)  -- ^ the path reference type, either relative or absolute
    (pt :: PathType) -- ^ the path type, either directory or file
    a                -- ^ the type of the file, used to enhance type-safety
  where
    AbsPath :: String -> FilePathFor Abs t a -- ^ the absolute path constructor
    RelPath :: String -> FilePathFor Rel t a -- ^ the relative path constructor

absFile :: String -> FilePathFor Abs File a
absFile = AbsPath

absDir :: String -> FilePathFor Abs Directory a
absDir = AbsPath

relFile :: String -> FilePathFor Rel File a
relFile = RelPath

relDir  :: String -> FilePathFor Rel Directory a
relDir  = RelPath

instance IsString (FilePathFor Rel t a) where
  fromString = RelPath
instance IsString (FilePathFor Abs t a) where
  fromString = AbsPath

deriving instance Show (FilePathFor r t a)
deriving instance Eq   (FilePathFor r t a)
deriving instance Ord  (FilePathFor r t a)

-- | Use this function to destruct and specify that an absolute path must be used
useAbsPath :: FilePathFor Abs t a -> String
useAbsPath (AbsPath s) = s
{-# INLINE useAbsPath #-}

-- | Use this function to destruct and specify that a relative path must be used
useRelPath :: FilePathFor Rel t a -> String
useRelPath (RelPath s) = s
{-# INLINE useRelPath #-}

-- | Use this function to destruct and specify that any path type can be used
useAnyPath :: FilePathFor pt t a -> String
useAnyPath (AbsPath s) = s
useAnyPath (RelPath s) = s
{-# INLINE useAnyPath #-}

data PDF    -- only exists at the type level
data Image
data Markdown
data NoExtension
data FileType (s :: Symbol)

class ComposablePath (pa :: PathRef) (pb :: PathRef) where
  (</>) :: FilePathFor pa Directory a -> FilePathFor pb tb a -> FilePathFor pa tb a

instance ComposablePath Abs Rel where
  AbsPath a </> RelPath b = AbsPath $ a FP.</> b
  {-# INLINE (</>) #-}

instance ComposablePath Rel Rel where
  RelPath a </> RelPath b = RelPath $ a FP.</> b
  {-# INLINE (</>) #-}

-- | Make directory if does not exist
checkMkdir :: (MonadIO m) => FilePathFor any Directory a -> m (FilePathFor any Directory a)
checkMkdir fp = do
  liftIO $ D.createDirectoryIfMissing True . useAnyPath $ fp
  return fp
{-# INLINE checkMkdir #-}

-- | Change the usage of a file path
changeUsage :: FilePathFor r t a -> FilePathFor r t b
changeUsage (AbsPath s) = AbsPath s
changeUsage (RelPath s) = RelPath s
{-# INLINE changeUsage #-}

-- | Get the base name of a file path
takeBaseName :: FilePathFor r File a -> FilePathFor Rel t NoExtension
takeBaseName = RelPath . FP.takeBaseName . useAnyPath
{-# INLINE takeBaseName #-}

-- | Add an extension to a file path, allows changing usage
addExtensionTo :: String -> FilePathFor r t NoExtension -> FilePathFor r File b
addExtensionTo ext (AbsPath s) = AbsPath $ FP.addExtension s ext
addExtensionTo ext (RelPath s) = RelPath $ FP.addExtension s ext
{-# INLINE addExtensionTo #-}

-- | List the directory of a file path, running in any IO-capable monad
listDirectory :: (MonadIO m) => FilePathFor r Directory a -> m [FilePathFor Rel t a]
listDirectory = fmap (map RelPath) . liftIO . D.listDirectory . useAnyPath
{-# INLINE listDirectory #-}

-- | Add an extension to a file path, allows changing the usage
addExtension = flip addExtensionTo
{-# INLINE addExtension #-}

-- | Get the current directory as an absolute path, running in any IO-capable monad
getCurrentDirectory :: (MonadIO m) => m (FilePathFor Abs Directory a)
getCurrentDirectory = AbsPath <$> liftIO D.getCurrentDirectory
{-# INLINE getCurrentDirectory #-}

-- | This function appends cuurent directory to the file path inside any IO-capable monad
toAbsoluteFilePath :: (MonadIO m) => FilePathFor Rel t a -> m (FilePathFor Abs t a)
toAbsoluteFilePath relFp = (</> relFp) <$> getCurrentDirectory

-- | This function appends the same current directory to all file paths inside any IO-capable monad
toAbsoluteFilePaths :: (MonadIO m) => [FilePathFor Rel t a] -> m [FilePathFor Abs t a]
toAbsoluteFilePaths relFps = do
  cd <- getCurrentDirectory
  return $ map (cd </>) relFps

-- | Convert any filepath to an absolute path, if it is already an absolute path, it will be returned as is
toAbsPath :: (MonadIO m) => FilePathFor r t a -> m (FilePathFor Abs t a)
toAbsPath (AbsPath s) = return $ AbsPath s
toAbsPath (RelPath s) = (</> RelPath s) <$> getCurrentDirectory
{-# INLINE toAbsPath #-}

-- | Convert a list of filepaths to absolute paths, if they are already absolute paths, they will be returned as is
toAbsPaths :: (MonadIO m) => [FilePathFor r t a] -> m [FilePathFor Abs t a]
toAbsPaths [] = return []
toAbsPaths abs@(AbsPath _ : _) = return abs
toAbsPaths rel@(RelPath _ : _) = toAbsoluteFilePaths rel
{-# INLINE toAbsPaths #-}

-- | List a directory content and separate them into directories, files.
listDirectoriesAndFiles :: FilePathFor r Directory a -> IO ([FilePathFor Rel Directory a], [FilePathFor Rel File a])
listDirectoriesAndFiles dir = do
  all <- D.listDirectory $ useAnyPath dir
  (dirs, files) <- partitionM D.doesDirectoryExist all
  return (map RelPath dirs, map RelPath files)
  where partitionM _ [] = return ([], [])
        partitionM p (x:xs) = p x >>= \case
          True -> first  (x:) <$> partitionM p xs
          False-> second (x:) <$> partitionM p xs

