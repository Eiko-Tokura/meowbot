{-# LANGUAGE GADTs, DataKinds, KindSignatures, DerivingVia, StandaloneDeriving, MultiParamTypeClasses, ScopedTypeVariables #-}
module Data.FilePathFor 
  ( ComposablePath(..)
  , toAbsoluteFilePath, toAbsoluteFilePaths
  , toAbsPath, toAbsPaths
  , getCurrentDirectory
  , listDirectory
  , FilePathFor(..)
  , PathType(..)
  , useAbsPath, useRelPath, useAnyPath
  , takeBaseName, addExtensionTo, addExtension
  , changeUsage
  , PDF, Image, Markdown, FileType -- these are type constuctors, not data constructors
  ) where

import Control.Monad.IO.Class
import qualified System.Directory as D
import qualified System.FilePath as FP
import GHC.TypeLits

-- | This type constructor is used to specify the path type
data PathType = Rel | Abs

-- | the type variable a is used to enhance type-safety. GADT is used to provide two constructors for different path types
data 
  FilePathFor 
    (pt :: PathType) -- ^ the path type, either relative or absolute
    a                -- ^ the type of the file, used to enhance type-safety
  where
    AbsPath :: String -> FilePathFor Abs a -- ^ the absolute path constructor
    RelPath :: String -> FilePathFor Rel a -- ^ the relative path constructor

deriving instance Show (FilePathFor pt a)
deriving instance Eq   (FilePathFor pt a)
deriving instance Ord  (FilePathFor pt a)

-- | Use this function to destruct and specify that an absolute path must be used
useAbsPath :: FilePathFor Abs a -> String
useAbsPath (AbsPath s) = s
{-# INLINE useAbsPath #-}

-- | Use this function to destruct and specify that a relative path must be used
useRelPath :: FilePathFor Rel a -> String
useRelPath (RelPath s) = s
{-# INLINE useRelPath #-}

-- | Use this function to destruct and specify that any path type can be used
useAnyPath :: FilePathFor pt a -> String
useAnyPath (AbsPath s) = s
useAnyPath (RelPath s) = s
{-# INLINE useAnyPath #-}

data PDF    -- only exists at the type level
data Image
data Markdown
data FileType (s :: Symbol)

class ComposablePath (pa :: PathType) (pb :: PathType) where
  (</>) :: FilePathFor pa a -> FilePathFor pb a -> FilePathFor pa a

instance ComposablePath Abs Rel where
  AbsPath a </> RelPath b = AbsPath $ a FP.</> b
  {-# INLINE (</>) #-}

instance ComposablePath Rel Rel where
  RelPath a </> RelPath b = RelPath $ a FP.</> b
  {-# INLINE (</>) #-}

-- | Change the usage of a file path
changeUsage :: forall b a pt. FilePathFor pt a -> FilePathFor pt b
changeUsage (AbsPath s) = AbsPath s
changeUsage (RelPath s) = RelPath s
{-# INLINE changeUsage #-}

-- | Get the base name of a file path
takeBaseName :: FilePathFor pt a -> FilePathFor Rel a
takeBaseName = RelPath . FP.takeBaseName . useAnyPath
{-# INLINE takeBaseName #-}

-- | Add an extension to a file path, allows changing usage
addExtensionTo :: String -> FilePathFor pt a -> FilePathFor pt b
addExtensionTo ext (AbsPath s) = AbsPath $ FP.addExtension s ext
addExtensionTo ext (RelPath s) = RelPath $ FP.addExtension s ext
{-# INLINE addExtensionTo #-}

-- | List the directory of a file path, running in any IO-capable monad
listDirectory :: (MonadIO m) => FilePathFor any a -> m [FilePathFor Rel a]
listDirectory = fmap (map RelPath) . liftIO . D.listDirectory . useAnyPath
{-# INLINE listDirectory #-}

-- | Add an extension to a file path, allows changing the usage
addExtension = flip addExtensionTo
{-# INLINE addExtension #-}

-- | Get the current directory as an absolute path, running in any IO-capable monad
getCurrentDirectory :: (MonadIO m) => m (FilePathFor Abs a)
getCurrentDirectory = AbsPath <$> liftIO D.getCurrentDirectory
{-# INLINE getCurrentDirectory #-}

-- | This function appends cuurent directory to the file path inside any IO-capable monad
toAbsoluteFilePath :: (MonadIO m) => FilePathFor Rel a -> m (FilePathFor Abs a)
toAbsoluteFilePath relFp = (</> relFp) <$> getCurrentDirectory

-- | This function appends the same current directory to all file paths inside any IO-capable monad
toAbsoluteFilePaths :: (MonadIO m) => [FilePathFor Rel a] -> m [FilePathFor Abs a]
toAbsoluteFilePaths relFps = do
  cd <- getCurrentDirectory
  return $ map (cd </>) relFps

-- | Convert any filepath to an absolute path, if it is already an absolute path, it will be returned as is
toAbsPath :: (MonadIO m) => FilePathFor any a -> m (FilePathFor Abs a)
toAbsPath (AbsPath s) = return $ AbsPath s 
toAbsPath (RelPath s) = (</> RelPath s) <$> getCurrentDirectory
{-# INLINE toAbsPath #-}

-- | Convert a list of filepaths to absolute paths, if they are already absolute paths, they will be returned as is
toAbsPaths :: (MonadIO m) => [FilePathFor any a] -> m [FilePathFor Abs a]
toAbsPaths [] = return []
toAbsPaths abs@(AbsPath _ : _) = return abs
toAbsPaths rel@(RelPath _ : _) = toAbsoluteFilePaths rel
{-# INLINE toAbsPaths #-}

