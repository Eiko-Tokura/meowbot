{-# LANGUAGE OverloadedStrings, UndecidableInstances #-}
module Utils.ToText where

import Data.Text (Text)
import qualified Data.Text as T

-- | A class for converting values to 'Text'.
-- it avoids repetedly using 'T.pack . show' in the code, and automatically avoids using show to a Text value
class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance ToText Char where
  toText = T.singleton

instance ToText String where
  toText = T.pack

instance ToText a => ToText (Maybe a) where
  toText Nothing  = "Nothing"
  toText (Just a) = "Just (" <> toText a <> ")"

instance {-# OVERLAPPABLE #-} Show a => ToText a where
  toText = T.pack . show
