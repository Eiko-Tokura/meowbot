{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
module MeowBot.Data.CQHttp.CQCode where

import Control.DeepSeq
import GHC.Generics
import Data.Text (Text, pack)
import qualified Data.Text as T
import Utils.ByteString
import Utils.Base64
import Utils.Persist
import Data.Aeson
import Data.String

data CQCode
  = CQAt Int (Maybe Text)
  | CQReply Int
  | CQRecord Text
  | CQImage Text
  | CQImage64 Base64
  | CQOther Text [(Text, Text)]
  deriving (Show, Read, Eq, Generic, NFData)
  deriving (PersistField, PersistFieldSql) via (PersistUseShow CQCode)

-- note: we should create a newtype like EscapedText or RawText to avoid mixing unescaped and escaped text

embedCQCode :: CQCode -> Text
embedCQCode (CQAt qq Nothing)     = "[CQ:at,qq=" <> pack (show qq) <> "]"
embedCQCode (CQAt qq (Just name))     = "[CQ:at,qq=" <> pack (show qq) <> ",name=" <> name <> "]"
embedCQCode (CQReply id)  = "[CQ:reply,id=" <> pack (show id) <> "]"
embedCQCode (CQImage str) = "[CQ:image,file=file://" <> str <> "]"
embedCQCode (CQImage64 str) = "[CQ:image,file=base64://" <> bsToText (runBase64 str) <> "]"
embedCQCode (CQRecord str)= "[CQ:record,file=file://" <> str <> "]"
embedCQCode (CQOther str list)
  =  "[CQ:" <> str
  <> if null list then "" else ","
  <> T.intercalate "," [ key <> "=" <> val | (key, val) <- list]
  <> "]"

instance ToJSON CQCode where
  toJSON (CQAt qq mName) = object
    [ "type" .= ("at" :: Text)
    , "data" .= object
        ( [ "qq" .= qq
          ]
        <> maybe [] (\name -> ["name" .= name]) mName
        )
    ]
  toJSON (CQReply id) = object
    [ "type" .= ("reply" :: Text)
    , "data" .= object
        [ "id" .= id
        ]
    ]
  toJSON (CQRecord file) = object
    [ "type" .= ("record" :: Text)
    , "data" .= object
        [ "file" .= ("file://" <> file)
        ]
    ]
  toJSON (CQImage file) = object
    [ "type" .= ("image" :: Text)
    , "data" .= object
        [ "file" .= ("file://" <> file)
        ]
    ]
  toJSON (CQImage64 file) = object
    [ "type" .= ("image" :: Text)
    , "data" .= object
        [ "file" .= ("base64://" <> bsToText (runBase64 file))
        ]
    ]
  toJSON (CQOther str list) = object
    [ "type" .= str
    , "data" .= object
        [ fromString (T.unpack key) .= val | (key, val) <- list
        ]
    ]
