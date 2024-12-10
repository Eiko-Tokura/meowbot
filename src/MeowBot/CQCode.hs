{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
module MeowBot.CQCode where

import Control.DeepSeq
import GHC.Generics
import Data.Text (Text, pack)
import qualified Data.Text as T
import Utils.Persist

data CQCode
  = CQAt Int
  | CQReply Int
  | CQRecord Text
  | CQImage Text
  | CQOther Text [(Text, Text)]
  deriving (Show, Read, Eq, Generic, NFData)

instance PersistUseShow CQCode

-- note: we should create a newtype like EscapedText or RawText to avoid mixing unescaped and escaped text

embedCQCode :: CQCode -> Text
embedCQCode (CQAt qq)     = "[CQ:at,qq=" <> pack (show qq) <> "]"
embedCQCode (CQReply id)  = "[CQ:reply,id=" <> pack (show id) <> "]"
embedCQCode (CQImage str) = "[CQ:image,file=file://" <> str <> "]"
embedCQCode (CQRecord str)= "[CQ:record,file=file://" <> str <> "]"
embedCQCode (CQOther str list) = "[CQ:" <> str <> T.intercalate "," [ key <> "=" <> val | (key, val) <- list] <> "]"
