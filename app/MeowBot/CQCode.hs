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

data CQCode 
  = CQAt Int 
  | CQReply Int 
  | CQRecord Text
  | CQImage Text
  | CQOther Text [(Text, Text)]
  deriving (Show, Read, Eq, Generic, NFData)

embedCQCode :: CQCode -> Text
embedCQCode (CQAt qq)     = "[CQ:at,qq=" <> pack (show qq) <> "]"
embedCQCode (CQReply id)  = "[CQ:reply,id=" <> pack (show id) <> "]"
embedCQCode (CQImage str) = "[CQ:image,file=file://" <> str <> "]"
embedCQCode (CQRecord str)= "[CQ:record,file=file://" <> str <> "]"
embedCQCode (CQOther str list) = "[CQ:" <> str <> T.intercalate "," [ key <> "=" <> val | (key, val) <- list] <> "]"
