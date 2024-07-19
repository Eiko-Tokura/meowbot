module MeowBot.CQCode where

data CQCode 
  = CQAt Int 
  | CQReply Int 
  | CQRecord String
  | CQImage String
  | CQOther String 
  deriving (Show, Read)

embedCQCode :: CQCode -> String
embedCQCode (CQAt qq)     = "[CQ:at,qq=" ++ show qq ++ "]"
embedCQCode (CQReply id)  = "[CQ:reply,id=" ++ show id ++ "]"
embedCQCode (CQImage str) = "[CQ:image,file=file://" ++ str ++ "]"
embedCQCode (CQRecord str)= "[CQ:record,file=file://" ++ str ++ "]"
embedCQCode (CQOther str) = "[CQ:" ++ str ++ "]"
