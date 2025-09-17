-- | A model independent, abstract representation of a message.
--
-- in this way, we can transform various different message formats
-- like we can have MeowMsg 'CQHttp, MeowMsg 'ChatAPI, MeowMsg 'MeowInternal, MeowMsg 'Discord, etc.
module MeowBot.MeowMsg where

import Data.Kind (Type)
import Data.Void
import Utils.Lens
import Utils.Text
import qualified Data.ByteString.Lazy as BL

class Platform (w :: k) where
  -- identity types:
  type MType   w :: Type -- ^ message type, like group message, private message, etc. Cannot be Void

  type MUserId w :: Type  -- ^ user id type
  type MGroupId w :: Type -- ^ group id type

  type MUserInfo w :: Type -- ^ user info type, like nickname ..

  type MMsgId  w :: Type -- ^ message id type

  -- inline content types:
  type MMsgInline w :: Type -- ^ inline message content type, like text, image, mention, face/emoticon, etc.
  type MMsgInline w = MsgInline w

  type MText   w :: Type -- ^ text content type
  type MText   w = Text

  type MImage  w :: Type -- ^ image content type
  type MImage  w = UrlOrBytes

  type MAt     w :: Type -- ^ @mention type
  type MAt     w = MUserId w

  type MFace   w :: Type -- ^ face/emoticon type, not emojis as they are part of text
  type MFace   w = Void

  type MOther  w :: Type -- ^ kept open for extension
  type MOther  w = Void

  -- attachment types:
  type MAttach w :: Type -- ^ attachment, like files, videos, images, etc.
  type MAttach w = Void

  type MMeta   w :: Type -- ^ meta information type, like timestamp, sender info, raw message, etc. Cannot be Void
  type MMeta   w = ()

  type MReply  w :: Type -- ^ reply type, typically = Maybe (MMsgId w)
  type MReply  w = Maybe (MMsgId w)

newtype MessageId = MessageId { unMessageId :: Int }
data UrlOrBytes = Url Text | Bytes BL.ByteString
  deriving (Show, Eq)

data MsgInline (w :: k)
  = MsgText  (MText   w)
  | MsgImage (MImage  w)
  | MsgAt    !(MAt    w)
  | MsgFace  !(MFace  w)
  | MsgOther !(MOther w)

data MeowMsg (w :: k) = MeowMsg
  { msgInlines :: ![MMsgInline w]
  , msgAttach  :: ![MAttach w]
  , msgReply   :: !(MReply w)
  , msgChat    :: !(MType w)
  , msgUserId  :: !(MUserId w)
  , msgUserInfo:: !(MUserInfo w)
  , msgMeta    :: MMeta w
  }

makeLenses_ ''MeowMsg
