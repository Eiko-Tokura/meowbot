module Module.MeowConnection where

import Control.Exception
import Control.Monad.Effect
import Control.System
import Module.RS.QQ
import Network.WebSockets (Connection)
import Utils.Text
import Utils.ByteString
import qualified Network.WebSockets as W

[makeRModule|
MeowConnection
  meowConnection :: !Connection
|]

type MeowSend mods es =
  ( In' FData MeowConnection mods
  , InList (ErrorText "send_connection") es
  )

sendTextData :: (In' c MeowConnection mods, InList (ErrorText "send_connection") es, MonadIO m) => LazyByteString -> EffT' c mods es m ()
sendTextData txt = do
  conn <- asksModule meowConnection
  embedError $ liftIOSafeWith (errorText @"send_connection" . toText @SomeException) $ W.sendTextData conn txt
{-# INLINE sendTextData #-}

recvLBSData ::
  ( In' c MeowConnection mods
  , InList (ErrorText "recv_connection") es
  , MonadIO m
  ) => EffT' c mods es m LazyByteString
recvLBSData = do
  conn <- asksModule meowConnection
  embedError $ liftIOSafeWith (errorText @"recv_connection" . toText @SomeException) $ W.receiveData conn
{-# INLINE recvLBSData #-}

instance EventLoop c MeowConnection mods es
