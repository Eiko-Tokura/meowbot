{-# LANGUAGE TypeFamilies, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.ConnectionManager where

import Control.Monad.Effect
import Control.Monad.Logger
import Control.System
import Data.Default
import Data.Function ((&))
import Module.Logging
import Module.RS.QQ
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.TLS
import Utils.ByteString
import Utils.Text

[makeRModule__|
ConnectionManagerModule
  manager       :: Manager
  customTimeout :: Int
|]

instance SystemModule ConnectionManagerModule where
  data ModuleInitData ConnectionManagerModule = ConnectionManagerModuleInitData
  data ModuleEvent    ConnectionManagerModule = ConnectionManagerModuleEvent

instance
  Dependency' c ConnectionManagerModule '[LoggingModule] mods
  => Loadable c ConnectionManagerModule mods ies where
  withModule _ act = do
    let customTimeout = 120 * 1000000 -- 120 seconds in microseconds
    let customManagerSettings =
          (mkManagerSettings
            ( case def of  -- | Turn off forcing EMS since bilibili doesn't support it, weird.
                           -- The library author says tls is insecure without EMS, but we don't have a choice.
               t@TLSSettingsSimple{} -> t { settingClientSupported = def { supportedExtendedMainSecret = AllowEMS } }
               t -> t
           )
            Nothing
          ){ managerResponseTimeout = responseTimeoutMicro customTimeout }
    manager <- liftIO $ newManager customManagerSettings
    $logInfo "Connection Manager Initialized"
    let r = ConnectionManagerModuleRead manager customTimeout
    runEffTOuter_ r ConnectionManagerModuleState act

instance
  Dependency' c ConnectionManagerModule '[LoggingModule] mods
  => EventLoop c ConnectionManagerModule mods es where

withConnectionManager ::
  ( LoggingModule `In` mods
  , ConsFDataList FData (ConnectionManagerModule : mods)
  , m ~ IO
  )
  => EffT (ConnectionManagerModule : mods) es m a -> EffT mods es m a
withConnectionManager act = do
  let customTimeout = 120 * 1000000 -- 120 seconds in microseconds
  let customManagerSettings =
        (mkManagerSettings
          ( case def of  -- | Turn off forcing EMS since bilibili doesn't support it, weird.
                         -- The library author says tls is insecure without EMS, but we don't have a choice.
             t@TLSSettingsSimple{} -> t { settingClientSupported = def { supportedExtendedMainSecret = AllowEMS } }
             t -> t
         )
          Nothing
        ){ managerResponseTimeout = responseTimeoutMicro customTimeout }
  manager <- liftIO $ newManager customManagerSettings
  $logInfo "Connection Manager Initialized"
  let r = ConnectionManagerModuleRead manager customTimeout
  runConnectionManagerModule r act

type URL = Text
download ::
  ( ConnectionManagerModule `In` mods
  , InList (ErrorText "download") es
  , MonadIO m
  )
  => URL -> EffT mods es m LazyByteString
download url = do
  man   <-  asksModule manager
  req   <-  parseRequest (unpack url)
    `effCatch` \(e :: MonadThrowError) -> effThrow $ errorText @"download" $ "Invalid URL: " <> url <> " (" <> toText e <> ")"
  resp  <-  httpLbs req man
    & liftIOSafeWith (errorText @"download" . toText @HttpException)
    & embedError
  case resp.responseStatus of
    s | s >= status200 && s < status300 -> return resp.responseBody
      | otherwise -> effThrow $ errorText @"download" $ "HTTP error code " <> toText (statusCode s) <> " when downloading " <> url
