{-# LANGUAGE TypeFamilies, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.ConnectionManager where

import Module.RS
import Control.Monad.Logger
import Control.Monad.Effect
import Control.System
import Module.RS.QQ
import Module.Logging
import Network.HTTP.Client (Manager, newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Connection
import Network.TLS
import Data.Default

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
