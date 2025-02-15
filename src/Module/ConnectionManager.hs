{-# LANGUAGE TypeFamilies, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.ConnectionManager where

import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import Control.Applicative
import Module
import MeowBot.BotStructure
import Network.HTTP.Client (Manager, newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Connection
import Network.TLS
import Data.Default

data ConnectionManagerModule

instance MeowModule r AllData ConnectionManagerModule where
  data ModuleLocalState ConnectionManagerModule  = ConnectionManagerModuleL
  data ModuleEarlyLocalState ConnectionManagerModule = ConnectionManagerEarlyLocalState
  data ModuleGlobalState ConnectionManagerModule = ConnectionManagerModuleG { manager :: Manager, customTimeout :: Int }
  data ModuleEvent ConnectionManagerModule       = ConnectionManagerEvent
  data ModuleInitDataG ConnectionManagerModule   = ConnectionManagerInitDataG
  data ModuleInitDataL ConnectionManagerModule   = ConnectionManagerInitDataL

  getInitDataG _ = (Just ConnectionManagerInitDataG, empty)

  getInitDataL _ = (Just ConnectionManagerInitDataL, empty)

  initModule _ _ = do
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
    $(logInfo) "Connection Manager Initialized"
    return $ ConnectionManagerModuleG manager customTimeout

  initModuleLocal _ _ _ _ _ = return ConnectionManagerModuleL

  initModuleEarlyLocal _ _ _ = return ConnectionManagerEarlyLocalState
