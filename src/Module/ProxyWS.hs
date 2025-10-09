{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Module.ProxyWS where

import Control.Monad.Effect
import Control.System
import Module.Logging
import Module.RS.QQ
import Module.RS
import Module.MeowConnection
import Module.RecvSentCQ

import MeowBot.BotStructure
import MeowBot.Update
import MeowBot.CQMessage.Convert
import External.ProxyWS as Proxy
import qualified Data.ByteString.Lazy as BL
import Utils.ByteString
import Control.Monad.Logger
import Control.Monad
import Parser.Run
import Data.Maybe
import Data.Coerce
import Data.Aeson
import Control.Concurrent.STM

[makeRSModule|
ProxyWS
  State proxyDatas     :: ![ProxyData]
  State pendingProxies :: ![ProxyData]
|]

instance SystemModule ProxyWS where
  data ModuleInitData ProxyWS = ProxyWSInitData { addressesAndIps :: [(String, Int)] }
  data ModuleEvent    ProxyWS = ProxyWSEvent    { proxyEvent :: BL.ByteString }

instance Dependency' c ProxyWS '[SModule WholeChat, SModule BotConfig, SModule OtherData, RecvSentCQ, MeowConnection, LoggingModule] mods
  => Loadable c ProxyWS mods ies where
  withModule (ProxyWSInitData {addressesAndIps}) act = do
    proxyDatas <- liftIO $ mapM (uncurry createProxyData) addressesAndIps
    runEffTOuter_ ProxyWSRead (ProxyWSState proxyDatas proxyDatas) act

instance
  ( c ~ FData
  , Dependency' c ProxyWS '[SModule WholeChat, SModule BotConfig, SModule OtherData, RecvSentCQ, MeowConnection, LoggingModule] mods
  , InList (ErrorText "send_connection") es
  )
  => EventLoop c ProxyWS mods es where

  moduleEvent = do
    proxyDatas' <- getsModule proxyDatas
    return $ ProxyWSEvent <$> asum [ receiveFromProxy pd | pd <- proxyDatas' ]

  handleEvent (ProxyWSEvent bs) = do
    name <- getsS (nameOfBot . botModules)
    $(logInfo) $ pack $ fromMaybe "喵喵" (maybeBotName name) <> " <- Proxy : " <> take 512 (bsToString bs)
    sendTextData bs

  afterEvent = do
    mcqmsg <- asksModule meowRecvCQ >>= liftIO . atomically . fmap coerce . readTVar
    mbs    <- asksModule meowRawByteString >>= liftIO . readTVarIO
    name  <- getsS (nameOfBot . botModules)
    proxyDatas' <- getsModule proxyDatas
    case (mcqmsg, mbs) of
      (Just cqmsg, Just bs) ->
        when (eventType cqmsg == LifeCycle ||
              eventType cqmsg `elem` [PrivateMessage, GroupMessage] && filterMsg cqmsg) $ do
            let cqmsgTyped = decode bs :: Maybe CQMessageObject
                convertedToArrayStyle = fmap stringToArray cqmsgTyped
                mbs' = encode <$> convertedToArrayStyle
            when (eventType cqmsg `elem` [PrivateMessage, GroupMessage]) $ do
              $(logInfo) $ pack $ fromMaybe "喵喵" (maybeBotName name) <> " -> Proxy : " <> take 512 (bsToString bs)
              case mbs' of
                Nothing -> $(logWarn) $ "Failed to convert CQMessageObject to array style: " <> pack (show cqmsgTyped)
                Just bs' -> do
                  liftIO $ mapM_ (`sendToProxy` bs') proxyDatas'
            makeHeader >>= \case
              Nothing -> return ()
              Just headers -> do
                pending <- getsModule pendingProxies
                mapM_ (\pd -> embedEffT $ Proxy.runProxyWS pd headers) pending
                unless (null pending) $ modifyModule $ \s -> s { pendingProxies = [] }
      _ -> return ()
    where filterMsg cqmsg = isJust $ runParser ($(itemInQ ['!', '！', '/']) >> getItem) (fromMaybe "" $ message cqmsg)

withProxyWS ::
  ( SModule WholeChat `In` mods
  , SModule BotConfig `In` mods
  , SModule OtherData `In` mods
  , RecvSentCQ `In` mods
  , MeowConnection `In` mods
  , LoggingModule `In` mods
  , ConsFDataList FData (ProxyWS : mods)
  , MonadIO m
  )
  => ModuleInitData ProxyWS -> EffT (ProxyWS : mods) es m a -> EffT mods es m a
withProxyWS (ProxyWSInitData {addressesAndIps}) act = do
  proxyDatas <- liftIO $ mapM (uncurry createProxyData) addressesAndIps
  runProxyWS_ ProxyWSRead (ProxyWSState proxyDatas proxyDatas) act
