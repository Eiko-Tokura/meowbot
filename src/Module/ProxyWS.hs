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
import External.ProxyWS
import Network.WebSockets
import qualified Data.ByteString.Lazy as BL
import Utils.ByteString
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad
import Parser.Run
import Parser.Except
import Data.Maybe
import Data.Coerce
import Data.Aeson
import Control.Concurrent.STM

[makeRSModule|
ProxyWS
  State proxyDatas     :: [ProxyData]
  State pendingProxies :: [ProxyData]
|]

instance SystemModule ProxyWS where
  data ModuleInitData ProxyWS = ProxyWSInitData { addressesAndIps :: [(String, Int)] }
  data ModuleEvent    ProxyWS = ProxyWSEvent    { proxyEvent :: BL.ByteString }

instance Dependency' c ProxyWS '[SModule WholeChat, SModule BotConfig, SModule OtherData, RecvSentCQ, MeowConnection, LoggingModule] mods
  => Loadable c ProxyWS mods where
  initModule (ProxyWSInitData {addressesAndIps}) = do
    proxyDatas <- liftIO $ mapM (uncurry createProxyData) addressesAndIps
    return ( ProxyWSRead
           , ProxyWSState proxyDatas proxyDatas
           )

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
-- data ProxyWS
-- instance
--   ( HasSystemRead (TVar (Maybe ReceCQMessage)) r
--   , HasSystemRead (TVar (Maybe BL.ByteString)) r
--   , HasSystemRead Connection r
--   ) => MeowModule r AllData ProxyWS where
-- 
--   data ModuleGlobalState ProxyWS = ProxyWSGS
--   data ModuleInitDataG ProxyWS = ProxyWSInitDataG
--   data ModuleInitDataL ProxyWS = ProxyWSInitDataL { addressesAndIps :: [(String, Int)] }
--   data ModuleEarlyLocalState ProxyWS = ProxyWSELS [ProxyData]
--   data ModuleLocalState ProxyWS = ProxyWSLS { localProxyDatas :: [ProxyData], pendingProxies :: [ProxyData] }
--   data ModuleEvent ProxyWS = ProxyWSEvent { proxyEvent :: BL.ByteString }
-- 
--   getInitDataG _ = (Just ProxyWSInitDataG, empty)
-- 
--   getInitDataL _ = (Nothing, liftR1 just "--proxy" >> withE "--proxy needs a list of addresses and ports" (ProxyWSInitDataL <$> many ((,) <$> nonFlagString <*> (read <$> nonFlagString))))
-- 
--   initModule _ _ = return ProxyWSGS
-- 
--   initModuleEarlyLocal _ _ ProxyWSInitDataL { addressesAndIps } = do
--     proxyDatas <- liftIO $ mapM (uncurry createProxyData) addressesAndIps
--     return $ ProxyWSELS proxyDatas
-- 
--   initModuleLocal _ _ _ _ (ProxyWSELS proxyDatas) =
--     return $ ProxyWSLS proxyDatas proxyDatas
-- 
--   moduleEvent _ = do
--     ProxyWSLS proxyDatas _ <- readModuleStateL (Proxy @ProxyWS)
--     return $ ProxyWSEvent <$> asum [ receiveFromProxy pd | pd <- proxyDatas ]
-- 
--   moduleEventHandler _ (ProxyWSEvent bs) = do
--     conn <- askSystem @Connection
--     name <- queries (nameOfBot . botModules . botConfig)
--     $(logInfo) $ pack $ fromMaybe "喵喵" (maybeBotName name) <> " <- Proxy : " <> take 512 (bsToString bs)
--     liftIO $ sendTextData conn bs
-- 
--   afterMeow _ = do
--     mcqmsg <- askSystem @(TVar (Maybe ReceCQMessage)) >>= liftIO . atomically . fmap coerce . readTVar
--     mbs    <- askSystem @(TVar (Maybe BL.ByteString)) >>= liftIO . atomically . readTVar
--     name  <- queries (nameOfBot . botModules . botConfig)
--     ProxyWSLS proxyDatas _ <- readModuleStateL (Proxy @ProxyWS)
--     case (mcqmsg, mbs) of
--       (Just cqmsg, Just bs) ->
--         if eventType cqmsg `elem` [LifeCycle] ||
--            eventType cqmsg `elem` [PrivateMessage, GroupMessage] && filterMsg cqmsg
--           then do
--             let cqmsgTyped = decode bs :: Maybe CQMessageObject
--                 convertedToArrayStyle = fmap stringToArray cqmsgTyped
--                 mbs' = encode <$> convertedToArrayStyle
--             when (eventType cqmsg `elem` [PrivateMessage, GroupMessage]) $ do
--               $(logInfo) $ pack $ fromMaybe "喵喵" (maybeBotName name) <> " -> Proxy : " <> take 512 (bsToString bs)
--               case mbs' of
--                 Nothing -> $(logWarn) $ "Failed to convert CQMessageObject to array style: " <> pack (show cqmsgTyped)
--                 Just bs' -> do
--                   liftIO $ mapM_ (`sendToProxy` bs') proxyDatas
--             makeHeader >>= \case
--               Nothing -> return ()
--               Just headers -> do
--                 pending <- pendingProxies <$> readModuleStateL (Proxy @ProxyWS)
--                 lift . mapM_ (\pd -> runProxyWS pd headers) $ pending
--                 unless (null pending) $ modifyModuleState (Proxy @ProxyWS) $ \s -> s { pendingProxies = [] }
--           else return ()
--       _ -> return ()
--     where filterMsg cqmsg = isJust $ runParser ($(itemInQ ['!', '！', '/']) >> getItem) (fromMaybe "" $ message cqmsg)
