{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Module.ProxyWS where

import Module
import MeowBot.BotStructure
import MeowBot.Update
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
import Control.Concurrent.STM

data ProxyWS

instance
  ( HasSystemRead (TVar (Maybe ReceCQMessage)) r
  , HasSystemRead (TVar (Maybe BL.ByteString)) r
  , HasSystemRead Connection r
  ) => MeowModule r AllData ProxyWS where

  data ModuleGlobalState ProxyWS = ProxyWSGS
  data ModuleInitDataG ProxyWS = ProxyWSInitDataG
  data ModuleInitDataL ProxyWS = ProxyWSInitDataL { addressesAndIps :: [(String, Int)] }
  data ModuleEarlyLocalState ProxyWS = ProxyWSELS [ProxyData]
  data ModuleLocalState ProxyWS = ProxyWSLS { localProxyDatas :: [ProxyData], pendingProxies :: [ProxyData] }
  data ModuleEvent ProxyWS = ProxyWSEvent { proxyEvent :: BL.ByteString }

  getInitDataG _ = (Just ProxyWSInitDataG, empty)

  getInitDataL _ = (Nothing, liftR1 just "--proxy" >> withE "--proxy needs a list of addresses and ports" (ProxyWSInitDataL <$> many ((,) <$> nonFlagString <*> (read <$> nonFlagString))))

  initModule _ _ = return ProxyWSGS

  initModuleEarlyLocal _ _ ProxyWSInitDataL { addressesAndIps } = do
    proxyDatas <- liftIO $ mapM (uncurry createProxyData) addressesAndIps
    return $ ProxyWSELS proxyDatas

  initModuleLocal _ _ _ _ (ProxyWSELS proxyDatas) =
    return $ ProxyWSLS proxyDatas proxyDatas

  moduleEvent _ = do
    ProxyWSLS proxyDatas _ <- readModuleStateL (Proxy @ProxyWS)
    return $ ProxyWSEvent <$> asum [ receiveFromProxy pd | pd <- proxyDatas ]

  moduleEventHandler _ (ProxyWSEvent bs) = do
    conn <- askSystem @Connection
    name <- queries (nameOfBot . botModules . botConfig)
    $(logInfo) $ pack $ fromMaybe "喵喵" (maybeBotName name) <> " <- Proxy : " <> take 512 (bsToString bs)
    liftIO $ sendTextData conn bs

  afterMeow _ = do
    mcqmsg <- askSystem @(TVar (Maybe ReceCQMessage)) >>= liftIO . atomically . fmap coerce . readTVar
    mbs    <- askSystem @(TVar (Maybe BL.ByteString)) >>= liftIO . atomically . readTVar
    name  <- queries (nameOfBot . botModules . botConfig)
    ProxyWSLS proxyDatas _ <- readModuleStateL (Proxy @ProxyWS)
    case (mcqmsg, mbs) of
      (Just cqmsg, Just bs) ->
        if eventType cqmsg `elem` [LifeCycle] ||
           eventType cqmsg `elem` [PrivateMessage, GroupMessage] && filterMsg cqmsg
          then do
            when (eventType cqmsg `elem` [PrivateMessage, GroupMessage]) $ $(logInfo) $ pack $ fromMaybe "喵喵" (maybeBotName name) <> " -> Proxy : " <> take 512 (bsToString bs)
            liftIO $ mapM_ (`sendToProxy` bs) proxyDatas
            makeHeader >>= \case
              Nothing -> return ()
              Just headers -> do
                pending <- pendingProxies <$> readModuleStateL (Proxy @ProxyWS)
                lift . mapM_ (\pd -> runProxyWS pd headers) $ pending
                unless (null pending) $ modifyModuleState (Proxy @ProxyWS) $ \s -> s { pendingProxies = [] }
          else return ()
      _ -> return ()
    where filterMsg cqmsg = isJust $ runParser ($(itemInQ ['!', '！', '/']) >> getItem) (fromMaybe "" $ message cqmsg)
