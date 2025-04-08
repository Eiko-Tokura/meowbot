{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- Author : Eiko chan >w<
-- | In this module we define the functionalities to proxy over a WebSocket connection.
--
-- this will be used to proxy the messages from the client to the server and vice versa.
module External.ProxyWS
  ( Headers
  , ProxyData(..)
  , cqhttpHeaders
  , sendToProxy
  , receiveFromProxy
  , createProxyData
  , runProxyWS
  , proxyClientForWS
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Applicative
import Data.Maybe (fromMaybe)
import Utils.Logging

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T

import Network.WebSockets

-- The server C connects at
--
-- botws.desu.life:65000
--
-- Implementation plan:
--
-- plan 1:
--
-- * run two clients A, B
--
--   A connects to the server Q
--
--   B connects to the server C
--
--   A receives a message from Q and sends it to B, then B sends it to C
--
--   Q    C
--   |    ^
--   v    |
--   A -> B
--
--   B receives a message from C and sends it to A, then A sends it to Q
--
--   Q    C
--   ^    |
--   |    v
--   A <- B
--
-- plan 2:
--
--   The current thread that already connects to Q, will also broadcast the message over a channel or a TChan, this needs to be bi-directional
--
--   we only need to make a client B that connects to C and reads from the channel or TChan
--
--
-- In any case, we need to have B connect to C.
--
-- the plan is to write a function for B, that
--
-- * launches two TChan for reading and writing

tbQueueBound :: Integral a => a
tbQueueBound = 16

type AddressString = String
type PortInt = Int

data ProxyData = ProxyData
  { proxyAddr    :: AddressString
  , proxyPort    :: PortInt
  , proxyChans   :: (TBQueue ByteString, TBQueue ByteString)
  , proxyRunning :: TVar Bool -- ^ This is used to check if the proxy is already running, avoid creating multiple proxies
  }

instance Show ProxyData where
  show (ProxyData addr port _ _) = "ProxyData { proxyAddr = " ++ addr ++ ", proxyPort = " ++ show port ++ " }"

-- | The headers for cqhttp connection.
cqhttpHeaders :: Int -> Headers
cqhttpHeaders sid =
  [ ("X-Client-Role", "Universal")
  , ("X-Self-Id", B8.pack $ show sid)
  ]

-- | Send a message to the proxy (write to TBQueue).
sendToProxy :: ProxyData -> ByteString -> IO ()
sendToProxy (ProxyData _ _ (chanIn, _) _) msg = atomically $ writeTBQueue chanIn msg

-- | Receive a message from the proxy (read from TBQueue).
receiveFromProxy :: ProxyData -> STM ByteString
receiveFromProxy (ProxyData _ _ (_, chanOut) _) = readTBQueue chanOut

-- | Create a new 'ProxyData' with the provided address and port, and empty TBQueues.
createProxyData :: AddressString -> PortInt -> IO ProxyData
createProxyData addr port = do
  chanIn  <- newTBQueueIO tbQueueBound
  chanOut <- newTBQueueIO tbQueueBound
  running <- newTVarIO False
  return $ ProxyData addr port (chanIn, chanOut) running

-- | Create a proxy client thread for a WebSocket connection, using the provided headers. It will avoid creating multiple proxies.
-- wraps 'proxyClientForWS' and starts a new thread for it.
runProxyWS :: ProxyData -> Headers -> LoggingT IO ()
runProxyWS (ProxyData addr port chans running) headers = do
  alreadyRunning <- liftIO . atomically $ readTVar running
  unless alreadyRunning $ do
    liftIO . atomically $ writeTVar running True
    void $ proxyClientForWS (Just chans) headers addr port

-- | Create a proxy client for a WebSocket connection, using the provided headers and TBQueues.
proxyClientForWS :: a ~ ByteString => Maybe (TBQueue a, TBQueue a) -> Headers -> AddressString -> PortInt -> LoggingT IO (TBQueue a, TBQueue a)
proxyClientForWS ioChans headers address port = do
  chanIn  <- maybe (liftIO $ newTBQueueIO tbQueueBound) (return . fst) ioChans
  chanOut <- maybe (liftIO $ newTBQueueIO tbQueueBound) (return . snd) ioChans
  proxyClient Nothing chanIn chanOut `logForkFinally` \case
    Left e  -> do
      $(logWarn) $ "Connection to " <> T.pack address <> ":" <> T.pack (show port) <> " broken: " <> T.pack (show e)
      $(logWarn) "Restarting in 30 seconds owo"
      asyncDelay <- liftIO $ async $ threadDelay 30_000_000
      let discardWhileWaiting wait = do
            delayEnded <- atomically $ pure True <* waitSTM wait <|> pure False <* readTBQueue chanIn
            unless delayEnded $ discardWhileWaiting wait
      liftIO $ discardWhileWaiting asyncDelay
      void $ proxyClientForWS (Just (chanIn, chanOut)) headers address port
    Right _ -> return ()
  return (chanIn, chanOut)
  where
    proxyClient masync chanIn chanOut = do
      logThroughCont (runClientWith address port "" defaultConnectionOptions headers) $ \conn -> do
        $(logInfo) $ "Connected to " <> T.pack address <> ":" <> T.pack (show port)
        $(logInfo) $ "Headers: " <> T.pack (show headers)
        clientLoop masync conn (chanIn, chanOut)
    clientLoop :: a ~ ByteString => Maybe (Async a) -> Connection -> (TBQueue a ,TBQueue a) -> LoggingT IO never_returns
    clientLoop asyncReceiveData conn (chanIn, chanOut) = do
      asyncReceiveData' <- fromMaybe (liftIO . async $ receiveData conn) (return <$> asyncReceiveData)
      inOrOut <- liftIO . atomically $ Left <$> readTBQueue chanIn <|> Right <$> waitSTM asyncReceiveData'
      case inOrOut of
        Left  msg -> do
          -- putStrLn "Sending message to proxy : " >> putStr (bsToString msg)
          liftIO (sendTextData conn msg) `logCatch` \e -> do
            $(logError) $ "Error sending message to proxy: " <> T.pack (show (e :: SomeException))
            liftIO $ uninterruptibleCancel asyncReceiveData' -- cancel the async receive thread
            liftIO $ throwIO e
          clientLoop (Just asyncReceiveData') conn (chanIn, chanOut)
        Right msg -> do
          -- putStrLn "Received message from proxy : " >> putStr (bsToString msg)
          liftIO . atomically $ writeTBQueue chanOut msg
          clientLoop Nothing conn (chanIn, chanOut)
