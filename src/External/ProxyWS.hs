{-# LANGUAGE LambdaCase, OverloadedStrings #-}
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
  , proxyClientForWS
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (ByteString)
-- import Utils.ByteString (bsToString)

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

type AddressString = String
type PortInt = Int

data ProxyData = ProxyData
  { proxyAddr    :: AddressString
  , proxyPort    :: PortInt
  , proxyChans   :: (TBQueue ByteString, TBQueue ByteString)
  }

instance Show ProxyData where
  show (ProxyData addr port _) = "ProxyData { proxyAddr = " ++ addr ++ ", proxyPort = " ++ show port ++ " }"

cqhttpHeaders :: Int -> Headers
cqhttpHeaders sid = 
  [ ("X-Client-Role", "Universal")
  , ("X-Self-Id", B8.pack $ show sid)
  ]

sendToProxy :: ProxyData -> ByteString -> IO ()
sendToProxy (ProxyData _ _ (chanIn, _)) msg = atomically $ writeTBQueue chanIn msg

receiveFromProxy :: ProxyData -> STM ByteString
receiveFromProxy (ProxyData _ _ (_, chanOut)) = readTBQueue chanOut

createProxyData :: AddressString -> PortInt -> IO ProxyData
createProxyData addr port = do
  chanIn  <- newTBQueueIO 10
  chanOut <- newTBQueueIO 10
  return $ ProxyData addr port (chanIn, chanOut) 

proxyClientForWS :: a ~ ByteString => Maybe (TBQueue a, TBQueue a) -> Headers -> AddressString -> PortInt -> IO (TBQueue a, TBQueue a)
proxyClientForWS ioChans headers address port = do
  chanIn  <- maybe (newTBQueueIO 10) (return . fst) ioChans
  chanOut <- maybe (newTBQueueIO 10) (return . snd) ioChans
  proxyClient Nothing chanIn chanOut `forkFinally` \case
    Left e  -> do
      putStrLn $ "Connection to " ++ address ++ ":" ++ show port ++ " broken: " ++ show e
      putStrLn "Restarting in 30 seconds owo"
      threadDelay 30_000_000
      void $ proxyClientForWS (Just (chanIn, chanOut)) headers address port
    Right _ -> return ()
  return (chanIn, chanOut)
  where 
    proxyClient masync chanIn chanOut = 
      runClientWith address port "" defaultConnectionOptions headers $ \conn -> do
        putStrLn $ "Connected to " ++ address ++ ":" ++ show port
        putStrLn $ "Headers: " ++ show headers
        clientLoop masync conn (chanIn, chanOut)
    clientLoop :: a ~ ByteString => Maybe (Async a) -> Connection -> (TBQueue a ,TBQueue a) -> IO never_returns
    clientLoop asyncReceiveData conn (chanIn, chanOut) = do
      asyncReceiveData' <- fromMaybe (async $ receiveData conn) (return <$> asyncReceiveData)
      inOrOut <- atomically $ Left <$> readTBQueue chanIn <|> Right <$> waitSTM asyncReceiveData'
      case inOrOut of
        Left  msg -> do
          -- putStrLn "Sending message to proxy : " >> putStr (bsToString msg)
          sendTextData conn msg
          clientLoop (Just asyncReceiveData') conn (chanIn, chanOut)
        Right msg -> do
          -- putStrLn "Received message from proxy : " >> putStr (bsToString msg)
          atomically $ writeTBQueue chanOut msg
          clientLoop Nothing conn (chanIn, chanOut)
