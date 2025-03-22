module MeowBot.API where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Aeson
import MeowBot.Data
import Network.WebSockets (Connection, sendTextData)
import System.Random
import Utils.ByteString

-- | Low-level functions to send any action
actionAPI :: Connection -> ActionForm ActionAPI -> LoggingT IO ()
actionAPI conn af = do
  lift . sendTextData conn $ encode af
  $(logInfo) $ "=> Action: " <> tshow af

queryAPI
  :: ( MonadLogger m
     , MonadIO m
     , FromJSON (WithEcho (QueryAPIResponse queryType))
     )
  => Connection
  -> QueryAPI queryType
  -> m ( LazyByteString
       -> Maybe (QueryAPIResponse queryType)
       )
queryAPI conn query = do
  echo <- generateUniqueEcho
  liftIO $ sendTextData conn $ encode (ActionForm query $ Just echo)
  $(logInfo) $ "=> Query: " <> tshow query
  return $ \received -> do
    decoded :: WithEcho (QueryAPIResponse queryType) <- decode received
    hasEcho <- maybeEcho decoded
    if hasEcho == echo
    then Just $ params decoded
    else Nothing

generateUniqueEcho :: MonadIO m => m Text
generateUniqueEcho = do
  r :: Int <- liftIO randomIO
  return $ "echo-" <> tshow r

