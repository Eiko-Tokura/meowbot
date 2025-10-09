module MeowBot.API where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson
import MeowBot.Data
import Module.MeowConnection
import System.Random
import Utils.ByteString
import Control.Monad.Effect
import Module.Logging

-- | Low-level functions to send any action
actionAPI ::
  ( MonadIO m
  , In' c LoggingModule mods
  , In' c MeowConnection mods
  , InList (ErrorText "send_connection") es
  )
  => ActionForm ActionAPI -> EffT' c mods es m ()
actionAPI af = do
  sendTextData $ encode af
  effAddLogCat' (LogCat @Text "Action") $ $(logInfo) $ "=> Action: " <> tshow af

-- | This keeps the send logic and the receive logic together
-- with continuation style passing functions around, we can enforce a lot of coupled relationships
queryAPI
  :: ( FromJSON (WithEcho (QueryAPIResponse queryType))
     , In' c LoggingModule mods
     , In' c MeowConnection mods
     , InList (ErrorText "send_connection") es
     , MonadIO m
     )
  => QueryAPI queryType
  -> EffT' c mods es m
       ( LazyByteString
       -> Maybe (QueryAPIResponse queryType)
       ) -- ^ the echo is captured in the returned function, force a tightly coupled relationship between the query and the response
queryAPI query = do
  echo <- generateUniqueEcho
  sendTextData $ encode (ActionForm query $ Just echo)
  $(logInfo) $ "=> Query: " <> tshow query <> " with echo: " <> echo
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

