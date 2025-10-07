module Module.RecvSentCQ where

import Control.Concurrent.STM
import Control.Monad.Effect
import Control.System
import MeowBot.BotStructure
import Module.RS.QQ
import qualified Data.ByteString.Lazy as BL

[makeRModule__|
RecvSentCQ
  meowSentCQ        :: TVar (Maybe SentCQMessage)
  meowRecvCQ        :: TVar (Maybe ReceCQMessage)
  meowRawByteString :: TVar (Maybe BL.ByteString)
|]

instance SystemModule RecvSentCQ where
  data ModuleInitData RecvSentCQ = RecvSentCQInitData
  data ModuleEvent    RecvSentCQ = RecvSentCQEvent

instance Loadable c RecvSentCQ mods where
  initModule _ = do
    sentCQVar <- liftIO $ newTVarIO Nothing
    recvCQVar <- liftIO $ newTVarIO Nothing
    rawBSVar  <- liftIO $ newTVarIO Nothing
    return ( RecvSentCQRead sentCQVar recvCQVar rawBSVar
           , RecvSentCQState
           )

withRecvSentCQ :: (MonadIO m, ConsFDataList FData (RecvSentCQ : mods)) => EffT (RecvSentCQ : mods) es m a -> EffT mods es m a
withRecvSentCQ act = do
  sentCQVar <- liftIO $ newTVarIO Nothing
  recvCQVar <- liftIO $ newTVarIO Nothing
  rawBSVar  <- liftIO $ newTVarIO Nothing
  runRecvSentCQ (RecvSentCQRead sentCQVar recvCQVar rawBSVar) act
