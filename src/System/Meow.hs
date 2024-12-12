{-# LANGUAGE TypeFamilies, DataKinds, DerivingVia #-}
module System.Meow where

import Control.Monad.Trans.ReaderState
import Control.Monad.Logger
import System
import Data.Kind
import Data.Bifunctor
import MeowBot.BotStructure
import Data.Time.Clock
import Network.WebSockets

-- the hierarchy of types:
--
--           SystemT r s mods m a
--           /                  \   (set s = AllData)
-- ModuleT r s l m a         CatT r mods m a
--                               |  (split AllData into (WholeChat, BotConfig) and OtherData)
--                           MeowT r mods m a
--
-- their raw definitions are
--
-- SystemT r s mods m a = ReaderStateT (AllModuleGlobalStates mods, r)  (AllModuleLocalStates mods, s) (LoggingT m) a
-- ModuleT r s l m a    = ReaderStateT (ModuleGlobalState l, r)         (ModuleLocalState l, s) (LoggingT m) a
-- CatT r mods m a      = ReaderStateT (AllModuleGlobalStates mods, r)  AllData (LoggingT m) a
-- MeowT r mods m a     = ReaderStateT ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r)) (AllModuleLocalStates mods, OtherData) (LoggingT m) a
--
-- since ModuleT is not stronger than MeowT, we cannot run MeowT in ModuleT.
-- this might not be good for our AsyncModule, since it wants to run Meow [BotAction] in the module.
--
-- there are three ways to solve this:
-- 1. Deprecate ModuleT and use SystemT for all modules.
-- 2. Restrict and add a weaker type for MeowT, so that it can be run in ModuleT.
-- 3. Set a global state for the AsyncModule

-- | The modules loaded into the bot
type Mods   = '[]
type Meow a = MeowT Connection Mods IO a
type Cat  a = CatT  Connection Mods IO a

newtype CatT r mods m a = CatT { runCatT :: SystemT r AllData mods m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving 
    ( MonadReader (AllModuleGlobalStates mods, r)
    , MonadState (AllModuleLocalStates mods, AllData)
    ) via ReaderStateT (AllModuleGlobalStates mods, r) (AllModuleLocalStates mods, AllData) (LoggingT m)
-- ^ the monad that the bot runs in
-- running in this monad it is necessary to block other threads from modifying the data.
-- so avoid running long blocking operations in this monad, use async and staged actions instead.
-- type System mods = SystemT () AllData mods (LoggingT IO) ()

-- | The monad transformer that the bot runs in.
newtype MeowT (r :: Type) (mods :: [Type]) (m :: Type -> Type) a = MeowT 
  { runMeowT :: 
      ReaderStateT 
        ( (WholeChat, BotConfig)
        , (AllModuleGlobalStates mods, r)
        )
        ( AllModuleLocalStates mods
        , OtherData
        )
        (LoggingT m) -- ^ the monad to run in
        a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving 
    ( MonadReader ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r))
    , MonadState (AllModuleLocalStates mods, OtherData)
    ) via ReaderStateT ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r)) (AllModuleLocalStates mods, OtherData) (LoggingT m)

-----------------------------------------------------------------------
instance MonadTrans (MeowT r mods) where
  lift = MeowT . lift . lift
  {-# INLINE lift #-}

instance Monad m => MonadReadable BotConfig (MeowT r mods m) where
  query = asks (snd . fst)
  {-# INLINE query #-}

instance Monad m => MonadReadable BotModules (MeowT r mods m) where
  query = queries botModules
  {-# INLINE query #-}

instance Monad m => MonadReadable RunningMode (MeowT r mods m) where
  query = queries runningMode
  {-# INLINE query #-}

instance Monad m => MonadReadable WholeChat (MeowT r mods m) where
  query = asks (fst . fst)
  {-# INLINE query #-}

instance Monad m => MonadReadable BotName (MeowT r mods m) where
  query = queries nameOfBot
  {-# INLINE query #-}

-- instance Monad m => MonadReadable CQMessage (MeowT r mods m) where
--   query = asks (getNewMsg . fst . fst)
--   {-# INLINE query #-}

instance MonadIO m => MonadReadable UTCTime (MeowT r mods m) where
  query = liftIO getCurrentTime
  {-# INLINE query #-}

instance Monad m => MonadReadable (AllModuleGlobalStates mods) (MeowT r mods m) where
  query = asks (fst . snd)
  {-# INLINE query #-}

instance Monad m => MonadReadable AllData (MeowT r mods m) where
  query = do
    (wc, bc) <- asks fst
    (_, od) <- get
    return $ AllData wc bc od
  {-# INLINE query #-}

instance Monad m => MonadReadable OtherData (MeowT r mods m) where
  query = gets snd
  {-# INLINE query #-}

instance Monad m => MonadModifiable OtherData (MeowT r mods m) where
  change f = modify $ second f
  {-# INLINE change #-}

------------------------------------------------------------------------
instance MonadTrans (CatT r mods) where
  lift = CatT . lift . lift
  {-# INLINE lift #-}

instance Monad m => MonadReadable AllData (CatT r mods m) where
  query = gets snd
  {-# INLINE query #-}

instance Monad m => MonadModifiable AllData (CatT r mods m) where
  change f = modify $ second f
  {-# INLINE change #-}

-- the meow part should be able o read all global states and local states, i.e. running in the monad
-- 
-- -- ideally, the loop should look like this
-- botLoop :: System mods
-- botLoop = do
--   event <- receiveEvent
--   beforeMeow
--   meowHandleEvent event
--   afterMeow
--   botLoop
-- 
-- botClient :: BotModules -> RunningMode -> ClientApp ()
-- botClient mods mode connection = do
--   putStrLn "Connected to go-cqhttp WebSocket server."
--   initialData mods >>= void . runStateT (botLoop Nothing mods mode connection)
-- 
-- botServer :: BotModules -> RunningMode -> PendingConnection -> IO ()
-- botServer mods mode connection = do
--   conn <- acceptRequest connection
--   putStrLn "As server, connected to go-cqhttp WebSocket client."
--   initialData mods >>= void . runStateT (botLoop Nothing mods mode conn)
-- 
-- -- | changed the model to allow some concurrency
-- botLoop :: Maybe (Async BL.ByteString) -> BotModules -> RunningMode -> Connection -> StateT AllData IO never_returns
-- botLoop reuseAsyncMsgText mods mode conn = do
--   asyncMsgText    <- maybe (lift $ async $ traceModeWith DebugJson mode bsToString <$> receiveData conn) return reuseAsyncMsgText
--   asyncActionList <- gets (S.toList . asyncActions . otherdata)
--   prevData        <- get
--   result <- lift . atomically $ asum
--     [ Left <$> waitSTM asyncMsgText
--     , Right . Left  <$> asum [ (ba, ) <$> waitSTM ba | ba <- asyncActionList ]
--     , Right . Right <$> asum ( map receiveFromProxy (proxyTChans mods) )
--     ]
--   newAsyncMsg <- case result of
--     Left  msgBS                                  -> handleMessage mods mode conn msgBS >> return Nothing
--     Right (Left (completedAsync, meowBotAction)) -> handleCompletedAsync conn completedAsync meowBotAction >> return (Just asyncMsgText)
--     Right (Right proxyMsg)                       -> do
--       lift $ putStrLn $ fromMaybe "喵喵" (nameOfBot mods) ++ " <- Proxy : " ++ take 512 (bsToString proxyMsg)
--       lift $ sendTextData conn proxyMsg `cancelOnException` asyncMsgText
--       return (Just asyncMsgText)
--   saveData prevData
--   botLoop newAsyncMsg mods mode conn
-- 
-- -- | deregister the completed async action and do the bot action
-- handleCompletedAsync :: Connection -> Async (Meow [BotAction]) -> Meow [BotAction] -> StateT AllData IO ()
-- handleCompletedAsync conn completedAsync meowBotAction = do
--   -- remove the completed async action from the set
--   newAsyncSet <- gets (S.delete completedAsync . asyncActions . otherdata)
--   modify $ \ad -> ad { otherdata = (otherdata ad) { asyncActions = newAsyncSet } }
--   -- do the bot action in the Meow monad
--   globalizeMeow $ meowBotAction >>= mapM_ (doBotAction conn)
--   -- update the saved data if needed
--   updateSavedAdditionalData
-- 
-- -- | handle the message from the server, also handles proxy messages
-- handleMessage :: BotModules -> RunningMode -> Connection -> BL.ByteString -> StateT AllData IO ()
-- handleMessage mods mode conn msgBS = do
--   let eCQmsg  = eitherDecode msgBS :: Either String CQMessage
--       nameBot = fromMaybe "喵喵" $ nameOfBot mods
--   case traceModeWith DebugCQMessage mode (((nameBot ++ "debug: ") ++) . show) eCQmsg of
--     Left errMsg -> lift $ putStrLn $ nameBot ++ (" Failed to decode message: " ++ errMsg ++ "\n" ++ bsToString msgBS)
--     Right cqmsg -> case eventType cqmsg of
--       LifeCycle -> updateSelfInfo cqmsg >> doProxyWork (not . null $ mode) nameBot
--       HeartBeat -> doProxyWork (not . null $ mode) nameBot
--       Response -> do
--         modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg
--         updateSavedAdditionalData
--         lift $ putStrLn $ nameBot ++ " <- response."
--       PrivateMessage -> do
--         updateStates nameBot cqmsg
--         doBotCommands conn (filter ((`elem` canUsePrivateCommands mods) . identifier) allPrivateCommands)
--         when (filterMsg cqmsg) $ doProxyWork True nameBot
--       GroupMessage -> do
--         updateStates nameBot cqmsg
--         doBotCommands conn (filter ((`elem` canUseGroupCommands mods) . identifier) allGroupCommands)
--         when (filterMsg cqmsg) $ doProxyWork True nameBot
--       _ -> return ()
--       where
--         updateStates nameBot cqmsg = do
--           cqmsg' <- (\mid -> cqmsg {absoluteId = Just mid}) <$> gIncreaseAbsoluteId
--           modify $ (`using` rseqWholeChat) . updateAllDataByMessage cqmsg'
--           updateSavedAdditionalData
--           lift $ putStrLn $ nameBot ++ " <- " ++ showCQ cqmsg'
--         filterMsg cqmsg' =  isJust $ runParser ($(itemInQ ['!', '！', '/']) >> getItem) (fromMaybe "" $ message cqmsg')
--         doProxyWork shouldPrint nameBot | null (proxyTChans mods) = return ()
--                                         | otherwise = do
--           when shouldPrint $ lift $ putStrLn (nameBot ++ " -> Proxy ") >> putStr (bsToString msgBS ++ "\n")
--           lift $ mapM_ (`sendToProxy` msgBS) (proxyTChans mods)
--           makeHeader >>= \case
--             Nothing      -> return ()
--             Just headers -> do
--               pending <- gets (pendingProxies . otherdata)
--               lift . mapM_ (\pd -> runProxyWS pd headers) $ pending
--               unless (null pending) $ modify $ \ad -> ad { otherdata = (otherdata ad) { pendingProxies = [] } }
-- 
