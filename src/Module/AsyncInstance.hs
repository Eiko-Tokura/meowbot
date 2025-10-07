{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.AsyncInstance where

import Module.RS
import Control.Monad.Effect
import Control.Monad.RS.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Applicative
import qualified Data.Set as S

import Control.Monad.Effect
import Control.System
import Module.Logging
import Module.RS.QQ
import Module.Async
import System.Meow
import MeowBot.BotStructure
import Utils.Lens
import Language.Haskell.TH

instance Module AsyncModule where
  data    ModuleRead  AsyncModule = AsyncRead
  newtype ModuleState AsyncModule = AsyncState { asyncSet :: S.Set (Async (Meow [BotAction])) }

makeLenses_ 'AsyncState

instance SystemModule AsyncModule where
  data ModuleInitData AsyncModule = AsyncInitData
  data ModuleEvent    AsyncModule = AsyncEvent { completedAsync :: Async (Meow [BotAction]), meowAction :: Meow [BotAction] } -- ^ the (completed) async handle and the action to run


instance Dependency' c AsyncModule '[MeowActionQueue, LoggingModule] mods
  => Loadable c AsyncModule mods where
  initModule _ = return (AsyncRead, AsyncState S.empty)

  moduleEvent = do
    asyncs <- getsModule asyncSet
    return $ asum [ AsyncEvent ba <$> waitSTM ba | ba <- S.toList asyncs ]

  handleEvent (AsyncEvent completedAsync meowAct) = do
    $(logDebug) "Async Event Completed"
    modifyModule $ AsyncState . S.delete completedAsync . asyncSet
    meowList <- asksModule meowReadsAction
    liftIO $ atomically $ modifyTVar meowList (++ [meowAct])

withAsyncModule ::
  ( LoggingModule `In` mods
  , MeowActionQueue `In` mods
  , ConsFDataList FData (AsyncModule : mods)
  , Monad m
  )
  => EffT (AsyncModule : mods) es m a -> EffT mods es m a
withAsyncModule = runEffTOuter_ AsyncRead (AsyncState S.empty)

-- instance HasSystemRead (TVar [Meow [BotAction]]) r => MeowModule r AllData AsyncModule where
--   data ModuleLocalState AsyncModule  = AsyncModuleL { asyncSet :: S.Set (Async (Meow [BotAction])) }
--   data ModuleEarlyLocalState AsyncModule = AsyncEarlyLocalState
--   data ModuleGlobalState AsyncModule = AsyncModuleG
--   data ModuleEvent AsyncModule       = AsyncEvent { completedAsync :: Async (Meow [BotAction]), meowAction :: Meow [BotAction] } -- ^ the (completed) async handle and the action to run
--   data ModuleInitDataG AsyncModule   = AsyncInitDataG
--   data ModuleInitDataL AsyncModule   = AsyncInitDataL
-- 
--   getInitDataG _ = (Just AsyncInitDataG, empty)
-- 
--   getInitDataL _ = (Just AsyncInitDataL, empty)
-- 
--   initModule _ _ = return AsyncModuleG
-- 
--   initModuleLocal _ _ _ _ _ = return $ AsyncModuleL S.empty
-- 
--   initModuleEarlyLocal _ _ _ = return AsyncEarlyLocalState
-- 
--   quitModule _ = do
--     AsyncModuleL asyncs <- readModuleStateL (Proxy @AsyncModule)
--     liftIO $ mapM_ cancel asyncs
-- 
--   moduleEvent _ = do
--     AsyncModuleL asyncs <- readModuleStateL (Proxy @AsyncModule)
--     return $ asum [ AsyncEvent ba <$> waitSTM ba | ba <- S.toList asyncs ]
-- 
--   moduleEventHandler p (AsyncEvent completedAsync meowAct) = do
--     $(logDebug) "Async Event Completed"
--     modifyModuleState p $ AsyncModuleL . S.delete completedAsync . asyncSet
--     meowList <- asks (readSystem . snd)
--     liftIO $ atomically $ modifyTVar meowList (++ [meowAct])
-- 
