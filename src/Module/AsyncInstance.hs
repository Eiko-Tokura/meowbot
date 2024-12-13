{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Module.AsyncInstance where

import Control.Monad.Trans.ReaderState
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Applicative
import qualified Data.Set as S
import Module
import Module.Async
import System.Meow
import MeowBot.BotStructure

instance HasSystemRead (TVar [Meow [BotAction]]) r => MeowModule r AllData AsyncModule where
  data ModuleLocalState AsyncModule  = AsyncModuleL { asyncSet :: S.Set (Async (Meow [BotAction])) }
  data ModuleGlobalState AsyncModule = AsyncModuleG
  data ModuleEvent AsyncModule       = AsyncEvent { completedAsync :: Async (Meow [BotAction]), meowAction :: Meow [BotAction] } -- ^ the (completed) async handle and the action to run
  data ModuleInitDataG AsyncModule   = AsyncInitDataG
  data ModuleInitDataL AsyncModule   = AsyncInitDataL

  getInitDataG _ = (Just AsyncInitDataG, empty)

  getInitDataL _ = (Just AsyncInitDataL, empty)

  initModule _ _ = return AsyncModuleG

  initModuleLocal _ _ _ _ = return $ AsyncModuleL S.empty

  quitModule _ = do
    AsyncModuleL asyncs <- readModuleStateL (Proxy @AsyncModule)
    liftIO $ mapM_ cancel asyncs

  moduleEvent _ = do
    AsyncModuleL asyncs <- readModuleStateL (Proxy @AsyncModule)
    return $ asum [ AsyncEvent ba <$> waitSTM ba | ba <- S.toList asyncs ]

  moduleEventHandler p (AsyncEvent completedAsync meowAct) = do
    modifyModuleState p $ AsyncModuleL . S.delete completedAsync . asyncSet
    meowList <- asks (readSystem . snd)
    liftIO $ atomically $ modifyTVar meowList (++ [meowAct])

