{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
module Module.AsyncInstance where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Effect
import Control.Monad.Logger
import Control.System
import Module.Async
import Module.Logging
import System.Meow
import Utils.Lens
import qualified Data.Set as S

instance Module AsyncModule where
  data    ModuleRead  AsyncModule = AsyncRead
  newtype ModuleState AsyncModule = AsyncState { asyncSet :: S.Set (Async (Meow [BotAction])) }

makeLenses_ 'AsyncState

instance SystemModule AsyncModule where
  data ModuleInitData AsyncModule = AsyncInitData
  data ModuleEvent    AsyncModule = AsyncEvent { completedAsync :: Async (Meow [BotAction]), meowAction :: Meow [BotAction] } -- ^ the (completed) async handle and the action to run


instance Dependency' c AsyncModule '[MeowActionQueue, LoggingModule] mods
  => Loadable c AsyncModule mods ies where
  withModule _ = runEffTOuter_ AsyncRead (AsyncState S.empty)

instance Dependency' c AsyncModule '[MeowActionQueue, LoggingModule] mods
  => EventLoop c AsyncModule mods es where
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
