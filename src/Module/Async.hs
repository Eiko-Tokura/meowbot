{-# LANGUAGE TypeFamilies #-}
module Module.Async where

import Control.Monad.Trans.ReaderState
import Control.Concurrent.Async
import Control.Applicative
import MeowBot
import qualified Data.Set as S
import Module

data AsyncModule

instance MeowModule r AllData AsyncModule where
  type ModuleLocalState AsyncModule = S.Set (Async (Meow [BotAction]))
  type ModuleGlobalState AsyncModule = ()
  type ModuleEvent AsyncModule = (Async (Meow [BotAction]), Meow [BotAction]) -- ^ the (completed) async handle and the action to run

  initModule _ _ = return ()

  initModuleLocal _ _ = return S.empty

  quitModule _ = do
    asyncs <- readModuleStateL (Proxy @AsyncModule)
    liftIO $ mapM_ cancel asyncs

  moduleEvent _ = do
    asyncs <- readModuleStateL (Proxy @AsyncModule)
    return $ asum [ (ba, ) <$> waitSTM ba | ba <- S.toList asyncs ]

  moduleEventHandler p (completedAsync, meowAct) = do
    modifyModuleState p $ S.delete completedAsync
    mapM_ doBotAction meowAct
