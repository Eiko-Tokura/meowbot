module External.ChatAPI.Tool.MeowToolEnv where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import System.General
import MeowBot.BotStructure

-- | This will be the monad the tools will run in, ReaderT is necessary to make UnliftIO work
newtype MeowToolEnvT r mods = MeowToolEnvT 
  { runMeowToolEnvT
    :: ReaderT 
        ( ( (WholeChat, BotConfig)
          , (AllModuleGlobalStates mods, r)
          )
        , ( AllModuleLocalStates mods
          , OtherData
          )
        )
        (LoggingT IO)
        a
  } deriving newtype 
    ( Functor, Applicative, Monad, MonadIO, MonadLogger
    , MonadReader
        ( ((WholeChat, BotConfig), (AllModuleGlobalStates mods, r))
        , (AllModuleLocalStates mods, OtherData)
        )
    )
