-- | This module provides some simple async related wrappers for MeowBot.
module MeowBot.Async where

import MeowBot.BotStructure
import Control.Monad.Trans

asyncPureIOBotAction :: IO [BotAction] -> Meow [BotAction]
asyncPureIOBotAction = fmap (pure . BAPureAsync) . lift . async
{-# INLINE asyncPureIOBotAction #-}

asyncMeowBotAction :: IO (Meow [BotAction]) -> Meow [BotAction]
asyncMeowBotAction = fmap (pure . BAAsync) . lift . async
{-# INLINE asyncMeowBotAction #-}

-- | Separate the action into async IO action and the function to convert the result of the IO action to a list of BotActions.
-- the first part will be handled asynchronously, will not block the main thread.
-- the second part will be executed in the main thread, not asynchronously.
asyncMeowBot 
  :: IO a -- ^ The IO action to be executed asynchronously
  -> (a -> Meow [BotAction]) -- ^ The function to convert the result of the IO action to a list of BotActions
  -> Meow [BotAction] -- ^ The Meow action that will be executed asynchronously
asyncMeowBot ioa f = asyncMeowBotAction $ fmap f ioa
{-# INLINE asyncMeowBot #-}
