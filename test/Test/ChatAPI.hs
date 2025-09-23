module Test.ChatAPI where

import Control.Monad.Except
import External.ChatAPI
import External.ChatAPI.Tool
import Network.HTTP.Client (Manager)
import Test.Tasty
import Test.Tasty.HUnit
import Utils.Logging
import Utils.Text
import Data.Default

timeoutHttp :: Int
timeoutHttp = 30 * 1000000

testChatAPI :: Manager -> TestTree
testChatAPI man = testGroup "ChatAPI Round Trip"
  [
  --   testGroup "OpenRouter DeepSeekV3 Free"
  --   [ testCaseInfo "Say hi" $ do
  --       let params = ChatParams
  --             { chatMarkDown = False
  --             , chatSetting = def
  --             , chatManager = man
  --             , chatTimeout = timeoutHttp
  --             } :: ChatParams (OpenRouter OR_DeepSeekV3_Free) '[]
  --       res <- runStdoutLoggingT . runExceptT $ messageChat params [UserMessage "你好"]
  --       case content <$> res of
  --         Left err -> assertFailure $ "messageChat failed: " ++ show err
  --         Right r  -> return $ unpack r
  --   ]
  testGroup "Local Model Qwen3_30B"
    [ testCaseInfo "Say hi" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = def
              , chatManager = man
              , chatTimeout = timeoutHttp
              } :: ChatParams (Local Qwen3_30B) '[]
        res <- runStdoutLoggingT . runExceptT $ messageChat params [UserMessage "你好"]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r  -> return $ unpack r
    ]
  -- , testGroup "DeepSeek API"
  --   [ testCaseInfo "Say hi" $ do
  --       let params = ChatParams
  --             { chatMarkDown = False
  --             , chatSetting = def
  --             , chatManager = man
  --             , chatTimeout = timeoutHttp
  --             } :: ChatParams (DeepSeek DeepSeekChat) '[]
  --       res <- runStdoutLoggingT . runExceptT $ messageChat params [UserMessage "你好"]
  --       case content <$> res of
  --         Left err -> assertFailure $ "messageChat failed: " ++ show err
  --         Right r -> return $ unpack r
  --   ]
  -- , testGroup "OpenRouter API"
  --   [ testCaseInfo "Say hi" $ do
  --       let params = ChatParams
  --             { chatMarkDown = False
  --             , chatSetting = def
  --             , chatManager = man
  --             , chatTimeout = timeoutHttp
  --             } :: ChatParams (OpenRouter OR_DeepSeekR1_Free) '[]
  --       res <- runStdoutLoggingT . runExceptT $ messageChat params [UserMessage "你好"]
  --       case content <$> res of
  --         Left err -> assertFailure $ "messageChat failed: " ++ show err
  --         Right r -> return $ unpack r
  --   ]
  -- , testGroup "OpenAI API"
  --   [ testCaseInfo "Say hi" $ do
  --       let params = ChatParams
  --             { chatMarkDown = False
  --             , chatSetting = def
  --             , chatManager = man
  --             , chatTimeout = timeoutHttp
  --             } :: ChatParams (OpenAI GPT4oMini) '[]
  --       res <- runStdoutLoggingT . runExceptT $ messageChat params [UserMessage "你好"]
  --       case content <$> res of
  --         Left err -> assertFailure $ "messageChat failed: " ++ show err
  --         Right r -> return $ unpack r
  --   ]
  , testGroup "Agent Time Query"
    [ testCaseInfo "Say time" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = def
              , chatManager = man
              , chatTimeout = timeoutHttp
              } :: ChatParams (Local Qwen3_30B) '[TimeTool]
        res <- runStdoutLoggingT . runExceptT $ messageChat params
          [ UserMessage "What is the time now?"
          , AssistantMessage "{\"tool\": \"time\", \"args\": {\"timezone\": 8}}" Nothing Nothing Nothing
          , UserMessage "{\"tool_output\": \"2025-02-09 13:05:55.689695563 UTC\"}"
          ]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r -> return $ unpack r
    ]
  -- , testGroup "Agent Time Query"
  --   [ testCase "Query time" $ do
  --       let params = ChatParams
  --             { chatMarkDown = False
  --             , chatSetting = def
  --             , chatManager = man
  --             , chatTimeout = timeoutHttp
  --             } :: ChatParams (OpenAI GPT4oMini) '[TimeTool]
  --       (res, _) <- runStdoutLoggingT $ messagesChat params [UserMessage "现在几点了"]
  --       case res of
  --         Left err -> assertFailure $ "messagesChat failed: " ++ show err
  --         Right res -> mapM_ printMessage res
  --    ]
  ]

testTools :: TestTree
testTools = testGroup "Tools test"
  [ testCase "TimeTool" sanityCheckTimeTool
  , testCase "FibonacciTool" sanityCheckFibonacciTool
  ]


