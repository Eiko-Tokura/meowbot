module Test.ChatAPI where

import Control.Monad.Except
import External.ChatAPI
import External.ChatAPI.Tool
import Utils.Text
import Test.Tasty
import Test.Tasty.HUnit
import Network.HTTP.Client (Manager)

timeoutHttp :: Int
timeoutHttp = 30 * 1000000

testChatAPI :: Manager -> TestTree
testChatAPI man = testGroup "ChatAPI Round Trip"
 -- testGroup "Local Model DeepSeekR1_14B"
 --   [ testCaseInfo "Say hi" $ do
 --       let params = ChatParams
 --             { chatMarkDown = False
 --             , chatSetting = ChatSetting
 --               { systemMessage = Nothing
 --               , systemTemp = Nothing
 --               , systemMaxToolDepth = Nothing
 --               , systemApiKeys = Nothing
 --               }
 --             , chatManager = man
 --             , chatTimeout = timeoutHttp
 --             } :: ChatParams (Local DeepSeekR1_14B) '[]
 --       res <- runExceptT $ messageChat params [UserMessage "你好"]
 --       case content <$> res of
 --         Left err -> assertFailure $ "messageChat failed: " ++ show err
 --         Right r  -> return $ unpack r
 --   ]
  --[ testGroup "DeepSeek API"
  --  [ testCaseInfo "Say hi" $ do
  --      let params = ChatParams
  --            { chatMarkDown = False
  --            , chatSetting = ChatSetting
  --              { systemMessage = Nothing
  --              , systemTemp = Nothing
  --              , systemMaxToolDepth = Nothing
  --              , systemApiKeys = Nothing
  --              }
  --            , chatManager = man
  --            , chatTimeout = timeoutHttp
  --            } :: ChatParams (DeepSeek DeepSeekChat) '[]
  --      res <- runExceptT $ messageChat params [UserMessage "你好"]
  --      case content <$> res of
  --        Left err -> assertFailure $ "messageChat failed: " ++ show err
  --        Right r -> return $ unpack r
  --  ]
  --, testGroup "OpenAI API"
  --  [ testCaseInfo "Say hi" $ do
  --      let params = ChatParams
  --            { chatMarkDown = False
  --            , chatSetting = ChatSetting
  --              { systemMessage = Nothing
  --              , systemTemp = Nothing
  --              , systemMaxToolDepth = Nothing
  --              , systemApiKeys = Nothing
  --              }
  --            , chatManager = man
  --            , chatTimeout = timeoutHttp
  --            } :: ChatParams (OpenAI GPT4oMini) '[]
  --      res <- runExceptT $ messageChat params [UserMessage "你好"]
  --      case content <$> res of
  --        Left err -> assertFailure $ "messageChat failed: " ++ show err
  --        Right r -> return $ unpack r
  --  ]
  [ testGroup "Agent Time Query"
    [ testCaseInfo "Say time" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = ChatSetting
                { systemMessage = Nothing
                , systemTemp = Nothing
                , systemMaxToolDepth = Nothing
                , systemApiKeys = Nothing
                }
              , chatManager = man
              , chatTimeout = timeoutHttp
              } :: ChatParams (OpenAI GPT4oMini) '[TimeTool]
        res <- runExceptT $ messageChat params 
          [ UserMessage "What is the time now?"
          , AssistantMessage "{\"tool\": \"time\", \"args\": {\"timezone\": 8}}" Nothing Nothing
          , UserMessage "{\"tool_output\": \"2025-02-09 13:05:55.689695563 UTC\"}"
          ]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r -> return $ unpack r
    ]
  , testGroup "Agent Time Query"
    [ testCase "Query time" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = ChatSetting
                { systemMessage = Nothing
                , systemTemp = Nothing
                , systemMaxToolDepth = Nothing
                , systemApiKeys = Nothing
                }
              , chatManager = man
              , chatTimeout = timeoutHttp
              } :: ChatParams (OpenAI GPT4oMini) '[TimeTool]
        res <- runExceptT $ messagesChat params [UserMessage "现在几点了"]
        case res of
          Left err -> assertFailure $ "messagesChat failed: " ++ show err
          Right res -> mapM_ printMessage res
     ]
  ]

testTools :: TestTree
testTools = testGroup "Tools test"
  [ testCase "TimeTool" sanityCheckTimeTool
  , testCase "FibonacciTool" sanityCheckFibonacciTool
  ]


