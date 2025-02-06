module Test.ChatAPI where

import Control.Monad.Except
import External.ChatAPI
import Utils.Text
import Test.Tasty
import Test.Tasty.HUnit

testChatAPI :: TestTree
testChatAPI = testGroup "ChatAPI Round Trip"
  [ testGroup "Local Model DeepSeekR1_14B"
    [ testCaseInfo "Say hi" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = ChatSetting
                { systemMessage = Nothing
                , systemTemp = Nothing
                , systemMaxToolDepth = Nothing
                , systemApiKeys = Nothing
                }
              } :: ChatParams (Local DeepSeekR1_14B) '[]
        res <- runExceptT $ messageChat params [UserMessage "你好"]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r  -> return $ unpack r
    ]
  , testGroup "Local Model DeepSeekR1_32B"
    [ testCaseInfo "Say hi" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = ChatSetting
                { systemMessage = Nothing
                , systemTemp = Nothing
                , systemMaxToolDepth = Nothing
                , systemApiKeys = Nothing
                }
              } :: ChatParams (Local DeepSeekR1_32B) '[]
        res <- runExceptT $ messageChat params [UserMessage "你好"]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r  -> return $ unpack r
    ]
  , testGroup "DeepSeek API"
    [ testCaseInfo "Say hi" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = ChatSetting
                { systemMessage = Nothing
                , systemTemp = Nothing
                , systemMaxToolDepth = Nothing
                , systemApiKeys = Nothing
                }
              } :: ChatParams (DeepSeek DeepSeekChat) '[]
        res <- runExceptT $ messageChat params [UserMessage "你好"]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r -> return $ unpack r
    ]
  , testGroup "OpenAI API"
    [ testCaseInfo "Say hi" $ do
        let params = ChatParams
              { chatMarkDown = False
              , chatSetting = ChatSetting
                { systemMessage = Nothing
                , systemTemp = Nothing
                , systemMaxToolDepth = Nothing
                , systemApiKeys = Nothing
                }
              } :: ChatParams (OpenAI GPT4oMini) '[]
        res <- runExceptT $ messageChat params [UserMessage "你好"]
        case content <$> res of
          Left err -> assertFailure $ "messageChat failed: " ++ show err
          Right r -> return $ unpack r
    ]
  ]



