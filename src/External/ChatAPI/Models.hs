module External.ChatAPI.Models where

import Data.Aeson
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data ChatModel
  = OpenAI OpenAIModel
  | DeepSeek DeepSeekModel
  | Local LocalModel
  | OpenRouter OpenRouterModel
  | SiliconFlow SiliconFlowModel
  | Anthropic AnthropicModel
  | XcApi XcApiModel
  deriving (Show, Read, Eq, Generic, NFData)

data OpenAIModel      = GPT4oMini | GPT4o | O1Mini | O3Mini deriving (Show, Read, Eq, Generic, NFData)
data DeepSeekModel    = DeepSeekChat | DeepSeekReasoner deriving (Show, Read, Eq, Generic, NFData)
data LocalModel       = Qwen3_30B | QwQ | Qwen2_5_32B | Command_R_Latest | DummyTestModel deriving (Show, Read, Eq, Generic, NFData)
data OpenRouterModel  = OR_DeepSeekV3_Free | OR_DeepSeekR1_Free | OR_DeepSeekR1 deriving (Show, Read, Eq, Generic, NFData)
data SiliconFlowModel = SF_DeepSeekV3 | SF_DeepSeekR1 deriving (Show, Read, Eq, Generic, NFData)
data AnthropicModel   = Claude_3_7 deriving (Show, Read, Eq, Generic, NFData)
data XcApiModel       = XC_Claude_3_7 | XC_Claude_3_5 deriving (Show, Read, Eq, Generic, NFData)

modelEndpoint :: ChatModel -> String
modelEndpoint OpenAI {}      = "https://api.openai.com/v1/chat/completions"
modelEndpoint DeepSeek {}    = "https://api.deepseek.com/chat/completions"
modelEndpoint  (Local DummyTestModel) = "http://localhost:8000/v1/chat/completions"
modelEndpoint Local {}       = "http://10.52.1.55:11434/api/chat" -- ^ my local network, won't work for anyone else
modelEndpoint OpenRouter {}  = "https://openrouter.ai/api/v1/chat/completions"
modelEndpoint SiliconFlow {} = "https://api.siliconflow.cn/v1/chat/completions"
modelEndpoint Anthropic {}   = "https://api.anthropic.com/v1/messages"
modelEndpoint XcApi {}       = "http://xcapi.top/v1/chat/completions"

instance ToJSON ChatModel where
  toJSON (OpenAI GPT4oMini)              = "gpt-4o-mini"
  toJSON (OpenAI GPT4o)                  = "gpt-4o"
  toJSON (OpenAI O1Mini)                 = "o1-mini"
  toJSON (OpenAI O3Mini)                 = "o3-mini"
  toJSON (DeepSeek DeepSeekChat)         = "deepseek-chat"
  toJSON (DeepSeek DeepSeekReasoner)     = "deepseek-reasoner"
  toJSON (Local DummyTestModel)          = "dummy-test-model"
  toJSON (Local Qwen2_5_32B)             = "qwen2.5:32b"
  toJSON (Local Command_R_Latest)        = "command-r:latest"
  toJSON (Local QwQ)                     = "qwq:latest"
  toJSON (Local Qwen3_30B)               = "qwen3:30b"
  toJSON (OpenRouter OR_DeepSeekR1)      = "deepseek/deepseek-r1"
  toJSON (OpenRouter OR_DeepSeekV3_Free) = "deepseek/deepseek-chat:free"
  toJSON (OpenRouter OR_DeepSeekR1_Free) = "deepseek/deepseek-r1:free"
  toJSON (SiliconFlow SF_DeepSeekV3)     = "deepseek-ai/DeepSeek-V3"
  toJSON (SiliconFlow SF_DeepSeekR1)     = "deepseek-ai/DeepSeek-R1"
  toJSON (Anthropic Claude_3_7)          = "claude-3.7-sonnet"
  toJSON (XcApi XC_Claude_3_7)           = "[W2+]claude-3.7-sonnet"
  toJSON (XcApi XC_Claude_3_5)           = "[W]claude-3.5-sonnet"

