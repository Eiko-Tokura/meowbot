{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE DerivingVia, DeriveAnyClass #-}

{-|
Module      : External.ChatAPI
Description : LLM chat interface with manual tool calling support
Copyright   : (c) 2024 Eiko
License     : BSD-3-Clause
Maintainer  : eikochanowo@outlook.com
Stability   : experimental

This module implements LLM chat functionality with manual tool calling support
for models that don't natively support tool invocation via API parameters.
-}
module External.ChatAPI.Tools where

-- ... existing imports ...
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.Kind
import Data.HList
import Data.Proxy
import Data.String (IsString, fromString)
import GHC.TypeLits
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Control.Monad.Trans.Except
import Control.DeepSeq (NFData)
import Control.Exception
import Utils.ToText

type TextLazy = TL.Text

-- -- | Defines a parameter for tool calls
-- data Parameter (pt :: ParameterType) = Parameter
--   { paramName :: Text           -- ^ Parameter name
--   , paramType :: ParameterType  -- ^ Expected data type
--   , paramDesc :: Text           -- ^ Description for LLM prompting
--   } deriving (Show, Eq, Generic)

data Parameter name description
  = StringP name description
  | IntP    name description
  | BoolP   name description
  | FloatP  name description
  | ObjectP name description [Parameter Symbol Symbol]
  | MaybeP  name description (Parameter Symbol Symbol)

newtype StringT t = StringT Text
newtype IntT t    = IntT Int
newtype BoolT t   = BoolT Bool
newtype FloatT t  = FloatT Double
newtype UnitT t   = UnitT ()
newtype MaybeT t  = MaybeT (Maybe (ParamToData t))
data ObjectT name (params :: [Parameter Symbol Symbol]) where
  (:@.)  :: ObjectT name '[]
  (:@*) :: ParamToData t -> ObjectT name ts -> ObjectT name (t ': ts)

type ParamExample
  = '[ StringP "city" "city to query"
     , StringP "temp" "temperature"
     , ObjectP "location" "location data"
       '[ StringP "lat" "latitude"
        , StringP "lon" "longitude"
        ]
     ] :: [Parameter Symbol Symbol]

--------------------------------FromJSON/ToJSON--------------------------------

instance KnownSymbol t => FromJSON (StringT t) where
  parseJSON = fmap StringT . parseJSON

instance KnownSymbol t => ToJSON (StringT t) where
  toJSON (StringT t) = String t

instance KnownSymbol t => FromJSON (IntT t) where
  parseJSON = fmap IntT . parseJSON

instance KnownSymbol t => ToJSON (IntT t) where
  toJSON (IntT i) = Number $ fromIntegral i

instance KnownSymbol t => FromJSON (BoolT t) where
  parseJSON = fmap BoolT . parseJSON

instance KnownSymbol t => ToJSON (BoolT t) where
  toJSON (BoolT b) = Bool b

instance KnownSymbol t => FromJSON (FloatT t) where
  parseJSON = fmap FloatT . parseJSON

instance KnownSymbol t => ToJSON (FloatT t) where
  toJSON (FloatT f) = Number $ realToFrac f

instance FromJSON (UnitT t) where
  parseJSON _ = return $ UnitT ()

instance ToJSON (UnitT t) where
  toJSON _ = Null

instance FromJSON (ParamToData t) => FromJSON (MaybeT t) where
  parseJSON Null = return $ MaybeT Nothing
  parseJSON v    = fmap MaybeT $ Just <$> parseJSON v

instance FromJSON (ObjectT name '[]) where
  parseJSON _ = return (:@.)

instance (FromJSON (ObjectT name ts), FromJSON (ParamToData t), HasName t) => FromJSON (ObjectT name (t ': ts)) where
  parseJSON = withObject "ObjectT" $ \o -> do
    t <- parseJSON =<< o .: fromString (getName @t)
    ts <- parseJSON (Object o)
    return (t :@* ts)

class HasName t where
  getName :: String

instance KnownSymbol t => HasName (StringP t d)    where getName = symbolVal (Proxy @t)
instance KnownSymbol t => HasName (IntP t d)       where getName = symbolVal (Proxy @t)
instance KnownSymbol t => HasName (BoolP t d)      where getName = symbolVal (Proxy @t)
instance KnownSymbol t => HasName (FloatP t d)     where getName = symbolVal (Proxy @t)
instance KnownSymbol t => HasName (ObjectP t d ps) where getName = symbolVal (Proxy @t)
instance KnownSymbol t => HasName (MaybeP t d p)   where getName = symbolVal (Proxy @t)

--------------------------------Param Explanation--------------------------------

class ParamExplained t where
  {-# MINIMAL printParamExplanation #-}
  printParamExplanation :: State PrintState ()

  getExplanation :: Text
  getExplanation
    = T.intercalate "\n" . reverse . printStateTextRev . snd 
    $ runState (printParamExplanation @t) (PrintState 0 [])
  {-# INLINE getExplanation #-}

data PrintState = PrintState { printStateIndent :: Int, printStateTextRev :: [Text] }

{-# INLINE indented #-}
indented indent = (T.replicate (indent * indentWidth) " " <>)
  where indentWidth = 2

prints :: Text -> State PrintState ()
prints txt = do
  indent <- gets printStateIndent
  modify $ \s -> s { printStateTextRev = indented indent txt : printStateTextRev s }
{-# INLINE prints #-}

printsLines :: [Text] -> State PrintState ()
printsLines = mapM_ prints
{-# INLINE printsLines #-}

indentInc :: State PrintState ()
indentInc = modify $ \s -> s { printStateIndent = printStateIndent s + 1 }
{-# INLINE indentInc #-}

indentDec :: State PrintState ()
indentDec = modify $ \s -> s { printStateIndent = printStateIndent s - 1 }
{-# INLINE indentDec #-}

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (StringP n d) where
  printParamExplanation = prints $ mconcat
    [ toText $ symbolVal (Proxy @n)
    , ": (type string) "
    , toText $ symbolVal (Proxy @d)
    ]
  {-# INLINE printParamExplanation #-}

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (IntP n d) where
  printParamExplanation = prints $ mconcat
    [ toText $ symbolVal (Proxy @n) 
    , ": (type integer) "
    , toText $ symbolVal (Proxy @d)
    ]
  {-# INLINE printParamExplanation #-}

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (BoolP n d) where
  printParamExplanation = prints $ mconcat
    [ toText $ symbolVal (Proxy @n) 
    , ": (type boolean) "
    , toText $ symbolVal (Proxy @d)
    ]
  {-# INLINE printParamExplanation #-}

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (FloatP n d) where
  printParamExplanation = prints $ mconcat
    [ toText $ symbolVal (Proxy @n) 
    , ": (type float) "
    , toText $ symbolVal (Proxy @d)
    ]
  {-# INLINE printParamExplanation #-}

instance (KnownSymbol n, KnownSymbol d, ParamExplained ps) => ParamExplained (ObjectP n d ps) where
  printParamExplanation = do
    prints $ mconcat
      [ toText $ symbolVal (Proxy @n)
      , ": (type object) "
      , toText $ symbolVal (Proxy @d)
      ]
    indentInc
    prints "This object contains the following parameters:"
    printParamExplanation @ps
    indentDec
  {-# INLINE printParamExplanation #-}

instance (ParamExplained '[]) where
  printParamExplanation = return ()
  {-# INLINE printParamExplanation #-}

instance (ParamExplained p, ParamExplained ps) => ParamExplained (p ': ps) where
  printParamExplanation = do
    printParamExplanation @p
    printParamExplanation @ps
  {-# INLINE printParamExplanation #-}

--------------------------------Tool Class-----------------------------------

-- | Convert parameter type to data type
type family ParamToData p :: Type where
  ParamToData (StringP n _)           = StringT n
  ParamToData (IntP n _)              = IntT n
  ParamToData (BoolP n _)             = BoolT n
  ParamToData (FloatP n _)            = FloatT n
  ParamToData (ObjectP n _ '[])       = ObjectT n '[]
  ParamToData (ObjectP n _ '[p])      = ObjectT n '[p]
  ParamToData (ObjectP n _ (p ': ps)) = ObjectT n (p ': ps)

-- | A class for tools
class
  ( ParamExplained (ToolInput a)
  , ToJSON (ToolOutput a)
  , FromJSON (ToolInput a)
  , Show (ToolError a)
  ) 
  => ToolClass a where
  type ToolInput  a :: Type
  data ToolOutput a :: Type
  data ToolError  a :: Type
  toolName          :: Proxy a -> Text
  toolDescription   :: Proxy a -> Text
  toolHandler       :: Proxy a -> ToolInput a -> ExceptT (ToolError a) IO (ToolOutput a)

  -- | Default tool handler with text error, defined by toolHandler, will also catch unhandled exceptions
  toolHandlerTextError :: Proxy a -> ToolInput a -> ExceptT Text IO (ToolOutput a)
  toolHandlerTextError t i = ExceptT $ do
    try @SomeException (runExceptT (toolHandler t i)) >>= \case
      Left someEx       -> return $ Left $ "Tool Unhandled Exception: " <> toText someEx
      Right (Left err)  -> return $ Left $ "Tool Returned Error: " <> toText err
      Right (Right val) -> return $ Right val

  jsonToInput :: Proxy a -> Value -> Either Text (ToolInput a)
  jsonToInput _ = first toText . parseEither parseJSON

  toolExplain :: Proxy a -> Text
  toolExplain _ = getExplanation @(ToolInput a)

class EncodeUtf8LazyByteString a where
  encodeUtf8LBS :: a -> ByteString

instance EncodeUtf8LazyByteString TextLazy   where encodeUtf8LBS = TLE.encodeUtf8
instance EncodeUtf8LazyByteString Text       where encodeUtf8LBS = TLE.encodeUtf8 . TL.fromStrict
instance EncodeUtf8LazyByteString ByteString where encodeUtf8LBS = id

-- | The tool call function and arguments generated by agent
data ToolCallPair = ToolCallPair { toolCallName :: Text, toolCallArgs :: Value }
  deriving (Show, Eq)

instance FromJSON ToolCallPair where
  parseJSON = withObject "ToolCallPair" $ \o -> ToolCallPair
    <$> o .: "tool"
    <*> o .: "args"

-- | Parse potential tool call from message content
parseToolCall :: EncodeUtf8LazyByteString text => text -> Maybe ToolCallPair
parseToolCall txt = case eitherDecode (encodeUtf8LBS txt) of
  Right tc -> Just tc
  _        -> Nothing

-- | A unique identifier for tool call tracking
newtype ToolCallId = ToolCallId { unToolCallId :: Text }
  deriving (Show, Read, Generic, Eq, Ord)
  deriving (FromJSON, ToJSON, IsString, NFData) via Text

-- | Metadata for tool call tracking
data ToolMeta = ToolMeta
  { toolCallId      :: ToolCallId -- ^ Unique identifier for tool invocation
  , toolNameUsed    :: Text       -- ^ Name of tool being called
  , toolAttempt     :: Int        -- ^ Retry attempt counter
  } deriving (Show, Read, Generic, Eq, Ord, NFData)

instance ToJSON ToolMeta
instance FromJSON ToolMeta

---------------------------------
appendToolPrompts :: ConstraintList ToolClass ts => Proxy ts -> Text -> Text
appendToolPrompts clist sep
  = ( T.unlines 
      [ sep
      , "## Available Tools"
      , "Format tool calls as JSON with 'tool' and 'args' fields."
      , "Example: {\"tool\": \"weather\", \"args\": {\"city\": \"Paris\"}}"
      , sep
      , "### TOOLS LIST:"
      , ""
      ] 
    <>
    ) 
  . T.unlines $ useConstraint (Proxy @ToolClass) clist (\t -> toolExplain t <> sep)

-- -- | Generate system prompt with tool descriptions
-- generateSystemPrompt :: ChatParams -> Text
-- generateSystemPrompt params
--   | null (chatTools params) = basePrompt
--   | otherwise               = basePrompt <> "\n\n" <> toolPrompt
--   where
--     basePrompt = case systemMessage (chatSetting params) of
--       Just (Message _ txt _) -> txt
--       Nothing -> "You are a helpful assistant."
--     
--     toolPrompt = T.intercalate "\n"
--       [ "---"
--       , "AVAILABLE TOOLS:"
--       , "Format tool calls as JSON with 'tool' and 'args' fields."
--       , "Example: {\"tool\": \"weather\", \"args\": {\"city\": \"Paris\"}}"
--       , "---"
--       , "TOOLS LIST:"
--       , T.intercalate "\n" (map describeTool $ chatTools params)
--       ]
--     
--     describeTool tool = T.intercalate " "
--       [ "- Name:", toolName tool
--       , "\n  Description:", toolDescription tool
--       , "\n  Parameters:", describeParams (toolParameters tool)
--       ]
--     
--     describeParams = T.intercalate ", " . map paramDesc
--       where
--         paramDesc p = T.intercalate " "
--           [ paramName p
--           , "(" <> paramTypeDesc (paramType p) <> ")"
--           , "-" <> paramDesc p
--           ]
--         
--         paramTypeDesc = \case
--           StringT -> "string"
--           IntT -> "integer"
--           BoolT -> "boolean"
--           FloatT -> "float"
--           ObjectT ps -> "object {" <> T.intercalate ", " (map paramName ps) <> "}"
-- 
-- -- | Parse potential tool call from message content
-- parseToolCall :: Text -> Maybe (Text, Value)
-- parseToolCall txt = case A.eitherDecode (BL.fromStrict $ encodeUtf8 txt) of
--   Right (Object o) -> do
--     tool <- o .: "tool"
--     args <- o .: "args"
--     return (tool, args)
--   _ -> Nothing
-- 
-- -- | Handle tool execution and recursion
-- handleToolCall :: ChatParams -> [Message] -> Text -> Value -> ToolMetadata -> ExceptT Text IO Text
-- handleToolCall params prevMsgs toolName args md = do
--   -- Find matching tool
--   tool <- case find (\t -> toolName == toolName t) (chatTools params) of
--     Just t -> return t
--     Nothing -> throwE $ "Unknown tool: " <> toolName
--   
--   -- Execute tool
--   result <- liftIO (toolHandler tool args) >>= \case
--     ToolSuccess val -> return val
--     ToolError err -> throwE $ "Tool error: " <> err
--   
--   -- Build updated message history
--   let toolMsg = Message
--         { role = "tool"
--         , content = case result of
--             String t -> t
--             _ -> decodeUtf8 (BL.toStrict $ encode result)
--         , toolMetadata = Just md
--             { toolCallId = T.pack (show (hash prevMsgs))  -- Simple ID generation
--             , toolAttempt = toolAttempt md + 1
--             }
--         }
--   
--   -- Recursive call with updated params and history
--   messageChat params { maxToolDepth = maxToolDepth params - 1 } 
--     (prevMsgs ++ [toolMsg])
-- 
-- -- | Smart constructor for chat requests with tool-aware prompts
-- generateRequestBody :: ChatParams -> [Message] -> BL.ByteString
-- generateRequestBody params msgs = encode $
--   ChatRequest
--     { model = modelString (chatModel params)
--     , messages = systemMessage : msgs
--     , temperature = fromMaybe 0.5 (systemTemp $ chatSetting params)
--     , stream = Just False
--     }
--   where
--     systemMessage = Message "system" (generateSystemPrompt params) Nothing
