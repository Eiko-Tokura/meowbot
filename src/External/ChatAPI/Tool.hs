{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE DerivingVia, DeriveAnyClass #-}

{-|
Module      : External.ChatAPI
Description : 
    LLM chat tool call interface, type level!
    automatically generate tool call interface from type level definitions!
    extremely cool!

    currently geenrating prompt for tools list, and tool call parsing, for generic llm to use
    will add openai tool call generation later
-}
module External.ChatAPI.Tool where

-- ... existing imports ...
import Control.Monad.State
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.Kind
import Data.HList
import Data.Proxy
import Data.String (IsString, fromString)
import GHC.TypeLits
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Control.Monad.Trans.Except
import Control.DeepSeq (NFData)
import Control.Exception
import Utils.Text

type TextLazy = TL.Text

data Parameter name description
  = StringP name description
  | IntP    name description
  | BoolP   name description
  | FloatP  name description
  | ObjectP name description [Parameter Symbol Symbol]
  | MaybeP  name description (Parameter Symbol Symbol)
  | ObjectP0 [Parameter Symbol Symbol] -- ^ outermost object type, no name

newtype StringT n e = StringT Text
newtype IntT    n e = IntT Int
newtype BoolT   n e = BoolT Bool
newtype FloatT  n e = FloatT Double
newtype UnitT   n e = UnitT ()
newtype MaybeTy t = MaybeTy (Maybe (ParamToData t)) -- ^ Maybe type wrapper, called MaybeTy to avoid conflict with MaybeT transformer

data ObjectT name exp (params :: [Parameter Symbol Symbol]) where
  ObjTNil  :: ObjectT name exp '[]
  (:@*)    :: ParamToData t -> ObjectT name exp ts -> ObjectT name exp (t ': ts)
infixr 5 :@*

data ObjectT0 (params :: [Parameter Symbol Symbol]) where
  ObjT0Nil :: ObjectT0 '[]
  (:%*)    :: ParamToData t -> ObjectT0 ts -> ObjectT0 (t ': ts)
infixr 5 :%*

type ParamExample
  = '[ StringP "city" "city to query"
     , StringP "temp" "temperature"
     , ObjectP "location" "location data"
       '[ StringP "lat" "latitude"
        , StringP "lon" "longitude"
        ]
     ] :: [Parameter Symbol Symbol]

--------------------------------FromJSON/ToJSON--------------------------------

instance (KnownSymbol t, KnownSymbol e) => FromJSON (StringT t e) where
  parseJSON = fmap StringT . parseJSON

instance (KnownSymbol t, KnownSymbol e) => ToJSON (StringT t e) where
  toJSON (StringT t) = String t

instance (KnownSymbol t, KnownSymbol e) => FromJSON (IntT t e) where
  parseJSON = fmap IntT . parseJSON

instance (KnownSymbol t, KnownSymbol e) => ToJSON (IntT t e) where
  toJSON (IntT i) = Number $ fromIntegral i

instance (KnownSymbol t, KnownSymbol e) => FromJSON (BoolT t e) where
  parseJSON = fmap BoolT . parseJSON

instance (KnownSymbol t, KnownSymbol e) => ToJSON (BoolT t e) where
  toJSON (BoolT b) = Bool b

instance (KnownSymbol t, KnownSymbol e) => FromJSON (FloatT t e) where
  parseJSON = fmap FloatT . parseJSON

instance (KnownSymbol t, KnownSymbol e) => ToJSON (FloatT t e) where
  toJSON (FloatT f) = Number $ realToFrac f

instance FromJSON (UnitT t e) where
  parseJSON _ = return $ UnitT ()

instance ToJSON (UnitT t e) where
  toJSON _ = Null

instance FromJSON (ParamToData t) => FromJSON (MaybeTy t) where
  parseJSON Null = return $ MaybeTy Nothing
  parseJSON v    = fmap MaybeTy $ Just <$> parseJSON v

instance FromJSON (ObjectT name e '[]) where
  parseJSON _ = return ObjTNil

instance FromJSON (ObjectT0 '[]) where
  parseJSON _ = return ObjT0Nil

instance (FromJSON (ObjectT name e ts), FromJSON (ParamToData t), HasName t) => FromJSON (ObjectT name e (t ': ts)) where
  parseJSON = withObject "ObjectT" $ \o -> do
    t <- parseJSON =<< o .: fromString (getName @t)
    ts <- parseJSON (Object o)
    return (t :@* ts)

instance (FromJSON (ObjectT0 ts), FromJSON (ParamToData t), HasName t) => FromJSON (ObjectT0 (t ': ts)) where
  parseJSON = withObject "ObjectT0" $ \o -> do
    t <- parseJSON =<< o .: fromString (getName @t)
    ts <- parseJSON (Object o)
    return (t :%* ts)

class HasName t where
  getName :: String

instance KnownSymbol t => HasName (StringP t d)    where
  getName = symbolVal (Proxy @t)
  {-# INLINE getName #-}
instance KnownSymbol t => HasName (IntP t d)       where
  getName = symbolVal (Proxy @t)
  {-# INLINE getName #-}
instance KnownSymbol t => HasName (BoolP t d)      where
  getName = symbolVal (Proxy @t)
  {-# INLINE getName #-}
instance KnownSymbol t => HasName (FloatP t d)     where
  getName = symbolVal (Proxy @t)
  {-# INLINE getName #-}
instance KnownSymbol t => HasName (ObjectP t d ps) where
  getName = symbolVal (Proxy @t)
  {-# INLINE getName #-}
instance KnownSymbol t => HasName (MaybeP t d p)   where
  getName = symbolVal (Proxy @t)
  {-# INLINE getName #-}

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

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (StringT n d) where
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

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (IntT n d) where
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

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (BoolT n d) where
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

instance (KnownSymbol n, KnownSymbol d) => ParamExplained (FloatT n d) where
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
    prints "This object contains the following parameters:"
    indentInc
    printParamExplanation @ps
    indentDec
  {-# INLINE printParamExplanation #-}

instance (KnownSymbol n, KnownSymbol d, ParamExplained ps) => ParamExplained (ObjectT n d ps) where
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

instance (ParamExplained ps) => ParamExplained (ObjectT0 ps) where
  printParamExplanation = do
    printParamExplanation @ps
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
  ParamToData (StringP n e)   = StringT n e
  ParamToData (IntP n e)      = IntT n e
  ParamToData (BoolP n e)     = BoolT n e
  ParamToData (FloatP n e)    = FloatT n e
  ParamToData (ObjectP n e l) = ObjectT n e l
  ParamToData (ObjectP0 l)    = ObjectT0 l

-- | A class for tools
class
  ( ParamExplained (ToolInput a)
  , ToJSON (ToolOutput a)
  , FromJSON (ToolInput a)
  , Show (ToolError a)
  ) 
  => ToolClass a where
  type ToolInput  a :: Type
  type ToolOutput a :: Type
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
  toolExplain pa = "Tool Name: " <> toolName pa <> "\n" <>
                   "Description: " <> toolDescription pa <> "\n" <>
                   getExplanation @(ToolInput a)

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
parseToolCall :: (text ~ Text, EncodeUtf8LazyByteString text) => text -> Maybe ToolCallPair
parseToolCall txt = decode (encodeUtf8LBS txt) <|> (parseToolCallText txt)
  where parseToolCallText txt
          | all (`T.isInfixOf` txt) ["{\"tool\":", "\"args\":", "```json"]
             = let start = T.drop 7 $ T.dropWhile (/= '`') txt
                   mid = T.dropWhileEnd (== '`') $ start
               in decode $ encodeUtf8LBS mid
          | otherwise = Nothing

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
  | null toolExplainList = ""
  | otherwise = 
    ( T.unlines
      [ sep
      , "## Available Tools"
      , "If you need to use tool, format output as JSON with 'tool' and 'args' fields, no other text."
      , "Example: {\"tool\": \"weather\", \"args\": {\"city\": \"Paris\"}}"
      , sep
      , "### Tools List"
      , ""
      ]
    <>
    )
    . T.unlines $ toolExplainList
  where toolExplainList = useConstraint (Proxy @ToolClass) clist (\t -> toolExplain t <> sep)

--------------------------------- example tool ---------------------------------

data FibonacciTool

instance ToolClass FibonacciTool where
  type ToolInput  FibonacciTool = ParamToData (ObjectP0 '[IntP "n" "Fibonacci number to calculate, in range [0, 100000]"])
  type ToolOutput FibonacciTool = ParamToData (StringP "result" "Calculated Fibonacci number")
  data ToolError  FibonacciTool = FibonacciError Text deriving (Show)
  toolName _ = "fibonacci"
  toolDescription _ = "Calculate the nth Fibonacci number"
  toolHandler _ ((IntT n) :%* ObjT0Nil)
    | n < 0     = do
        liftIO $ putTextLn $ "[TOOL]Negative Fibonacci number " <> tshow n
        throwE $ FibonacciError "Negative Fibonacci number"
    | n > 100000 = do
        liftIO $ putTextLn $ "[TOOL]Fibonacci number " <> tshow n <> " too large"
        throwE $ FibonacciError "Fibonacci number too large"
    | otherwise = do
        liftIO $ putTextLn $ "[TOOL]Calculating Fibonacci number " <> tshow n
        liftIO $ putTextLn $ "[TOOL]The result is " <> tshow (fib n)
        return $ StringT $ toText $ fib n
    where fib :: Int -> Integer
          fib n = go n 0 1
          go 0 a _ = a
          go n a b = go (n-1) b (a+b)

-- | Check dummy input output json parsing
sanityCheckFibonacciTool :: IO ()
sanityCheckFibonacciTool = do
  let input = "{\"n\": 10}"
  let inputVal = either (error "input not work") id $ jsonToInput (Proxy @FibonacciTool) =<< first toText (eitherDecode input)
  case inputVal of
    IntT 10 :%* ObjT0Nil -> putStrLn "input ok"
    _ -> error "input not ok"
  res <- runExceptT $ toolHandlerTextError (Proxy @FibonacciTool) inputVal
  case res of
    Right v -> -- print its json
      TIO.putStrLn $ TL.toStrict $ TLE.decodeUtf8 $ encode $ toJSON v
    _ -> error "output not ok"
