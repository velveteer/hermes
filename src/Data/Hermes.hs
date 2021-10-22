{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hermes
  ( decode
  , decodeWith
  , bool
  , char
  , double
  , int
  , scientific
  , string
  , text
  , isNull
  , mkHermesEnv
  , mkHermesEnv_
  , atKey
  , atOptionalKey
  , atOrderedKey
  , list
  , nullable
  , withArray
  , withBool
  , withDouble
  , withInt
  , withObject
  , withString
  , withText
  , Decoder
  , HermesEnv
  , HermesException(..)
  , HError(..)
  , Value
  , Object
  , Array
  , ArrayIter
  , SIMDParser
  , SIMDDocument
  , PaddedString
  ) where

-- import           Debug.Trace

import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.IO.Unlift (withRunInIO)
import           Control.Monad.Trans.Reader (ReaderT(..), asks, local, runReaderT)
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import           Data.ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.DList as DList
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import           UnliftIO.Exception
import           UnliftIO.Foreign hiding (allocaArray, withArray)

-- Opaque SIMD Types
data SIMDParser
data SIMDDocument
data PaddedString
type ErrPtr = Ptr CInt

-- Opaque JSON Types
data JSONValue
data JSONObject
data JSONArray
data JSONArrayIter

-- Constructor/destructors
foreign import ccall unsafe "parser_init" parserInit
  :: CSize -> IO (Ptr SIMDParser)

foreign import ccall unsafe "&parser_destroy" parserDestroy
  :: FunPtr (Ptr SIMDParser -> IO ())

foreign import ccall unsafe "make_document" makeDocumentImpl
  :: IO (Ptr SIMDDocument)

foreign import ccall unsafe "&delete_document" deleteDocumentImpl
  :: FunPtr (Ptr SIMDDocument -> IO ())

foreign import ccall unsafe "make_input" makeInputImpl
  :: CString -> CSize -> IO (Ptr PaddedString)

foreign import ccall unsafe "&delete_input" deleteInputImpl
  :: FunPtr (Ptr PaddedString -> IO ())

-- Document parsers
foreign import ccall unsafe "get_iterator" getIterator
  :: Parser -> InputBuffer -> Document -> ErrPtr -> IO ()

foreign import ccall unsafe "get_document_value" getDocumentValueImpl
  :: Document -> Value -> ErrPtr -> IO ()

foreign import ccall unsafe "get_object_from_value" getObjectFromValueImpl
  :: Value -> Object -> ErrPtr -> IO ()

foreign import ccall unsafe "get_array_from_value" getArrayFromValueImpl
  :: Value -> Array -> ErrPtr -> IO ()

foreign import ccall unsafe "get_array_iter" getArrayIterImpl
  :: Array -> ArrayIter -> ErrPtr -> IO ()

foreign import ccall unsafe "arr_iter_is_done" arrayIterIsDoneImpl
  :: ArrayIter -> IO CBool

foreign import ccall unsafe "arr_iter_get_current" arrayIterGetCurrentImpl
  :: ArrayIter -> Value -> ErrPtr -> IO ()

foreign import ccall unsafe "arr_iter_move_next" arrayIterMoveNextImpl
  :: ArrayIter -> IO ()

foreign import ccall unsafe "find_field_unordered" findFieldUnorderedImpl
  :: Object -> CString -> Value -> ErrPtr -> IO ()

foreign import ccall unsafe "find_field" findFieldImpl
  :: Object -> CString -> Value -> ErrPtr -> IO ()

-- Helpers
foreign import ccall unsafe "get_error_message" getErrorMessageImpl
  :: ErrPtr -> IO CString

foreign import ccall unsafe "current_location" currentLocationImpl
  :: Document -> Ptr CString -> ErrPtr -> IO CString

foreign import ccall unsafe "to_debug_string" toDebugStringImpl
  :: Document -> Ptr CString -> IO CString

foreign import ccall unsafe "is_null" isNullImpl
  :: Value -> IO CBool

-- Primitives
foreign import ccall unsafe "get_int" getIntImpl
  :: Value -> Ptr CInt -> ErrPtr -> IO ()

foreign import ccall unsafe "get_double" getDoubleImpl
  :: Value -> Ptr CDouble -> ErrPtr -> IO ()

foreign import ccall unsafe "get_string" getStringImpl
  :: Value -> Ptr CString -> Ptr CSize -> ErrPtr -> IO ()

foreign import ccall unsafe "get_bool" getBoolImpl
  :: Value -> Ptr CBool -> ErrPtr -> IO ()

foreign import ccall unsafe "get_raw_json_token" getRawJSONTokenImpl
  :: Value -> Ptr CString -> Ptr CSize -> ErrPtr -> IO ()

data HermesException =
    SIMDException HError
  | InternalException HError
  deriving (Show, Exception)

data HError =
  HError
    { hPtr        :: !String
    -- ^ The path to the current element determined by the decoder.
    , errorMsg    :: !String
    -- ^ An error message.
    , docLocation :: !String
    -- ^ Truncated location of the simdjson document iterator.
    , docDebug    :: !String
    -- ^ Debug information from simdjson::document.
    } deriving Show

throwSIMD :: String -> String -> String -> String -> IO a
throwSIMD ptr msg loc debug =
  throwIO . SIMDException $ HError ptr msg loc debug

throwInternal :: String -> Decoder a
throwInternal msg = withRunInIO $ \run -> do
  docPtr <- run $ asks hDocument
  pointer <- run $ asks hPointer
  alloca $ \errPtr -> run $
    alloca $ \strPtr -> do
      locStr <- peekCString =<< liftIO (currentLocationImpl docPtr strPtr errPtr)
      debugStr <- peekCString =<< liftIO (toDebugStringImpl docPtr strPtr)
      liftIO
        . throwIO
        . InternalException
        $ HError pointer msg (Prelude.take 100 locStr) debugStr

data SIMDErrorCode =
    SUCCESS
  | CAPACITY
  | MEMALLOC
  | TAPE_ERROR
  | DEPTH_ERROR
  | STRING_ERROR
  | T_ATOM_ERROR
  | F_ATOM_ERROR
  | N_ATOM_ERROR
  | NUMBER_ERROR
  | UTF8_ERROR
  | UNINITIALIZED
  | EMPTY
  | UNESCAPED_CHARS
  | UNCLOSED_STRING
  | UNSUPPORTED_ARCHITECTURE
  | INCORRECT_TYPE
  | NUMBER_OUT_OF_RANGE
  | INDEX_OUT_OF_BOUNDS
  | NO_SUCH_FIELD
  | IO_ERROR
  | INVALID_JSON_POINTER
  | INVALID_URI_FRAGMENT
  | UNEXPECTED_ERROR
  | PARSER_IN_USE
  | OUT_OF_ORDER_ITERATION
  | INSUFFICIENT_PADDING
  | INCOMPLETE_ARRAY_OR_OBJECT
  | SCALAR_DOCUMENT_AS_VALUE
  | OUT_OF_BOUNDS
  | NUM_ERROR_CODES
  deriving (Eq, Show, Bounded, Enum)

allocaValue :: (Value -> Decoder a) -> Decoder a
allocaValue f = allocaBytes 24 $ \val -> f (Value val)

allocaObject :: (Object -> Decoder a) -> Decoder a
allocaObject f = allocaBytes 24 $ \objPtr -> f (Object objPtr)

allocaArray :: (Array -> Decoder a) -> Decoder a
allocaArray f = allocaBytes 24 $ \arr -> f (Array arr)

allocaArrayIter :: (ArrayIter -> Decoder a) -> Decoder a
allocaArrayIter f = allocaBytes 24 $ \iter -> f (ArrayIter iter)

handleError :: ErrPtr -> Decoder ()
handleError errPtr = do
  docPtr <- asks hDocument
  pointer <- asks hPointer
  errCode <- toEnum . fromEnum <$> liftIO (peek errPtr)
  if errCode == SUCCESS
  then pure ()
  else liftIO $ do
    errStr <- peekCString =<< getErrorMessageImpl errPtr
    alloca $ \strPtr -> do
      locStr <- peekCString =<< currentLocationImpl docPtr strPtr errPtr
      debugStr <- peekCString =<< toDebugStringImpl docPtr strPtr
      throwSIMD pointer errStr (Prelude.take 100 locStr) debugStr

-- | A reference to an opaque simdjson::ondemand::parser.
newtype Parser = Parser (Ptr SIMDParser)

-- | A reference to an opaque simdjson::ondemand::document.
newtype Document = Document (Ptr SIMDDocument)

-- | A reference to an opaque simdjson::padded_string.
newtype InputBuffer = InputBuffer (Ptr PaddedString)

-- | A reference to an opaque simdjson::ondemand::value.
newtype Value = Value (Ptr JSONValue)

-- | A reference to an opaque simdjson::ondemand::object.
newtype Object = Object (Ptr JSONObject)

-- | A reference to an opaque simdjson::ondemand::array.
newtype Array = Array (Ptr JSONArray)

-- | A reference to an opaque simdjson::ondemand::array_iterator.
newtype ArrayIter = ArrayIter (Ptr JSONArrayIter)

type Decoder a = ReaderT HermesState IO a

data HermesState =
  HermesState
    { hDocument :: !Document
    , hPointer  :: !String
    }

mkHermesState :: Document -> HermesState
mkHermesState doc = HermesState doc mempty

withDocument :: (Value -> Decoder a) -> Parser -> Document -> InputBuffer -> Decoder a
withDocument f parserPtr docPtr inputPtr =
  allocaValue $ \valPtr -> withRunInIO $ \run ->
  alloca $ \errPtr -> run $ do
    -- traceM "getIterator"
    liftIO $ getIterator parserPtr inputPtr docPtr errPtr
    handleError errPtr
    -- traceM "getDocumentValue"
    liftIO $ getDocumentValueImpl docPtr valPtr errPtr
    handleError errPtr
    f valPtr

withObject :: (Object -> Decoder a) -> Value -> Decoder a
withObject f valPtr =
  allocaObject $ \oPtr -> withRunInIO $ \run ->
  alloca $ \errPtr -> run $ do
    -- traceM "withObject"
    liftIO $ getObjectFromValueImpl valPtr oPtr errPtr
    handleError errPtr
    f oPtr
{-# INLINE withObject #-}

withUnorderedField :: (Value -> Decoder a) -> Object -> String -> Decoder a
withUnorderedField f objPtr key = withRunInIO $ \run ->
  withCString key $ \cstr -> run $
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> withPath key $ do
    -- traceM $ "withUnorderedField " <> key
    liftIO $ findFieldUnorderedImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr
{-# INLINE withUnorderedField #-}

withUnorderedOptionalField :: (Value -> Decoder a) -> Object -> String -> Decoder (Maybe a)
withUnorderedOptionalField f objPtr key = withRunInIO $ \run ->
  withCString key $ \cstr -> run $
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> withPath key $ do
    -- traceM $ "withUnorderedOptionalField " <> key
    liftIO $ findFieldUnorderedImpl objPtr cstr vPtr errPtr
    errCode <- toEnum . fromEnum <$> (liftIO $ peek errPtr)
    if | errCode == SUCCESS       -> Just <$> f vPtr
       | errCode == NO_SUCH_FIELD -> pure Nothing
       | otherwise                -> Nothing <$ handleError errPtr
{-# INLINE withUnorderedOptionalField #-}

withPath :: String -> Decoder a -> Decoder a
withPath key = local (\st -> st { hPointer = hPointer st <> "." <> key })
{-# INLINE withPath #-}

withField :: (Value -> Decoder a) -> Object -> String -> Decoder a
withField f objPtr key = withRunInIO $ \run ->
  withCString key $ \cstr -> run $
  allocaValue $ \vPtr ->
    alloca $ \errPtr -> withPath key $ do
    -- traceM $ "withField " <> key
    liftIO $ findFieldImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr
{-# INLINE withField #-}

getInt :: Value -> Decoder Int
getInt valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    -- traceM "getInt"
    liftIO $ getIntImpl valPtr ptr errPtr
    handleError errPtr
    fmap fromEnum . liftIO $ peek ptr
{-# INLINE getInt #-}

withInt :: (Int -> Decoder a) -> Value -> Decoder a
withInt f = getInt >=> f
{-# INLINE withInt #-}

getDouble :: Value -> Decoder Double
getDouble valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    -- traceM "getDouble"
    liftIO $ getDoubleImpl valPtr ptr errPtr
    handleError errPtr
    fmap realToFrac . liftIO $ peek ptr
{-# INLINE getDouble #-}

withDouble :: (Double -> Decoder a) -> Value -> Decoder a
withDouble f = getDouble >=> f
{-# INLINE withDouble #-}

scientific :: Value -> Decoder Sci.Scientific
scientific = withRawByteString parseScientific
{-# INLINE scientific #-}

parseScientific :: BC.ByteString -> Decoder Sci.Scientific
parseScientific
  = either (\err -> throwInternal $ "failed to parse Scientific: " <> err) pure
  . A.parseOnly (A.scientific <* A.endOfInput)
{-# INLINE parseScientific #-}

getBool :: Value -> Decoder Bool
getBool valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    -- traceM "getBool"
    liftIO $ getBoolImpl valPtr ptr errPtr
    handleError errPtr
    fmap toBool . liftIO $ peek ptr
{-# INLINE getBool #-}

withBool :: (Bool -> Decoder a) -> Value -> Decoder a
withBool f = getBool >=> f
{-# INLINE withBool #-}

fromCStringLen :: (CStringLen -> Decoder a) -> Value -> Decoder a
fromCStringLen f valPtr = withRunInIO $ \run -> mask_ $
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $
  alloca $ \errPtr -> do
    liftIO $ getStringImpl valPtr strPtr lenPtr errPtr
    handleError errPtr
    len <- fmap fromEnum . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    result <- f (str, len)
    pure result
{-# INLINE fromCStringLen #-}

getString :: Value -> Decoder String
getString = fromCStringLen (liftIO . peekCStringLen)
{-# INLINE getString #-}

getText :: Value -> Decoder Text
getText = fromCStringLen (liftIO . T.peekCStringLen)
{-# INLINE getText #-}

getRawByteString :: Value -> Decoder BC.ByteString
getRawByteString valPtr = withRunInIO $ \run -> mask_ $
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $
  alloca $ \errPtr -> do
    liftIO $ getRawJSONTokenImpl valPtr strPtr lenPtr errPtr
    handleError errPtr
    len <- fmap fromEnum . liftIO $ peek lenPtr
    str' <- liftIO $ peek strPtr
    result <- liftIO $ BC.packCStringLen (str', len)
    pure result
{-# INLINE getRawByteString #-}

withRawByteString :: (BC.ByteString -> Decoder a) -> Value -> Decoder a
withRawByteString f = getRawByteString >=> f
{-# INLINE withRawByteString #-}

withString :: (String -> Decoder a) -> Value -> Decoder a
withString f = getString >=> f
{-# INLINE withString #-}

withText :: (Text -> Decoder a) -> Value -> Decoder a
withText f = getText >=> f
{-# INLINE withText #-}

isNull :: Value -> Decoder Bool
isNull valPtr = fmap toBool . liftIO $ isNullImpl valPtr
{-# INLINE isNull #-}

withArray :: (Array -> Decoder a) -> Value -> Decoder a
withArray f val = withRunInIO $ \run ->
  alloca $ \errPtr -> run $
  allocaArray $ \arrPtr -> do
    -- traceM "withArray"
    liftIO $ getArrayFromValueImpl val arrPtr errPtr
    handleError errPtr
    f arrPtr
{-# INLINE withArray #-}

withArrayIter :: (ArrayIter -> Decoder a) -> Array -> Decoder a
withArrayIter f arrPtr = withRunInIO $ \run ->
  alloca $ \errPtr -> run $
  allocaArrayIter $ \iterPtr -> do
    -- traceM "withArrayIter"
    liftIO $ getArrayIterImpl arrPtr iterPtr errPtr
    handleError errPtr
    f iterPtr
{-# INLINE withArrayIter #-}

iterateOverArray :: (Value -> Decoder a) -> ArrayIter -> Decoder [a]
iterateOverArray f iterPtr = go DList.empty
  where
    go acc = withRunInIO $ \runInIO -> do
      -- traceM "arrIterIsDone"
      isOver <- fmap toBool $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then
          alloca $ \errPtr -> runInIO $
          allocaValue $ \valPtr -> do
            -- traceM "arrayIterGetCurrent"
            liftIO $ arrayIterGetCurrentImpl iterPtr valPtr errPtr
            handleError errPtr
            result <- f valPtr
            -- traceM "arrayIterMoveNext"
            liftIO $ arrayIterMoveNextImpl iterPtr
            go (acc <> DList.singleton result)
        else
          pure $ DList.toList acc
{-# INLINE iterateOverArray #-}

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
atKey :: String -> (Value -> Decoder a) -> Object -> Decoder a
atKey key parser obj = withUnorderedField parser obj key
{-# INLINE atKey #-}

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atOptionalKey :: String -> (Value -> Decoder a) -> Object -> Decoder (Maybe a)
atOptionalKey key parser obj = withUnorderedOptionalField parser obj key
{-# INLINE atOptionalKey #-}

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atOrderedKey :: String -> (Value -> Decoder a) -> Object -> Decoder a
atOrderedKey key parser obj = withField parser obj key
{-# INLINE atOrderedKey #-}

-- | Parse a homogenous JSON array into a Haskell list.
list :: (Value -> Decoder a) -> Value -> Decoder [a]
list f val =
  flip withArray val $ \arr ->
  flip withArrayIter arr $ iterateOverArray f
{-# INLINE list #-}

-- | Transforms a parser to return Nothing when the value is null.
nullable :: (Value -> Decoder a) -> Value -> Decoder (Maybe a)
nullable parser val = do
  nil <- isNull val
  if nil
    then pure Nothing
    else Just <$> parser val
{-# INLINE nullable #-}

-- | Parse only a single character. TODO Unbounded versus bounded?
char :: Value -> Decoder Char
char = getText >=> justOne
  where
    justOne txt =
      case T.uncons txt of
        Just (c, "") ->
          pure c
        _ ->
          throwInternal "expected a single character"
{-# INLINE char #-}

-- | Parse a JSON string into a Haskell String.
string :: Value -> Decoder String
string = getString
{-# INLINE string #-}

-- | Parse a JSON string into Haskell Text.
text :: Value -> Decoder Text
text = getText
{-# INLINE text #-}

-- | Parse a JSON number into an unsigned Haskell Int.
int :: Value -> Decoder Int
int = getInt
{-# INLINE int #-}

-- | Parse a JSON boolean into a Haskell Bool.
bool :: Value -> Decoder Bool
bool = getBool
{-# INLINE bool #-}

-- | Parse a JSON number into a Haskell Double.
double :: Value -> Decoder Double
double = getDouble
{-# INLINE double #-}

-- Decoding Functions

data HermesEnv =
  HermesEnv
    { simdParser :: !(ForeignPtr SIMDParser)
    , simdDoc    :: !(ForeignPtr SIMDDocument)
    }

-- | Make a new HermesEnv. This allocates foreign references to
-- a simdjson::ondemand::parser and a simdjson::ondemand::document.
-- The instances are re-used on successive decodes via `decodeWith`.
-- The optional capacity argument sets the max capacity in bytes for the
-- simdjson::ondemand::parser, which defaults to 4GB.
mkHermesEnv :: Maybe Int -> IO HermesEnv
mkHermesEnv mCapacity = do
  parser   <- mkSIMDParser mCapacity
  document <- mkSIMDDocument
  pure HermesEnv
    { simdParser = parser
    , simdDoc    = document
    }

-- | Shortcut for constructing a default `HermesEnv`.
mkHermesEnv_ :: IO HermesEnv
mkHermesEnv_ = mkHermesEnv Nothing

mkSIMDParser :: Maybe Int -> IO (ForeignPtr SIMDParser)
mkSIMDParser mCap = mask_ $ do
  let maxCap = 4000000000;
  ptr <- parserInit . toEnum $ fromMaybe maxCap mCap
  newForeignPtr parserDestroy ptr

mkSIMDDocument :: IO (ForeignPtr SIMDDocument)
mkSIMDDocument = mask_ $ do
  ptr <- makeDocumentImpl
  newForeignPtr deleteDocumentImpl ptr

mkSIMDPaddedStr :: ByteString -> IO (ForeignPtr PaddedString)
mkSIMDPaddedStr input = mask_ $ useAsCStringLen input $ \(cstr, len) -> do
  ptr <- makeInputImpl cstr (toEnum len)
  newForeignPtr deleteInputImpl ptr

-- | Construct an ephemeral `HermesEnv` and use it to decode the input.
-- The simdjson instances will be out of scope when decode returns, which
-- means the garbage collector will/should run their finalizers.
-- This is convenient for users who do not need to hold onto a `HermesEnv` for
-- a long-running single-threaded process. There is a small performance penalty
-- for creating the simdjson instances on each decode.
decode :: ByteString -> (Value -> Decoder a) -> IO a
decode bs p = do
  parser   <- mkSIMDParser Nothing
  document <- mkSIMDDocument
  input    <- mkSIMDPaddedStr bs
  withForeignPtr parser $ \parserPtr ->
    withForeignPtr document $ \docPtr ->
      withForeignPtr input $ \inputPtr ->
      flip runReaderT (mkHermesState (Document docPtr)) $ withDocument p
          (Parser parserPtr) (Document docPtr) (InputBuffer inputPtr)

-- | Decode with a caller-provided `HermesEnv`. If the caller retains a reference to
-- the `HermesEnv` then the simdjson instance finalizers will not be run.
-- This is useful for long-running applications that want to re-use simdjson instances
-- for optimal performance.
-- Do NOT share a `HermesEnv` across multiple threads, but it is fine for each thread
-- to have its own.
decodeWith :: HermesEnv -> ByteString -> (Value -> Decoder a) -> IO a
decodeWith env bs parser =
  withForeignPtr (simdParser env) $ \parserPtr ->
  withForeignPtr (simdDoc env) $ \docPtr -> do
    paddedStr <- mkSIMDPaddedStr bs
    withForeignPtr paddedStr $ \paddedStrPtr ->
      flip runReaderT (mkHermesState (Document docPtr)) $ withDocument parser
        (Parser parserPtr) (Document docPtr) (InputBuffer paddedStrPtr)
