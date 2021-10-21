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
  , HermesEnv
  , Value
  , Object
  , Array
  , ArrayIter
  , SIMDParser
  , SIMDDocument
  , PaddedString
  ) where

-- import           Debug.Trace

import           Control.Exception (Exception, mask_, throwIO)
import           Control.Monad ((>=>))
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import           Data.ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.DList as DList
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

-- Opaque SIMD Types
data SIMDParser
data SIMDDocument
data PaddedString
data PaddedStringView
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

foreign import ccall unsafe "make_input_view" makeInputViewImpl
  :: CString -> CSize -> CSize -> IO (Ptr PaddedStringView)

foreign import ccall unsafe "&delete_input_view" deleteInputViewImpl
  :: FunPtr (Ptr PaddedStringView -> IO ())

-- Document parsers
foreign import ccall unsafe "get_iterator" getIterator
  :: Parser -> InputBuffer -> Document -> ErrPtr -> IO ()

foreign import ccall unsafe "get_iterator_from_view" getIteratorFromViewImpl
  :: Parser -> InputView -> Document -> ErrPtr -> IO ()

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

data SIMDException = SIMDException String
  deriving (Show, Exception)

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

allocaValue :: (Value -> IO a) -> IO a
allocaValue f = allocaBytes 24 $ \val -> f (Value val)

allocaObject :: (Object -> IO a) -> IO a
allocaObject f = allocaBytes 24 $ \objPtr -> f (Object objPtr)

allocaArray :: (Array -> IO a) -> IO a
allocaArray f = allocaBytes 24 $ \arr -> f (Array arr)

allocaArrayIter :: (ArrayIter -> IO a) -> IO a
allocaArrayIter f = allocaBytes 24 $ \iter -> f (ArrayIter iter)

handleError :: ErrPtr -> IO ()
handleError errPtr = do
  errCode <- toEnum . fromEnum <$> peek errPtr
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- peekCString =<< getErrorMessageImpl errPtr
    throwIO . SIMDException $ errStr

-- | A reference to an opaque simdjson::ondemand::parser.
newtype Parser = Parser (Ptr SIMDParser)

-- | A reference to an opaque simdjson::ondemand::document.
newtype Document = Document (Ptr SIMDDocument)

-- | A reference to an opaque simdjson::padded_string.
newtype InputBuffer = InputBuffer (Ptr PaddedString)

-- | A reference to an opaque simdjson::padded_string_view.
newtype InputView = InputView (Ptr PaddedStringView)

-- | A reference to an opaque simdjson::ondemand::value.
newtype Value = Value (Ptr JSONValue)

-- | A reference to an opaque simdjson::ondemand::object.
newtype Object = Object (Ptr JSONObject)

-- | A reference to an opaque simdjson::ondemand::array.
newtype Array = Array (Ptr JSONArray)

-- | A reference to an opaque simdjson::ondemand::array_iterator.
newtype ArrayIter = ArrayIter (Ptr JSONArrayIter)

withDocument :: (Value -> IO a) -> Parser -> Document -> InputBuffer -> IO a
withDocument f parserPtr docPtr inputPtr =
  allocaValue $ \valPtr ->
  alloca $ \errPtr -> do
    -- traceM "getIterator"
    getIterator parserPtr inputPtr docPtr errPtr
    handleError errPtr
    -- traceM "getDocumentValue"
    getDocumentValueImpl docPtr valPtr errPtr
    handleError errPtr
    f valPtr

_withDocumentView :: (Value -> IO a) -> Parser -> Document -> InputView -> IO a
_withDocumentView f parserPtr docPtr inputPtr =
  allocaValue $ \valPtr ->
  alloca $ \errPtr -> do
    -- traceM "getIterator"
    getIteratorFromViewImpl parserPtr inputPtr docPtr errPtr
    handleError errPtr
    -- traceM "getDocumentValue"
    getDocumentValueImpl docPtr valPtr errPtr
    handleError errPtr
    f valPtr

withObject :: (Object -> IO a) -> Value -> IO a
withObject f valPtr =
  allocaObject $ \oPtr ->
  alloca $ \errPtr -> do
    -- traceM "withObject"
    getObjectFromValueImpl valPtr oPtr errPtr
    handleError errPtr
    f oPtr
{-# INLINE withObject #-}

withUnorderedField :: (Value -> IO a) -> Object -> String -> IO a
withUnorderedField f objPtr key =
  withCString key $ \cstr ->
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM $ "withUnorderedField " <> key
    findFieldUnorderedImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr
{-# INLINE withUnorderedField #-}

withUnorderedOptionalField :: (Value -> IO a) -> Object -> String -> IO (Maybe a)
withUnorderedOptionalField f objPtr key =
  withCString key $ \cstr ->
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM $ "withUnorderedOptionalField " <> key
    findFieldUnorderedImpl objPtr cstr vPtr errPtr
    errCode <- toEnum . fromEnum <$> peek errPtr
    if | errCode == SUCCESS       -> Just <$> f vPtr
       | errCode == NO_SUCH_FIELD -> pure Nothing
       | otherwise                -> Nothing <$ handleError errPtr
{-# INLINE withUnorderedOptionalField #-}

withField :: (Value -> IO a) -> Object -> String -> IO a
withField f objPtr key =
  withCString key $ \cstr ->
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM $ "withField " <> key
    findFieldImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr
{-# INLINE withField #-}

getInt :: Value -> IO Int
getInt valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getInt"
    getIntImpl valPtr ptr errPtr
    handleError errPtr
    fromEnum <$> peek ptr
{-# INLINE getInt #-}

withInt :: (Int -> IO a) -> Value -> IO a
withInt f = getInt >=> f
{-# INLINE withInt #-}

getDouble :: Value -> IO Double
getDouble valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getDouble"
    getDoubleImpl valPtr ptr errPtr
    handleError errPtr
    realToFrac <$> peek ptr
{-# INLINE getDouble #-}

withDouble :: (Double -> IO a) -> Value -> IO a
withDouble f = getDouble >=> f
{-# INLINE withDouble #-}

scientific :: Value -> IO Sci.Scientific
scientific = withRawByteString parseScientific
{-# INLINE scientific #-}

parseScientific :: BC.ByteString -> IO Sci.Scientific
parseScientific
  = either fail pure
  . A.parseOnly (A.scientific <* A.endOfInput)
{-# INLINE parseScientific #-}

getBool :: Value -> IO Bool
getBool valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getBool"
    getBoolImpl valPtr ptr errPtr
    handleError errPtr
    toBool <$> peek ptr
{-# INLINE getBool #-}

withBool :: (Bool -> IO a) -> Value -> IO a
withBool f = getBool >=> f
{-# INLINE withBool #-}

fromCStringLen :: (CStringLen -> IO a) -> Value -> IO a
fromCStringLen f valPtr = mask_ $ do
  strPtr <- mallocForeignPtr
  alloca $ \lenPtr ->
    alloca $ \errPtr ->
      withForeignPtr strPtr $ \str -> do
        getStringImpl valPtr str lenPtr errPtr
        handleError errPtr
        len <- fromEnum <$> peek lenPtr
        str' <- peek str
        result <- f (str', len)
        pure result
{-# INLINE fromCStringLen #-}

getString :: Value -> IO String
getString = fromCStringLen peekCStringLen
{-# INLINE getString #-}

getText :: Value -> IO Text
getText = fromCStringLen T.peekCStringLen
{-# INLINE getText #-}

getRawByteString :: Value -> IO BC.ByteString
getRawByteString valPtr = mask_ $ do
  strPtr <- mallocForeignPtr
  alloca $ \lenPtr ->
    alloca $ \errPtr -> do
      withForeignPtr strPtr $ \str -> do
        getRawJSONTokenImpl valPtr str lenPtr errPtr
        handleError errPtr
        len <- fromEnum <$> peek lenPtr
        str' <- peek str
        result <- BC.packCStringLen (str', len)
        pure result
{-# INLINE getRawByteString #-}

withRawByteString :: (BC.ByteString -> IO a) -> Value -> IO a
withRawByteString f = getRawByteString >=> f
{-# INLINE withRawByteString #-}

withString :: (String -> IO a) -> Value -> IO a
withString f = getString >=> f
{-# INLINE withString #-}

withText :: (Text -> IO a) -> Value -> IO a
withText f = getText >=> f
{-# INLINE withText #-}

isNull :: Value -> IO Bool
isNull valPtr = toBool <$> isNullImpl valPtr
{-# INLINE isNull #-}

withArray :: (Array -> IO a) -> Value -> IO a
withArray f val =
  alloca $ \errPtr ->
  allocaArray $ \arrPtr -> do
    -- traceM "withArray"
    getArrayFromValueImpl val arrPtr errPtr
    handleError errPtr
    f arrPtr
{-# INLINE withArray #-}

withArrayIter :: (ArrayIter -> IO a) -> Array -> IO a
withArrayIter f arrPtr =
  alloca $ \errPtr ->
  allocaArrayIter $ \iterPtr -> do
    -- traceM "withArrayIter"
    getArrayIterImpl arrPtr iterPtr errPtr
    handleError errPtr
    f iterPtr
{-# INLINE withArrayIter #-}

iterateOverArray :: (Value -> IO a) -> ArrayIter -> IO [a]
iterateOverArray f iterPtr = go DList.empty
  where
    go acc = do
      -- traceM "arrIterIsDone"
      isOver <- toBool <$> arrayIterIsDoneImpl iterPtr
      if not isOver
        then
          alloca $ \errPtr ->
          allocaValue $ \valPtr -> do
            -- traceM "arrayIterGetCurrent"
            arrayIterGetCurrentImpl iterPtr valPtr errPtr
            handleError errPtr
            result <- f valPtr
            -- traceM "arrayIterMoveNext"
            arrayIterMoveNextImpl iterPtr
            go (acc <> DList.singleton result)
        else
          pure $ DList.toList acc
{-# INLINE iterateOverArray #-}

-- Public Interface

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
atKey :: String -> (Value -> IO a) -> Object -> IO a
atKey key parser obj = withUnorderedField parser obj key
{-# INLINE atKey #-}

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atOptionalKey :: String -> (Value -> IO a) -> Object -> IO (Maybe a)
atOptionalKey key parser obj = withUnorderedOptionalField parser obj key
{-# INLINE atOptionalKey #-}

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atOrderedKey :: String -> (Value -> IO a) -> Object -> IO a
atOrderedKey key parser obj = withField parser obj key
{-# INLINE atOrderedKey #-}

-- | Parse a homogenous JSON array into a Haskell list.
list :: (Value -> IO a) -> Value -> IO [a]
list f val =
  flip withArray val $ \arr ->
  flip withArrayIter arr $ iterateOverArray f
{-# INLINE list #-}

-- | Transforms a parser to return Nothing when the value is null.
nullable :: (Value -> IO a) -> Value -> IO (Maybe a)
nullable parser val = do
  nil <- isNull val
  if nil
    then pure Nothing
    else Just <$> parser val
{-# INLINE nullable #-}

-- | Parse only a single character. TODO Unbounded versus bounded?
char :: Value -> IO Char
char = getText >=> justOne
  where
    justOne txt =
      case T.uncons txt of
        Just (c, "") -> pure c
        _            -> throwIO $ SIMDException "expected a single character"
{-# INLINE char #-}

-- | Parse a JSON string into a Haskell String.
string :: Value -> IO String
string = getString
{-# INLINE string #-}

-- | Parse a JSON string into Haskell Text.
text :: Value -> IO Text
text = getText
{-# INLINE text #-}

-- | Parse a JSON number into an unsigned Haskell Int.
int :: Value -> IO Int
int = getInt
{-# INLINE int #-}

-- | Parse a JSON boolean into a Haskell Bool.
bool :: Value -> IO Bool
bool = getBool
{-# INLINE bool #-}

-- | Parse a JSON number into a Haskell Double.
double :: Value -> IO Double
double = getDouble
{-# INLINE double #-}

-- Decoding Functions

data HermesEnv =
  HermesEnv
    { simdParser :: ForeignPtr SIMDParser
    , simdDoc    :: ForeignPtr SIMDDocument
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

_mkSIMDPaddedStrView :: CString -> Int -> Int -> IO (ForeignPtr PaddedStringView)
_mkSIMDPaddedStrView buf len cap = mask_ $ do
  ptr <- makeInputViewImpl buf (toEnum len) (toEnum cap)
  newForeignPtr deleteInputViewImpl ptr

_mkSIMDJSONBuffer :: Int -> IO (ForeignPtr CString)
_mkSIMDJSONBuffer = mask_ . mallocForeignPtrBytes

-- | Construct an ephemeral `HermesEnv` and use it to decode the input.
-- The simdjson instances will be out of scope when decode returns, which
-- means the garbage collector will/should run their finalizers.
-- This is convenient for users who do not need to hold onto a `HermesEnv` for
-- a long-running single-threaded process. There is a small performance penalty
-- for creating the simdjson instances on each decode.
decode :: ByteString -> (Value -> IO a) -> IO a
decode bs p = do
  parser   <- mkSIMDParser Nothing
  document <- mkSIMDDocument
  input    <- mkSIMDPaddedStr bs
  withForeignPtr parser $ \parserPtr ->
    withForeignPtr document $ \docPtr ->
      withForeignPtr input $ \inputPtr ->
        withDocument p
          (Parser parserPtr) (Document docPtr) (InputBuffer inputPtr)

-- | Decode with a caller-provided `HermesEnv`. If the caller retains a reference to
-- the `HermesEnv` then the simdjson instance finalizers will not be run.
-- This is useful for long-running applications that want to re-use simdjson instances
-- for optimal performance.
-- Do NOT share a `HermesEnv` across multiple threads, but it is fine for each thread
-- to have its own.
decodeWith :: HermesEnv -> ByteString -> (Value -> IO a) -> IO a
decodeWith env bs parser =
  withForeignPtr (simdParser env) $ \parserPtr ->
  withForeignPtr (simdDoc env) $ \docPtr -> do
    paddedStr <- mkSIMDPaddedStr bs
    withForeignPtr paddedStr $ \paddedStrPtr ->
      withDocument parser
        (Parser parserPtr) (Document docPtr) (InputBuffer paddedStrPtr)
