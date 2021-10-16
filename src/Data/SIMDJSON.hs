{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SIMDJSON
  ( decode
  , decodeWith
  , mkSIMDDocument
  , mkSIMDPaddedStr
  , mkSIMDParser
  , withObject
  , (.:)
  , (.:>)
  , FromJSON(..)
  ) where

-- import           Debug.Trace

import           Control.Concurrent    (threadDelay)
import           Control.Exception     (Exception, mask_, throwIO, toException)
import           Control.Monad         (replicateM_, (>=>))
import qualified Data.Aeson            as Aeson
import           Data.ByteString
import           Data.Foldable         (for_)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Traversable      (for)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable

-- SIMD Types
data SIMDParser
data SIMDDocument
data PaddedString
type ErrPtr = Ptr CInt

-- JSON Types
data JSONValue
data JSONObject
data JSONArray

-- Constructor/destructors
foreign import ccall unsafe "parser_init" parserInit
  :: IO (Ptr SIMDParser)

foreign import ccall unsafe "&parser_destroy" parserDestroy
  :: FunPtr (Ptr SIMDParser -> IO ())

foreign import ccall unsafe "make_values_array" makeValuesImpl
  :: CSize -> Ptr (Ptr JSONValue) -> IO (Ptr (Ptr JSONValue))

foreign import ccall unsafe "&delete_values_array" deleteValuesImpl
  :: FunPtr (Ptr CSize -> Ptr (Ptr JSONValue) -> IO ())

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
  :: Ptr SIMDParser -> Ptr PaddedString -> Ptr SIMDDocument -> ErrPtr -> IO ()

foreign import ccall unsafe "get_document_value" getDocumentValueImpl
  :: Ptr SIMDDocument -> Ptr JSONValue -> ErrPtr -> IO ()

foreign import ccall unsafe "get_object_from_value" getObjectFromValueImpl
  :: Ptr JSONValue -> Ptr JSONObject -> ErrPtr -> IO ()

foreign import ccall unsafe "get_array_from_value" getArrayFromValueImpl
  :: Ptr JSONValue -> Ptr JSONArray -> Ptr CSize -> ErrPtr -> IO ()

foreign import ccall unsafe "get_array_elems" getArrayElemsImpl
  :: Ptr JSONArray -> Ptr (Ptr JSONValue) -> IO ()

foreign import ccall unsafe "find_field_unordered" findFieldUnorderedImpl
  :: Ptr JSONObject -> CString -> Ptr JSONValue -> ErrPtr -> IO ()

foreign import ccall unsafe "find_field" findFieldImpl
  :: Ptr JSONObject -> CString -> Ptr JSONValue -> ErrPtr -> IO ()

-- Helpers
foreign import ccall unsafe "get_raw_json_str" getRawJSONStringImpl
  :: Ptr SIMDDocument -> Ptr CString -> ErrPtr -> IO ()

foreign import ccall unsafe "get_error_message" getErrorMessageImpl
  :: ErrPtr -> IO CString

-- Primitives
foreign import ccall unsafe "get_int" getIntImpl
  :: Ptr JSONValue -> Ptr CInt -> ErrPtr -> IO CInt

foreign import ccall unsafe "get_string" getStringImpl
  :: Ptr JSONValue -> Ptr CString -> ErrPtr -> IO CString

foreign import ccall unsafe "get_bool" getBoolImpl
  :: Ptr JSONValue -> Ptr CBool -> ErrPtr -> IO CBool

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

handleError :: ErrPtr -> IO ()
handleError errPtr = do
  errCode <- toEnum . fromEnum <$> peek errPtr
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- peekCString =<< getErrorMessageImpl errPtr
    throwIO . SIMDException $ errStr

withDocument
  :: Ptr SIMDParser
  -> Ptr SIMDDocument
  -> Ptr PaddedString
  -> (Ptr JSONValue -> IO a)
  -> IO a
withDocument parserPtr docPtr inputPtr action =
  allocaBytes 24 $ \valPtr ->
  alloca $ \errPtr -> do
    -- traceM "getIterator"
    getIterator parserPtr inputPtr docPtr errPtr
    handleError errPtr
    -- traceM "getDocumentValue"
    getDocumentValueImpl docPtr valPtr errPtr
    handleError errPtr
    action valPtr

withObject :: Ptr JSONValue -> (Ptr JSONObject -> IO a) -> IO a
withObject valPtr action =
  allocaBytes 24 $ \oPtr ->
  alloca $ \errPtr -> do
    -- traceM "withObject"
    getObjectFromValueImpl valPtr oPtr errPtr
    handleError errPtr
    action oPtr

withUnorderedField :: Ptr JSONObject -> String -> (Ptr JSONValue -> IO a) -> IO a
withUnorderedField objPtr key action =
  withCString key $ \cstr ->
  allocaBytes 24 $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM "withUnorderedField"
    findFieldUnorderedImpl objPtr cstr vPtr errPtr
    handleError errPtr
    action vPtr

withField :: Ptr JSONObject -> String -> (Ptr JSONValue -> IO a) -> IO a
withField objPtr key action =
  withCString key $ \cstr ->
  allocaBytes 24 $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM "withField"
    findFieldImpl objPtr cstr vPtr errPtr
    handleError errPtr
    action vPtr

getInt :: Ptr JSONValue -> IO Int
getInt valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getInt"
    getIntImpl valPtr ptr errPtr
    handleError errPtr
    fromEnum <$> peek ptr

getBool :: Ptr JSONValue -> IO Bool
getBool valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getBool"
    getBoolImpl valPtr ptr errPtr
    handleError errPtr
    toBool <$> peek ptr

getString :: Ptr JSONValue -> IO String
getString valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getString"
    getStringImpl valPtr ptr errPtr
    handleError errPtr
    peekCString =<< peek ptr

mkValues :: Int -> IO (ForeignPtr (Ptr JSONValue))
mkValues len = mask_ $ do
  outPtr <- mallocArray len
  lenPtr <- new size
  vals <- makeValuesImpl size outPtr
  newForeignPtrEnv deleteValuesImpl lenPtr vals
  where size = toEnum len

withArrayValues :: Ptr JSONArray -> Int -> ([Ptr JSONValue] -> IO a) -> IO a
withArrayValues arrPtr len action = do
  cells <- mkValues len
  withForeignPtr cells $ \outPtr -> do
    getArrayElemsImpl arrPtr outPtr
    ptrs <- peekArray len outPtr
    action ptrs

withArrayElems :: FromJSON a => Ptr JSONValue -> IO [a]
withArrayElems valPtr =
  alloca $ \lenPtr ->
  alloca $ \errPtr ->
  allocaBytes 24 $ \aPtr -> do
    -- traceM "withArrayElems"
    getArrayFromValueImpl valPtr aPtr lenPtr errPtr
    handleError errPtr
    len <- fromEnum <$> peek lenPtr
    withArrayValues aPtr len $ traverse parseJSON

(.:) :: FromJSON a => Ptr JSONObject -> String -> IO a
infixl 5 .:
objPtr .: key = withUnorderedField objPtr key parseJSON

(.:>) :: FromJSON a => Ptr JSONObject -> String -> IO a
infixl 5 .:>
objPtr .:> key = withField objPtr key parseJSON

getRawJSONString :: Ptr SIMDDocument -> IO ByteString
getRawJSONString docPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    getRawJSONStringImpl docPtr ptr errPtr
    peek ptr >>= packCString

mkSIMDParser :: IO (ForeignPtr SIMDParser)
mkSIMDParser = mask_ $ do
  ptr <- parserInit
  newForeignPtr parserDestroy ptr

mkSIMDDocument :: IO (ForeignPtr SIMDDocument)
mkSIMDDocument = mask_ $ do
  ptr <- makeDocumentImpl
  newForeignPtr deleteDocumentImpl ptr

mkSIMDPaddedStr :: ByteString -> IO (ForeignPtr PaddedString)
mkSIMDPaddedStr input = mask_ $ useAsCStringLen input $ \(cstr, len) -> do
  ptr <- makeInputImpl cstr (toEnum len)
  newForeignPtr deleteInputImpl ptr

type Value = Ptr JSONValue

class FromJSON a where
  parseJSON :: Ptr JSONValue -> IO a
  parseJSONList :: Ptr JSONValue -> IO [a]
  parseJSONList = withArrayElems

instance FromJSON Text where
  parseJSON = fmap T.pack . getString

instance FromJSON Int where
  parseJSON = getInt

instance FromJSON Bool where
  parseJSON = getBool

instance FromJSON Char where
  parseJSON = getString >=> parseChar
  parseJSONList = getString

parseChar :: String -> IO Char
parseChar [x] = pure x
parseChar _   = fail "expected a string of length 1"

instance (FromJSON a) => FromJSON [a] where
  parseJSON = parseJSON1

class FromJSON1 f where
  liftParseJSON :: (Value -> IO a) -> (Value -> IO [a]) -> Value -> IO (f a)
  liftParseJSONList :: (Value -> IO a) -> (Value -> IO [a]) -> Value -> IO [f a]
  liftParseJSONList f g v = listParser (liftParseJSON f g) v

instance FromJSON1 [] where
  liftParseJSON _ p = p

parseJSON1 :: (FromJSON1 f, FromJSON a) => Ptr JSONValue -> IO (f a)
parseJSON1 = liftParseJSON parseJSON parseJSONList
{-# INLINE parseJSON1 #-}

listParser :: (Value -> IO a) -> Value -> IO [a]
listParser f v = undefined
{-# INLINE listParser #-}

decode
  :: FromJSON a
  => ByteString
  -> IO a
decode bs = do
  parser <- mkSIMDParser
  document <- mkSIMDDocument
  input <- mkSIMDPaddedStr bs
  withForeignPtr parser $ \parserPtr ->
    withForeignPtr document $ \docPtr ->
      withForeignPtr input $ \inputPtr ->
        withDocument parserPtr docPtr inputPtr parseJSON

decodeWith
  :: FromJSON a
  => ForeignPtr SIMDParser
  -> ForeignPtr SIMDDocument
  -> ForeignPtr PaddedString
  -> IO a
decodeWith parser document input =
  withForeignPtr parser $ \parserPtr ->
  withForeignPtr document $ \docPtr ->
  withForeignPtr input $ \inputPtr ->
  withDocument parserPtr docPtr inputPtr parseJSON
