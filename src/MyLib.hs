{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib (test) where

import Control.Exception (Exception, mask_, throwIO, toException)
import Control.Monad ((>=>), replicateM)
import qualified Data.Aeson as Aeson
import Data.ByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Text as T
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types

test :: IO ()
test = do
  let testStr = "{\"world\": \"testing stuff\", \"hello\": 2, \"list\": [10, 20, 30]}"
  print testStr
  d <- decode testStr :: IO Test
  print d

aesonTest :: IO ()
aesonTest = do
  let testStr = "{\"world\": \"testing stuff\", \"hello\": 2}"
  let d = Aeson.decode testStr :: Maybe Test
  print d

-- SIMD Types
data SIMDParser
data SIMDDocument
type ErrPtr = Ptr CInt

-- JSON Types
data JSONValue
data JSONObject
data JSONArray
data ArrayIter

foreign import ccall unsafe "parser_init" parserInit
  :: IO (Ptr SIMDParser)

foreign import ccall unsafe "&parser_destroy" parserDestroy
  :: FunPtr (Ptr SIMDParser -> IO ())

foreign import ccall unsafe "make_values_array" makeValuesImpl
  :: CSize -> Ptr (Ptr JSONValue) -> IO (Ptr (Ptr JSONValue))

foreign import ccall unsafe "&delete_values_array" deleteValuesImpl
  :: FunPtr (Ptr CSize -> Ptr (Ptr JSONValue) -> IO ())

foreign import ccall unsafe "get_iterator" getIterator
  :: Ptr SIMDParser -> CString -> CInt -> Ptr SIMDDocument -> ErrPtr -> IO ()

foreign import ccall unsafe "get_document_value" getDocumentValueImpl
  :: Ptr SIMDDocument -> Ptr JSONValue -> ErrPtr -> IO (Ptr JSONValue)

foreign import ccall unsafe "get_object_from_value" getObjectFromValueImpl
  :: Ptr JSONValue -> Ptr JSONObject -> ErrPtr -> IO (Ptr JSONObject)

foreign import ccall unsafe "get_array_from_value" getArrayFromValueImpl
  :: Ptr JSONValue -> Ptr JSONArray -> Ptr CInt -> ErrPtr -> IO (Ptr JSONArray)

foreign import ccall unsafe "get_array_elems" getArrayElemsImpl
  :: Ptr JSONArray -> Ptr (Ptr JSONValue) -> IO ()

foreign import ccall unsafe "find_field_unordered" findFieldUnorderedImpl
  :: Ptr JSONObject -> CString -> Ptr JSONValue -> ErrPtr -> IO (Ptr JSONValue)

foreign import ccall unsafe "get_raw_json_str" getRawJSONStringImpl
  :: Ptr SIMDDocument -> Ptr CString -> ErrPtr -> IO ()

foreign import ccall unsafe "get_int" getIntImpl
  :: Ptr JSONValue -> Ptr CInt -> ErrPtr -> IO CInt

foreign import ccall unsafe "get_string" getStringImpl
  :: Ptr JSONValue -> Ptr CString -> ErrPtr -> IO CString

foreign import ccall unsafe "get_error_message" getErrorMessageImpl
  :: ErrPtr -> IO CString

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

withDocumentValue :: Ptr SIMDParser -> ByteString -> (Ptr JSONValue -> IO a) -> IO a
withDocumentValue parserPtr bytes action =
  useAsCStringLen bytes $ \(cstr, len) ->
    allocaBytes 64 $ \vPtr ->
    allocaBytes 64 $ \dPtr ->
    alloca $ \errPtr -> do
      getIterator parserPtr cstr (toEnum len) dPtr errPtr
      handleError errPtr
      getDocumentValueImpl dPtr vPtr errPtr
      handleError errPtr
      action vPtr

withObject :: Ptr JSONValue -> (Ptr JSONObject -> IO a) -> IO a
withObject valPtr action =
  allocaBytes 64 $ \oPtr ->
  alloca $ \errPtr -> do
    getObjectFromValueImpl valPtr oPtr errPtr
    handleError errPtr
    action oPtr

withField :: Ptr JSONObject -> String -> (Ptr JSONValue -> IO a) -> IO a
withField objPtr key action =
  withCString key $ \cstr ->
  allocaBytes 64 $ \vPtr ->
  alloca $ \errPtr ->  do
    findFieldUnorderedImpl objPtr cstr vPtr errPtr
    handleError errPtr
    action vPtr

getInt :: Ptr JSONValue -> IO Int
getInt valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    getIntImpl valPtr ptr errPtr
    handleError errPtr
    fromEnum <$> peek ptr

getString :: Ptr JSONValue -> IO String
getString valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    getStringImpl valPtr ptr errPtr
    handleError errPtr
    peekCString =<< peek ptr

mkValues :: Int -> IO (ForeignPtr (Ptr JSONValue))
mkValues len = mask_ $
  allocaArray (toEnum len) $ \outPtr -> do
    ptr <- mallocForeignPtr
    withForeignPtr ptr $ \lenPtr -> do
      poke lenPtr (toEnum len)
      vals <- makeValuesImpl (toEnum len) outPtr
      newForeignPtrEnv deleteValuesImpl lenPtr vals

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
  allocaBytes 64 $ \arrPtr -> do
    getArrayFromValueImpl valPtr arrPtr lenPtr errPtr
    handleError errPtr
    len <- fromEnum <$> peek lenPtr
    withArrayValues arrPtr len $ \ptrs ->
      traverse parseJSON ptrs

(.:) :: FromJSON a => Ptr JSONObject -> String -> IO a
infixl 5 .:
objPtr .: key = withField objPtr key parseJSON

getRawJSONString :: Ptr SIMDDocument -> IO ByteString
getRawJSONString docPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    getRawJSONStringImpl docPtr ptr errPtr
    peek ptr >>= packCString

mkSIMDParser :: IO (ForeignPtr SIMDParser)
mkSIMDParser = mask_ $ do
  ptr <- parserInit
  newForeignPtr parserDestroy ptr

data Test =
  Test
    { intField :: Int
    , stringField :: String
    , listIntField :: [Int]
    } deriving (Show)

instance FromJSON Test where
  parseJSON valPtr = withObject valPtr $ \ obj -> do
    int <- obj .: "hello"
    str <- obj .: "world"
    ints <- obj .: "list"
    pure Test
      { intField = int
      , stringField = str
      , listIntField = ints
      }

instance Aeson.FromJSON Test where
  parseJSON = Aeson.withObject "test" $ \obj -> do
    int <- obj Aeson..: "hello"
    str <- obj Aeson..: "world"
    ints <- obj Aeson..: "list"
    pure Test
      { intField = int
      , stringField = str
      , listIntField = ints
      }

type Value = Ptr JSONValue

class FromJSON a where
  parseJSON :: Ptr JSONValue -> IO a
  parseJSONList :: Ptr JSONValue -> IO [a]
  parseJSONList = withArrayElems

instance FromJSON Text where
  parseJSON = fmap T.pack . getString

instance FromJSON Int where
  parseJSON = getInt

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

decode :: FromJSON a => ByteString -> IO a
decode input = do
  parser <- mkSIMDParser
  withForeignPtr parser $ \parserPtr ->
    withDocumentValue parserPtr input parseJSON
