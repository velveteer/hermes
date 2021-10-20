{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SIMDJSON
  ( decode
  , decodeWith
  , getRawJSONString
  , isNull
  , mkSIMDJSONEnv
  , withArray
  , withBool
  , withDouble
  , withInt
  , withObject
  , withString
  , withText
  , FromJSON(..)
  , (.:)
  , (.:?)
  , (.:>)
  , Value
  , Object
  , Array
  , ArrayIter
  , SIMDJSONEnv
  , SIMDParser
  , SIMDDocument
  , PaddedString
  ) where

-- import           Debug.Trace

import           Control.Exception     (Exception, mask_, throwIO)
import           Control.Monad         ((>=>))
import           Data.ByteString
import qualified Data.DList            as DList
import           Data.Functor.Identity (Identity (..))
import           Data.Text             (Text)
import qualified Data.Text.Foreign     as T
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Float             (double2Float)

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
  :: IO (Ptr SIMDParser)

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
foreign import ccall unsafe "get_raw_json_str" getRawJSONStringImpl
  :: Document -> Ptr CString -> ErrPtr -> IO ()

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
  :: Value -> Ptr CString -> Ptr CInt -> ErrPtr -> IO ()

foreign import ccall unsafe "get_bool" getBoolImpl
  :: Value -> Ptr CBool -> ErrPtr -> IO ()

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

-- | A reference to an opaque simdjson::ondemand::value.
newtype Value = Value (Ptr JSONValue)

-- | A reference to an opaque simdjson::ondemand::object.
newtype Object = Object (Ptr JSONObject)

-- | A reference to an opaque simdjson::ondemand::array.
newtype Array = Array (Ptr JSONArray)

-- | A reference to an opaque simdjson::ondemand::array_iterator.
newtype ArrayIter = ArrayIter (Ptr JSONArrayIter)

withDocument :: Parser -> Document -> InputBuffer -> (Value -> IO a) -> IO a
withDocument parserPtr docPtr inputPtr action =
  allocaValue $ \valPtr ->
  alloca $ \errPtr -> do
    -- traceM "getIterator"
    getIterator parserPtr inputPtr docPtr errPtr
    handleError errPtr
    -- traceM "getDocumentValue"
    getDocumentValueImpl docPtr valPtr errPtr
    handleError errPtr
    action valPtr

withObject :: (Object -> IO a) -> Value -> IO a
withObject f valPtr =
  allocaObject $ \oPtr ->
  alloca $ \errPtr -> do
    -- traceM "withObject"
    getObjectFromValueImpl valPtr oPtr errPtr
    handleError errPtr
    f oPtr

withUnorderedField :: (Value -> IO a) -> Object -> String -> IO a
withUnorderedField f objPtr key =
  withCString key $ \cstr ->
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM $ "withUnorderedField " <> key
    findFieldUnorderedImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr

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

withField :: (Value -> IO a) -> Object -> String -> IO a
withField f objPtr key =
  withCString key $ \cstr ->
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> do
    -- traceM $ "withField " <> key
    findFieldImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr

getInt :: Value -> IO Int
getInt valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getInt"
    getIntImpl valPtr ptr errPtr
    handleError errPtr
    fromEnum <$> peek ptr

withInt :: (Int -> IO a) -> Value -> IO a
withInt f = getInt >=> f

getDouble :: Value -> IO Double
getDouble valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getDouble"
    getDoubleImpl valPtr ptr errPtr
    handleError errPtr
    realToFrac <$> peek ptr

withDouble :: (Double -> IO a) -> Value -> IO a
withDouble f = getDouble >=> f

getBool :: Value -> IO Bool
getBool valPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    -- traceM "getBool"
    getBoolImpl valPtr ptr errPtr
    handleError errPtr
    toBool <$> peek ptr

withBool :: (Bool -> IO a) -> Value -> IO a
withBool f = getBool >=> f

fromCStringLen :: (CStringLen -> IO a) -> Value -> IO a
fromCStringLen f valPtr = mask_ $ do
  strPtr <- malloc
  alloca $ \lenPtr ->
    alloca $ \errPtr -> do
      getStringImpl valPtr strPtr lenPtr errPtr
      handleError errPtr
      len <- fromEnum <$> peek lenPtr
      str <- peek strPtr
      result <- f (str, len)
      free strPtr
      pure result
{-# INLINE fromCStringLen #-}

getString :: Value -> IO String
getString = fromCStringLen peekCStringLen

getText :: Value -> IO Text
getText = fromCStringLen T.peekCStringLen

withString :: (String -> IO a) -> Value -> IO a
withString f = getString >=> f

withText :: (Text -> IO a) -> Value -> IO a
withText f = getText >=> f

isNull :: Value -> IO Bool
isNull valPtr = toBool <$> isNullImpl valPtr

withArray :: (Array -> IO a) -> Value -> IO a
withArray f val =
  alloca $ \errPtr ->
  allocaArray $ \arrPtr -> do
    -- traceM "withArray"
    getArrayFromValueImpl val arrPtr errPtr
    handleError errPtr
    f arrPtr

withArrayIter :: (ArrayIter -> IO a) -> Array -> IO a
withArrayIter f arrPtr =
  alloca $ \errPtr ->
  allocaArrayIter $ \iterPtr -> do
    -- traceM "withArrayIter"
    getArrayIterImpl arrPtr iterPtr errPtr
    handleError errPtr
    f iterPtr

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

getRawJSONString :: Document -> IO ByteString
getRawJSONString docPtr =
  alloca $ \ptr -> alloca $ \errPtr -> do
    getRawJSONStringImpl docPtr ptr errPtr
    peek ptr >>= packCString

-- Public Interface

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
(.:) :: FromJSON a => Object -> String -> IO a
infixl 5 .:
objPtr .: key = withUnorderedField parseJSON objPtr key

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
(.:?) :: FromJSON a => Object -> String -> IO (Maybe a)
infixl 5 .:?
objPtr .:? key = withUnorderedOptionalField parseJSON objPtr key

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
(.:>) :: FromJSON a => Object -> String -> IO a
infixl 5 .:>
objPtr .:> key = withField parseJSON objPtr key

class FromJSON a where
  parseJSON :: Value -> IO a
  parseJSONList :: Value -> IO [a]
  parseJSONList = parseArray

parseArray :: FromJSON a => Value -> IO [a]
parseArray valPtr =
  flip withArray valPtr $ \arrPtr ->
  flip withArrayIter arrPtr $ iterateOverArray parseJSON

-- | This is convenient for writing nested object parsers,
-- but it is a possible footgun for users who parse values
-- out of order.
instance FromJSON Value where
  parseJSON = pure

instance FromJSON Text where
  parseJSON = getText

instance FromJSON Int where
  parseJSON = getInt

instance FromJSON Float where
  parseJSON = fmap double2Float . getDouble

instance FromJSON Double where
  parseJSON = getDouble

instance FromJSON Bool where
  parseJSON = getBool

instance FromJSON Char where
  parseJSON = getString >=> parseChar
  parseJSONList = getString

parseChar :: String -> IO Char
parseChar [x] = pure x
parseChar _   = fail "expected a string of length 1"

class FromJSON1 f where
  liftParseJSON :: (Value -> IO a) -> (Value -> IO [a]) -> Value -> IO (f a)
  liftParseJSONList :: (Value -> IO a) -> (Value -> IO [a]) -> Value -> IO [f a]
  liftParseJSONList f g v = listParser (liftParseJSON f g) v

-- List
instance FromJSON1 [] where
  liftParseJSON _ p = p

instance (FromJSON a) => FromJSON [a] where
  parseJSON = parseJSON1

-- Maybe -- when values can be null
instance FromJSON1 Maybe where
  liftParseJSON p _ a = do
    nil <- isNull a
    if nil
       then pure Nothing
       else Just <$> p a

instance (FromJSON a) => FromJSON (Maybe a) where
  parseJSON = parseJSON1

-- Identity Functor
instance FromJSON1 Identity where
  liftParseJSON p _ a = Identity <$> p a
  liftParseJSONList _ p a = fmap Identity <$> p a

instance (FromJSON a) => FromJSON (Identity a) where
  parseJSON = parseJSON1
  parseJSONList = liftParseJSONList parseJSON parseJSONList

parseJSON1 :: (FromJSON1 f, FromJSON a) => Value -> IO (f a)
parseJSON1 = liftParseJSON parseJSON parseJSONList
{-# INLINE parseJSON1 #-}

listParser :: (Value -> IO a) -> Value -> IO [a]
listParser f valPtr =
  flip withArray valPtr $ \arrPtr ->
  flip withArrayIter arrPtr $ iterateOverArray f
{-# INLINE listParser #-}

-- Decoding

data SIMDJSONEnv =
  SIMDJSONEnv
    { simdParser :: ForeignPtr SIMDParser
    , simdDoc    :: ForeignPtr SIMDDocument
    , simdInput  :: ForeignPtr PaddedString
    }

mkSIMDJSONEnv :: ByteString -> IO SIMDJSONEnv
mkSIMDJSONEnv bytes = do
  parser   <- mkSIMDParser
  document <- mkSIMDDocument
  input    <- mkSIMDPaddedStr bytes
  pure SIMDJSONEnv
    { simdParser = parser
    , simdDoc = document
    , simdInput = input
    }

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

-- | Construct a SIMDJSONEnv from the provided input and decode it.
-- The simdjson instances will be out of scope when decode returns, which
-- means the garbage collector will/should run their finalizers.
decode :: FromJSON a => ByteString -> IO a
decode bs = do
  parser <- mkSIMDParser
  document <- mkSIMDDocument
  input <- mkSIMDPaddedStr bs
  withForeignPtr parser $ \parserPtr ->
    withForeignPtr document $ \docPtr ->
      withForeignPtr input $ \inputPtr ->
        withDocument (Parser parserPtr) (Document docPtr) (InputBuffer inputPtr) parseJSON

-- | Decode with a caller-provided `SIMDJSONEnv`. If the caller retains a reference to
-- the `SIMDJSONEnv` then the simdjson instance finalizers will not be run.
decodeWith :: FromJSON a => SIMDJSONEnv -> IO a
decodeWith env =
  withForeignPtr (simdParser env) $ \parserPtr ->
  withForeignPtr (simdDoc env) $ \docPtr ->
  withForeignPtr (simdInput env) $ \inputPtr ->
  withDocument (Parser parserPtr) (Document docPtr) (InputBuffer inputPtr) parseJSON
