{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Exposes functions for building JSON decoders that harness the power
of the simdjson::ondemand API.
-}

module Data.Hermes
  ( -- * Decoding from ByteString input
    Decoder
  , HermesEnv
  , decode
  , decodeWith
  -- * Object field accessors
  , atKey
  , atOptionalKey
  , atOrderedKey
    -- * Value decoders
  , bool
  , char
  , double
  , int
  , scientific
  , string
  , text
  , list
  , nullable
  -- * Environment creation
  , mkHermesEnv
  , mkHermesEnv_
  -- * Error Types
  , HermesException(..)
  , HError(..)
  -- * Value helpers
  , isNull
  , withArray
  , withBool
  , withDouble
  , withInt
  , withObject
  , withString
  , withText
  -- * simdjson Types
  , Value
  , Object
  , Array
  , ArrayIter
  ) where

import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.IO.Unlift (withRunInIO)
import           Control.Monad.Trans.Reader (ReaderT(..), asks, local, runReaderT)
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BS
import qualified Data.DList as DList
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import           UnliftIO.Exception
import           UnliftIO.Foreign hiding (allocaArray, withArray)

-- | Phantom type for a pointer to simdjson::ondemand::parser.
data SIMDParser
-- | Phantom type for a pointer to simdjson::ondemand::document.
data SIMDDocument
-- | Phantom type for a pointer to simdjson::padded_string_view.
data PaddedStringView

-- | Alias for a pointer to the simdjson::error_code that most functions use
-- to report errors back to us.
type ErrPtr = Ptr CInt

-- | Phantom type for a pointer to simdjson::ondemand::value.
data JSONValue
-- | Phantom type for a pointer to simdjson::ondemand::object.
data JSONObject
-- | Phantom type for a pointer to simdjson::ondemand::array.
data JSONArray
-- | Phantom type for a pointer to simdjson::ondemand::array_iterator
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

foreign import ccall unsafe "make_input_view" makeInputViewImpl
  :: CString -> CSize -> IO (Ptr PaddedStringView)

foreign import ccall unsafe "&delete_input_view" deleteInputViewImpl
  :: FunPtr (Ptr PaddedStringView -> IO ())

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
  :: Document -> CString -> Ptr CSize -> IO ()

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

-- | The library can throw exceptions from simdjson in addition to
-- its own exceptions.
data HermesException =
    SIMDException HError
    -- ^ An exception thrown from the simdjson library.
  | InternalException HError
    -- ^ An exception thrown from an internal library function.
  deriving (Show, Exception)

-- | Record containing all pertinent information for troubleshooting an exception.
data HError =
  HError
    { path        :: !String
    -- ^ The path to the current element determined by the decoder.
    , errorMsg    :: !String
    -- ^ An error message.
    , docLocation :: !String
    -- ^ Truncated location of the simdjson document iterator.
    , docDebug    :: !String
    -- ^ Debug information from simdjson::document.
    } deriving Show

throwSIMD :: String -> Decoder a
throwSIMD msg = buildHError msg >>= liftIO . throwIO . SIMDException

throwInternal :: String -> Decoder a
throwInternal msg = buildHError msg >>= liftIO . throwIO . InternalException

buildHError :: String -> Decoder HError
buildHError msg = withRunInIO $ \run -> do
  docFPtr <- run $ asks hDocument
  path' <- run $ asks hPath
  alloca $ \errPtr -> run $
    alloca $ \locStrPtr ->
    alloca $ \lenPtr ->
      withDocumentPointer docFPtr $ \docPtr -> do
        strFPtr <- mallocForeignPtrBytes 128
        withForeignPtr strFPtr $ \dbStrPtr -> do
          liftIO $ toDebugStringImpl docPtr dbStrPtr lenPtr
          len <- fmap fromEnum . liftIO $ peek lenPtr
          debugStr <- peekCStringLen (dbStrPtr, len)
          locStr <- peekCString =<< liftIO (currentLocationImpl docPtr locStrPtr errPtr)
          pure $ HError path' msg (Prelude.take 100 locStr) debugStr

handleError :: ErrPtr -> Decoder ()
handleError errPtr = do
  errCode <- toEnum . fromEnum <$> liftIO (peek errPtr)
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- peekCString =<< liftIO (getErrorMessageImpl errPtr)
    throwSIMD errStr

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

-- | A reference to an opaque simdjson::ondemand::parser.
newtype Parser = Parser (Ptr SIMDParser)

-- | A reference to an opaque simdjson::ondemand::document.
newtype Document = Document (Ptr SIMDDocument)

-- | A reference to an opaque simdjson::padded_string_view.
newtype InputBuffer = InputBuffer (Ptr PaddedStringView)

-- | A reference to an opaque simdjson::ondemand::value.
newtype Value = Value (Ptr JSONValue)

-- | A reference to an opaque simdjson::ondemand::object.
newtype Object = Object (Ptr JSONObject)

-- | A reference to an opaque simdjson::ondemand::array.
newtype Array = Array (Ptr JSONArray)

-- | A reference to an opaque simdjson::ondemand::array_iterator.
newtype ArrayIter = ArrayIter (Ptr JSONArrayIter)

withParserPointer :: ForeignPtr SIMDParser -> (Parser -> Decoder a) -> Decoder a
withParserPointer parserFPtr f =
  withForeignPtr parserFPtr $ f . Parser

withDocumentPointer :: ForeignPtr SIMDDocument -> (Document -> Decoder a) -> Decoder a
withDocumentPointer docFPtr f =
  withForeignPtr docFPtr $ f . Document

withInputPointer :: ForeignPtr PaddedStringView -> (InputBuffer -> Decoder a) -> Decoder a
withInputPointer pStrFPtr f =
  withForeignPtr pStrFPtr $ f . InputBuffer

withDocument :: (Value -> Decoder a) -> InputBuffer -> Decoder a
withDocument f inputPtr =
  allocaValue $ \valPtr -> withRunInIO $ \run ->
  alloca $ \errPtr -> run $ do
    docFPtr <- asks hDocument
    parserFPtr <- asks hParser
    withParserPointer parserFPtr $ \parserPtr ->
      withDocumentPointer docFPtr $ \docPtr -> do
        liftIO $ getIterator parserPtr inputPtr docPtr errPtr
        handleError errPtr
        liftIO $ getDocumentValueImpl docPtr valPtr errPtr
        handleError errPtr
        f valPtr

-- | Helper to work with an Object parsed from a Value.
withObject :: (Object -> Decoder a) -> Value -> Decoder a
withObject f valPtr =
  allocaObject $ \oPtr -> withRunInIO $ \run ->
  alloca $ \errPtr -> run $ do
    liftIO $ getObjectFromValueImpl valPtr oPtr errPtr
    handleError errPtr
    f oPtr

withUnorderedField :: (Value -> Decoder a) -> Object -> String -> Decoder a
withUnorderedField f objPtr key = withRunInIO $ \run ->
  withCString key $ \cstr -> run $
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> withPath key $ do
    liftIO $ findFieldUnorderedImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr

withUnorderedOptionalField :: (Value -> Decoder a) -> Object -> String -> Decoder (Maybe a)
withUnorderedOptionalField f objPtr key = withRunInIO $ \run ->
  withCString key $ \cstr -> run $
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> withPath key $ do
    liftIO $ findFieldUnorderedImpl objPtr cstr vPtr errPtr
    errCode <- toEnum . fromEnum <$> liftIO (peek errPtr)
    if | errCode == SUCCESS       -> Just <$> f vPtr
       | errCode == NO_SUCH_FIELD -> pure Nothing
       | otherwise                -> Nothing <$ handleError errPtr

withField :: (Value -> Decoder a) -> Object -> String -> Decoder a
withField f objPtr key = withRunInIO $ \run ->
  withCString key $ \cstr -> run $
  allocaValue $ \vPtr ->
    alloca $ \errPtr -> withPath key $ do
    liftIO $ findFieldImpl objPtr cstr vPtr errPtr
    handleError errPtr
    f vPtr

withPath :: String -> Decoder a -> Decoder a
withPath key = local (\st -> st { hPath = hPath st <> "." <> key })

getInt :: Value -> Decoder Int
getInt valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    liftIO $ getIntImpl valPtr ptr errPtr
    handleError errPtr
    fmap fromEnum . liftIO $ peek ptr

-- | Helper to work with an Int parsed from a Value.
withInt :: (Int -> Decoder a) -> Value -> Decoder a
withInt f = getInt >=> f

getDouble :: Value -> Decoder Double
getDouble valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    liftIO $ getDoubleImpl valPtr ptr errPtr
    handleError errPtr
    fmap realToFrac . liftIO $ peek ptr

-- | Helper to work with a Double parsed from a Value.
withDouble :: (Double -> Decoder a) -> Value -> Decoder a
withDouble f = getDouble >=> f

-- | Parse a Scientific from a Value.
scientific :: Value -> Decoder Sci.Scientific
scientific = withRawByteString parseScientific

-- | Parse a Scientific using attoparsec's ByteString.Char8 parser.
parseScientific :: BSC.ByteString -> Decoder Sci.Scientific
parseScientific
  = either (\err -> throwInternal $ "failed to parse Scientific: " <> err) pure
  . A.parseOnly (A.scientific <* A.endOfInput)

getBool :: Value -> Decoder Bool
getBool valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    liftIO $ getBoolImpl valPtr ptr errPtr
    handleError errPtr
    fmap toBool . liftIO $ peek ptr

-- | Helper to work with a Bool parsed from a Value.
withBool :: (Bool -> Decoder a) -> Value -> Decoder a
withBool f = getBool >=> f

fromCStringLen :: (CStringLen -> Decoder a) -> Value -> Decoder a
fromCStringLen f valPtr = withRunInIO $ \run -> mask_ $
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $
  alloca $ \errPtr -> do
    liftIO $ getStringImpl valPtr strPtr lenPtr errPtr
    handleError errPtr
    len <- fmap fromEnum . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    f (str, len)

getString :: Value -> Decoder String
getString = fromCStringLen (liftIO . peekCStringLen)

getText :: Value -> Decoder Text
getText = fromCStringLen (liftIO . T.peekCStringLen)

getRawByteString :: Value -> Decoder BSC.ByteString
getRawByteString valPtr = withRunInIO $ \run -> mask_ $
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $
  alloca $ \errPtr -> do
    liftIO $ getRawJSONTokenImpl valPtr strPtr lenPtr errPtr
    handleError errPtr
    len <- fmap fromEnum . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    liftIO $ BSC.packCStringLen (str, len)

-- | Helper to work with a raw ByteString.Char8 parsed from a Value.
withRawByteString :: (BSC.ByteString -> Decoder a) -> Value -> Decoder a
withRawByteString f = getRawByteString >=> f

-- | Helper to work with a String parsed from a Value.
withString :: (String -> Decoder a) -> Value -> Decoder a
withString f = getString >=> f

-- | Helper to work with a Text parsed from a Value.
withText :: (Text -> Decoder a) -> Value -> Decoder a
withText f = getText >=> f

-- | Returns True if the Value is null.
isNull :: Value -> Decoder Bool
isNull valPtr = fmap toBool . liftIO $ isNullImpl valPtr

-- | Helper to work with an Array parsed from a Value.
withArray :: (Array -> Decoder a) -> Value -> Decoder a
withArray f val = withRunInIO $ \run ->
  alloca $ \errPtr -> run $
  allocaArray $ \arrPtr -> do
    liftIO $ getArrayFromValueImpl val arrPtr errPtr
    handleError errPtr
    f arrPtr

-- | Helper to work with an ArrayIter started from an Array.
withArrayIter :: (ArrayIter -> Decoder a) -> Array -> Decoder a
withArrayIter f arrPtr = withRunInIO $ \run ->
  alloca $ \errPtr -> run $
  allocaArrayIter $ \iterPtr -> do
    liftIO $ getArrayIterImpl arrPtr iterPtr errPtr
    handleError errPtr
    f iterPtr

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a list.
iterateOverArray :: (Value -> Decoder a) -> ArrayIter -> Decoder [a]
iterateOverArray f iterPtr =
  withRunInIO $ \runInIO ->
  alloca $ \errPtr -> runInIO $
  allocaValue $ \valPtr ->
  go DList.empty valPtr errPtr
  where
    go acc valPtr errPtr = do
      isOver <- fmap toBool . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then do
            liftIO $ arrayIterGetCurrentImpl iterPtr valPtr errPtr
            handleError errPtr
            result <- f valPtr
            liftIO $ arrayIterMoveNextImpl iterPtr
            go (acc <> DList.singleton result) valPtr errPtr
        else
          pure $ DList.toList acc

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
atKey :: String -> (Value -> Decoder a) -> Object -> Decoder a
atKey key parser obj = withUnorderedField parser obj key

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atOptionalKey :: String -> (Value -> Decoder a) -> Object -> Decoder (Maybe a)
atOptionalKey key parser obj = withUnorderedOptionalField parser obj key

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atOrderedKey :: String -> (Value -> Decoder a) -> Object -> Decoder a
atOrderedKey key parser obj = withField parser obj key

-- | Parse a homogenous JSON array into a Haskell list.
list :: (Value -> Decoder a) -> Value -> Decoder [a]
list f val =
  flip withArray val $ \arr ->
  flip withArrayIter arr $ iterateOverArray f

-- | Transforms a parser to return Nothing when the value is null.
nullable :: (Value -> Decoder a) -> Value -> Decoder (Maybe a)
nullable parser val = do
  nil <- isNull val
  if nil
    then pure Nothing
    else Just <$> parser val

-- | Parse only a single character.
char :: Value -> Decoder Char
char = getText >=> justOne
  where
    justOne txt =
      case T.uncons txt of
        Just (c, "") ->
          pure c
        _ ->
          throwInternal "expected a single character"

-- | Parse a JSON string into a Haskell String.
-- For best performance you should use `text` instead.
string :: Value -> Decoder String
string = getString

-- | Parse a JSON string into Haskell Text.
text :: Value -> Decoder Text
text = getText

-- | Parse a JSON number into an unsigned Haskell Int.
int :: Value -> Decoder Int
int = getInt

-- | Parse a JSON boolean into a Haskell Bool.
bool :: Value -> Decoder Bool
bool = getBool

-- | Parse a JSON number into a Haskell Double.
double :: Value -> Decoder Double
double = getDouble

-- Decoding Types and Functions

-- | A Decoder is intended to be run in IO and will throw IO exceptions.
type Decoder a = ReaderT HermesEnv IO a

-- | Contains foreign references to the allocated simdjson::parser
-- and simdjson::document. Also maintains a path string that is updated
-- when an object field is entered and which is displayed in errors.
data HermesEnv =
  HermesEnv
    { hParser   :: !(ForeignPtr SIMDParser)
    , hDocument :: !(ForeignPtr SIMDDocument)
    , hPath     :: !String
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
    { hParser   = parser
    , hDocument = document
    , hPath     = ""
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

mkSIMDPaddedStrView :: ByteString -> IO (ForeignPtr PaddedStringView)
mkSIMDPaddedStrView input = mask_ $ do
  let (fp, o, len) = BS.toForeignPtr input
  withForeignPtr fp $ \bsPtr -> do
    ptr <- makeInputViewImpl (bsPtr `plusPtr` o) (toEnum len)
    newForeignPtr deleteInputViewImpl ptr

-- | Construct an ephemeral `HermesEnv` and use it to decode the input.
-- The simdjson instances will be out of scope when decode returns, which
-- means the garbage collector will/should run their finalizers.
-- This is convenient for users who do not need to hold onto a `HermesEnv` for
-- a long-running single-threaded process. There is a small performance penalty
-- for creating the simdjson instances on each decode.
decode :: ByteString -> (Value -> Decoder a) -> IO a
decode bs d = do
  hEnv      <- mkHermesEnv_
  paddedStr <- mkSIMDPaddedStrView bs
  flip runReaderT hEnv $
    withInputPointer paddedStr $ \inputPtr ->
      withDocument d inputPtr

-- | Decode with a caller-provided `HermesEnv`. If the caller retains a reference to
-- the `HermesEnv` then the simdjson instance finalizers will not be run.
-- This is useful for long-running applications that want to re-use simdjson instances
-- for optimal performance.
-- Do NOT share a `HermesEnv` across multiple threads, but it is fine for each thread
-- to have its own.
decodeWith :: HermesEnv -> ByteString -> (Value -> Decoder a) -> IO a
decodeWith env bs d = do
  paddedStr <- mkSIMDPaddedStrView bs
  flip runReaderT env $
    withInputPointer paddedStr $ \inputPtr ->
      withDocument d inputPtr
