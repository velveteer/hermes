{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Exposes functions for building JSON decoders that harness the power
of the simdjson::ondemand API.
-}

module Data.Hermes
  ( -- * Decoding from ByteString input
    Decoder(runDecoder)
  , HermesEnv
  , decode
  , decodeEither
  , withHermesEnv
  , withInputBuffer
  , withDocument
  -- * Object field accessors
  , Key
  , mkKey
  , keyFromText
  , atKey
  , atOptionalKey
  , atOrderedKey
  -- * JSON pointer decoder
  , Pointer
  , mkPointer
  , atPointer
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
  , objectAsKeyValues
  , day
  , month
  , quarter
  , timeOfDay
  , timeZone
  , localTime
  , utcTime
  , zonedTime
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

import           Control.Applicative (Alternative(..))
import           Control.DeepSeq (NFData)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Reader (ReaderT(..), asks, local, runReaderT)
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Time as ATime
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.DList as DList
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Foreign as T
import qualified Data.Time as Time
import qualified Data.Time.Calendar.Month.Compat as Time
import qualified Data.Time.Calendar.Quarter.Compat as Time
import qualified Data.Time.LocalTime as Local
import           GHC.Generics (Generic)
import qualified System.IO.Unsafe as Unsafe
import           UnliftIO.Exception (Exception, bracket, catch, mask_, throwIO, try)
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
-- | Phantom type for a pointer to simdjson::ondemand::object_iterator
data JSONObjectIter

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

foreign import ccall unsafe "at_pointer" atPointerImpl
  :: CString -> Int -> Document -> Value -> ErrPtr -> IO ()

foreign import ccall unsafe "get_object_from_value" getObjectFromValueImpl
  :: Value -> Object -> ErrPtr -> IO ()

foreign import ccall unsafe "get_object_iter" getObjectIterImpl
  :: Object -> ObjectIter -> ErrPtr -> IO ()

foreign import ccall unsafe "obj_iter_is_done" objectIterIsDoneImpl
  :: ObjectIter -> IO CBool

foreign import ccall unsafe "obj_iter_get_current" objectIterGetCurrentImpl
  :: ObjectIter -> Ptr CString -> Ptr CSize -> Value -> ErrPtr -> IO ()

foreign import ccall unsafe "obj_iter_move_next" objectIterMoveNextImpl
  :: ObjectIter -> IO ()

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
  :: Object -> CString -> Int -> Value -> ErrPtr -> IO ()

foreign import ccall unsafe "find_field" findFieldImpl
  :: Object -> CString -> Int -> Value -> ErrPtr -> IO ()

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
  :: Value -> Ptr Int -> ErrPtr -> IO ()

foreign import ccall unsafe "get_double" getDoubleImpl
  :: Value -> Ptr Double -> ErrPtr -> IO ()

foreign import ccall unsafe "get_string" getStringImpl
  :: Value -> Ptr CString -> Ptr CSize -> ErrPtr -> IO ()

foreign import ccall unsafe "get_bool" getBoolImpl
  :: Value -> Ptr CBool -> ErrPtr -> IO ()

foreign import ccall unsafe "get_raw_json_token" getRawJSONTokenImpl
  :: Value -> Ptr CString -> Ptr CSize -> IO ()

-- | The library can throw exceptions from simdjson in addition to
-- its own exceptions.
data HermesException =
    SIMDException HError
    -- ^ An exception thrown from the simdjson library.
  | InternalException HError
    -- ^ An exception thrown from an internal library function.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception, NFData)

-- | Record containing all pertinent information for troubleshooting an exception.
data HError =
  HError
    { path        :: !Text
    -- ^ The path to the current element determined by the decoder.
    -- Formatted in the JSON Pointer standard per RFC 6901.
    , errorMsg    :: !Text
    -- ^ An error message.
    , docLocation :: !Text
    -- ^ Truncated location of the simdjson document iterator.
    , docDebug    :: !Text
    -- ^ Debug information from simdjson::document.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Re-throw an exception caught from the simdjson library.
throwSIMD :: Text -> Decoder a
throwSIMD msg = buildHError msg >>= liftIO . throwIO . SIMDException

-- | Throw an IO exception in the `Decoder` context.
throwHermes :: Text -> Decoder a
throwHermes msg = buildHError msg >>= liftIO . throwIO . InternalException

typePrefix :: Text -> Text
typePrefix typ = "Error while getting value of type " <> typ <> ". "

buildHError :: Text -> Decoder HError
buildHError msg = Decoder $ withRunInIO $ \run -> do
  docFPtr <- run $ asks hDocument
  pth <- run $ (asks hPath >>= runDecoder . decodeUtfOrThrow)
  alloca $ \errPtr -> run . runDecoder $
    alloca $ \locStrPtr ->
    alloca $ \lenPtr ->
      withDocumentPointer docFPtr $ \docPtr -> do
        strFPtr <- mallocForeignPtrBytes 128
        withForeignPtr strFPtr $ \dbStrPtr -> do
          liftIO $ toDebugStringImpl docPtr dbStrPtr lenPtr
          len <- fmap fromIntegral . liftIO $ peek lenPtr
          debugStr <- liftIO $ T.peekCStringLen (dbStrPtr, len)
          locStr <- peekCString =<< liftIO (currentLocationImpl docPtr locStrPtr errPtr)
          pure $ HError pth msg (T.pack $ Prelude.take 20 locStr) debugStr

handleError :: Text -> ErrPtr -> Decoder ()
handleError pre errPtr = do
  errCode <- toEnum . fromIntegral <$> liftIO (peek errPtr)
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- peekCString =<< liftIO (getErrorMessageImpl errPtr)
    throwSIMD $ pre <> T.pack errStr
{-# INLINE handleError #-}

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
  deriving stock (Eq, Show, Bounded, Enum)

allocaValue :: (Value -> Decoder a) -> Decoder a
allocaValue f = allocaBytes 24 $ \val -> f (Value val)

allocaObject :: (Object -> Decoder a) -> Decoder a
allocaObject f = allocaBytes 24 $ \objPtr -> f (Object objPtr)

allocaArray :: (Array -> Decoder a) -> Decoder a
allocaArray f = allocaBytes 24 $ \arr -> f (Array arr)

allocaArrayIter :: (ArrayIter -> Decoder a) -> Decoder a
allocaArrayIter f = allocaBytes 24 $ \iter -> f (ArrayIter iter)

allocaObjectIter :: (ObjectIter -> Decoder a) -> Decoder a
allocaObjectIter f = allocaBytes 24 $ \iter -> f (ObjectIter iter)

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

-- | A reference to an opaque simdjson::ondemand::object_iterator.
newtype ObjectIter = ObjectIter (Ptr JSONObjectIter)

-- | Public-facing type for object keys. Use OverloadedStrings for convenience.
newtype Key = Key BSC.ByteString
  deriving newtype (IsString, Eq, Ord, Show)

mkKey :: BSC.ByteString -> Key
mkKey = Key

keyFromText :: T.Text -> Key
keyFromText = Key . T.encodeUtf8

-- | Public-facing type for a JSON pointer. Use OverloadedStrings for convenience.
newtype Pointer = Pointer BSC.ByteString
  deriving newtype (IsString, Eq, Ord, Show)

mkPointer :: BSC.ByteString -> Pointer
mkPointer = Pointer

withParserPointer :: ForeignPtr SIMDParser -> (Parser -> Decoder a) -> Decoder a
withParserPointer parserFPtr f =
  withForeignPtr parserFPtr $ f . Parser

withDocumentPointer :: ForeignPtr SIMDDocument -> (Document -> Decoder a) -> Decoder a
withDocumentPointer docFPtr f =
  withForeignPtr docFPtr $ f . Document

withDocument :: (Value -> Decoder a) -> InputBuffer -> Decoder a
withDocument f inputPtr =
  allocaValue $ \valPtr -> Decoder $ withRunInIO $ \run ->
  alloca $ \errPtr -> run $ do
    docFPtr <- asks hDocument
    parserFPtr <- asks hParser
    runDecoder . withParserPointer parserFPtr $ \parserPtr ->
      withDocumentPointer docFPtr $ \docPtr -> do
        liftIO $ getIterator parserPtr inputPtr docPtr errPtr
        handleError "" errPtr
        liftIO $ getDocumentValueImpl docPtr valPtr errPtr
        handleError "" errPtr
        f valPtr

-- | Decode a value at the particular JSON pointer following RFC 6901.
-- Be careful where you use this because it rewinds the document on each
-- successive call.
atPointer :: Pointer -> (Value -> Decoder a) -> Decoder a
atPointer (Pointer jptr) f = Decoder $ do
  docFPtr <- asks hDocument
  runDecoder . withDocumentPointer docFPtr $ \docPtr ->
    withRunInIO $ \run ->
      Unsafe.unsafeUseAsCStringLen jptr $ \(cstr, len) -> run $
        allocaValue $ \vPtr ->
          alloca $ \errPtr -> withPath jptr $ do
          liftIO $ atPointerImpl cstr len docPtr vPtr errPtr
          handleError "" errPtr
          f vPtr

-- | Helper to work with an Object parsed from a Value.
withObject :: (Object -> Decoder a) -> Value -> Decoder a
withObject f valPtr =
  allocaObject $ \oPtr -> withRunInIO $ \run ->
  alloca $ \errPtr -> run $ do
    liftIO $ getObjectFromValueImpl valPtr oPtr errPtr
    handleError (typePrefix "object") errPtr
    f oPtr

-- | Helper to work with an ObjectIter started from an Object.
withObjectIter :: (ObjectIter -> Decoder a) -> Object -> Decoder a
withObjectIter f objPtr = withRunInIO $ \run ->
  alloca $ \errPtr -> run $
  allocaObjectIter $ \iterPtr -> do
    liftIO $ getObjectIterImpl objPtr iterPtr errPtr
    handleError "" errPtr
    f iterPtr

-- | Execute a function on each Field in an ObjectIter and
-- accumulate key-value tuples into a list.
iterateOverFields :: (Text -> Decoder a) -> (Value -> Decoder b) -> ObjectIter -> Decoder [(a, b)]
iterateOverFields fk fv iterPtr =
  withRunInIO $ \runInIO ->
  alloca $ \errPtr ->
  alloca $ \lenPtr ->
  alloca $ \keyPtr -> runInIO $
  allocaValue $ \valPtr ->
  go DList.empty keyPtr lenPtr valPtr errPtr
  where
    go !acc !keyPtr !lenPtr !valPtr !errPtr = do
      isOver <- fmap toBool . liftIO $ objectIterIsDoneImpl iterPtr
      if not isOver
        then do
            liftIO $ objectIterGetCurrentImpl iterPtr keyPtr lenPtr valPtr errPtr
            handleError "" errPtr
            kLen <- fmap fromIntegral . liftIO $ peek lenPtr
            kStr <- liftIO $ peek keyPtr
            keyTxt <- parseText (kStr, kLen)
            withPath (dot $ T.encodeUtf8 keyTxt) $ do
              k <- fk keyTxt
              v <- fv valPtr
              liftIO $ objectIterMoveNextImpl iterPtr
              removePath (dot $ T.encodeUtf8 keyTxt) $
                go (acc <> DList.singleton (k, v)) keyPtr lenPtr valPtr errPtr
        else
          pure $ DList.toList acc

withUnorderedField :: (Value -> Decoder a) -> Object -> ByteString -> Decoder a
withUnorderedField f objPtr key = withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen key $ \(cstr, len) -> run $
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> withPath (dot key) $ do
    liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr errPtr
    handleError "" errPtr
    f vPtr
{-# INLINE withUnorderedField #-}

withUnorderedOptionalField :: (Value -> Decoder a) -> Object -> ByteString -> Decoder (Maybe a)
withUnorderedOptionalField f objPtr key = withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen key $ \(cstr, len) -> run $
  allocaValue $ \vPtr ->
  alloca $ \errPtr -> withPath (dot key) $ do
    liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr errPtr
    errCode <- toEnum . fromIntegral <$> liftIO (peek errPtr)
    if | errCode == SUCCESS       -> Just <$> f vPtr
       | errCode == NO_SUCH_FIELD -> pure Nothing
       | otherwise                -> Nothing <$ handleError "" errPtr
{-# INLINE withUnorderedOptionalField #-}

withField :: (Value -> Decoder a) -> Object -> ByteString -> Decoder a
withField f objPtr key = withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen key $ \(cstr, len) -> run $
  allocaValue $ \vPtr ->
    alloca $ \errPtr -> withPath (dot key) $ do
    liftIO $ findFieldImpl objPtr cstr len vPtr errPtr
    handleError "" errPtr
    f vPtr
{-# INLINE withField #-}

withPath :: ByteString -> Decoder a -> Decoder a
withPath key d =
  Decoder . local (\st -> st { hPath = hPath st <> key }) $ runDecoder d
{-# INLINE withPath #-}

removePath :: ByteString -> Decoder a -> Decoder a
removePath key d =
  Decoder . local
    (\st -> st {
      hPath = fromMaybe (hPath st) (BSC.stripSuffix key $ hPath st)
    }) $ runDecoder d
{-# INLINE removePath #-}

withPathIndex :: Int -> Decoder a -> Decoder a
withPathIndex idx =
  Decoder . local
    (\st -> st {
      hPath = fromMaybe (hPath st) (BSC.stripSuffix (showInt $ max 0 (idx - 1)) $ hPath st)
           <> showInt idx
    }) . runDecoder
  where showInt i = dot . BSC.pack $ show i
{-# INLINE withPathIndex #-}

dot :: ByteString -> ByteString
dot = BSC.cons '/'
{-# INLINE dot #-}

decodeUtfOrThrow :: ByteString -> Decoder Text
decodeUtfOrThrow = either (fail . show) pure . T.decodeUtf8'
{-# INLINE decodeUtfOrThrow #-}

getInt :: Value -> Decoder Int
getInt valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    liftIO $ getIntImpl valPtr ptr errPtr
    handleError (typePrefix "int") errPtr
    liftIO $ peek ptr
{-# INLINE getInt #-}

-- | Helper to work with an Int parsed from a Value.
withInt :: (Int -> Decoder a) -> Value -> Decoder a
withInt f = getInt >=> f

getDouble :: Value -> Decoder Double
getDouble valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    liftIO $ getDoubleImpl valPtr ptr errPtr
    handleError (typePrefix "double") errPtr
    liftIO $ peek ptr
{-# INLINE getDouble #-}

-- | Helper to work with a Double parsed from a Value.
withDouble :: (Double -> Decoder a) -> Value -> Decoder a
withDouble f = getDouble >=> f

-- | Parse a Scientific using attoparsec's ByteString.Char8 parser.
parseScientific :: BSC.ByteString -> Decoder Sci.Scientific
parseScientific
  = either (\err -> fail $ "Failed to parse Scientific: " <> err) pure
  . A.parseOnly (A.scientific <* A.endOfInput)
{-# INLINE parseScientific #-}

getBool :: Value -> Decoder Bool
getBool valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ alloca $ \errPtr -> do
    liftIO $ getBoolImpl valPtr ptr errPtr
    handleError (typePrefix "bool") errPtr
    fmap toBool . liftIO $ peek ptr
{-# INLINE getBool #-}

-- | Helper to work with a Bool parsed from a Value.
withBool :: (Bool -> Decoder a) -> Value -> Decoder a
withBool f = getBool >=> f

fromCStringLen :: Text -> (CStringLen -> Decoder a) -> Value -> Decoder a
fromCStringLen lbl f valPtr = withRunInIO $ \run ->
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $
  alloca $ \errPtr -> do
    liftIO $ getStringImpl valPtr strPtr lenPtr errPtr
    handleError (typePrefix lbl) errPtr
    len <- fmap fromIntegral . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    f (str, len)
{-# INLINE fromCStringLen #-}

getString :: Value -> Decoder String
getString = fromCStringLen "string" (liftIO . peekCStringLen)
{-# INLINE getString #-}

getText :: Value -> Decoder Text
getText = fromCStringLen "text" parseText
{-# INLINE getText #-}

parseText :: CStringLen -> Decoder Text
parseText cstr =
  withRunInIO $ \_ ->
    T.peekCStringLen cstr
      `catch` \(err :: T.UnicodeException) -> fail $ show err
{-# INLINE parseText #-}

getRawByteString :: Value -> Decoder BSC.ByteString
getRawByteString valPtr = withRunInIO $ \run ->
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $ do
    liftIO $ getRawJSONTokenImpl valPtr strPtr lenPtr
    len <- fmap fromIntegral . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    liftIO $ BSC.packCStringLen (str, len)
{-# INLINE getRawByteString #-}

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
    handleError (typePrefix "array") errPtr
    f arrPtr

-- | Helper to work with an ArrayIter started from an Array.
withArrayIter :: (ArrayIter -> Decoder a) -> Array -> Decoder a
withArrayIter f arrPtr = withRunInIO $ \run ->
  alloca $ \errPtr -> run $
  allocaArrayIter $ \iterPtr -> do
    liftIO $ getArrayIterImpl arrPtr iterPtr errPtr
    handleError "" errPtr
    f iterPtr

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a list.
iterateOverArray :: (Value -> Decoder a) -> ArrayIter -> Decoder [a]
iterateOverArray f iterPtr =
  withRunInIO $ \runInIO ->
  alloca $ \errPtr -> runInIO $
  allocaValue $ \valPtr ->
  go (0 :: Int) DList.empty valPtr errPtr
  where
    go !n !acc !valPtr !errPtr = do
      isOver <- fmap toBool . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then withPathIndex n $ do
          liftIO $ arrayIterGetCurrentImpl iterPtr valPtr errPtr
          handleError "" errPtr
          result <- f valPtr
          liftIO $ arrayIterMoveNextImpl iterPtr
          go (n + 1) (acc <> DList.singleton result) valPtr errPtr
        else
          pure $ DList.toList acc

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
atKey :: Key -> (Value -> Decoder a) -> Object -> Decoder a
atKey (Key key) parser obj = withUnorderedField parser obj key

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atOptionalKey :: Key -> (Value -> Decoder a) -> Object -> Decoder (Maybe a)
atOptionalKey (Key key) parser obj = withUnorderedOptionalField parser obj key

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atOrderedKey :: Key -> (Value -> Decoder a) -> Object -> Decoder a
atOrderedKey (Key key) parser obj = withField parser obj key

-- | Parse a homogenous JSON array into a Haskell list.
list :: (Value -> Decoder a) -> Value -> Decoder [a]
list f val =
  flip withArray val $ \arr ->
  flip withArrayIter arr $ iterateOverArray f

-- | Parse an object into a homogenous list of key-value tuples.
objectAsKeyValues
  :: (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> (Value -> Decoder v)
  -- ^ Decoder for the field value.
  -> Value
  -> Decoder [(k, v)]
objectAsKeyValues kf vf val =
  flip withObject val $ \obj ->
  flip withObjectIter obj $ iterateOverFields kf vf

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
          fail "Expected a single character"

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

-- | Parse a Scientific from a Value.
scientific :: Value -> Decoder Sci.Scientific
scientific = withRawByteString parseScientific

-- | Run an attoparsec text parser as a hermes decoder.
runAtto :: AT.Parser a -> Text -> Decoder a
runAtto p t =
  case AT.parseOnly (p <* AT.endOfInput) t of
    Left err -> fail $ "could not parse date: " <> err
    Right r  -> pure r
{-# INLINE runAtto #-}

-- | ISO 8601 Compatibility

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
day :: Value -> Decoder Time.Day
day = withText $ runAtto ATime.day

-- | Parse a date of the form @[+,-]YYYY-MM@.
month :: Value -> Decoder Time.Month
month = withText $ runAtto ATime.month

-- | Parse a date of the form @[+,-]YYYY-QN@.
quarter :: Value -> Decoder Time.Quarter
quarter = withText $ runAtto ATime.quarter

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Value -> Decoder Local.TimeOfDay
timeOfDay = withText $ runAtto ATime.timeOfDay

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Value -> Decoder (Maybe Local.TimeZone)
timeZone = withText $ runAtto ATime.timeZone

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@. The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Value -> Decoder Local.LocalTime
localTime = withText $ runAtto ATime.localTime

-- | Behaves as 'zonedTime', but converts any time zone offset into a UTC time.
utcTime :: Value -> Decoder Time.UTCTime
utcTime = withText $ runAtto ATime.utcTime

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Value -> Decoder Local.ZonedTime
zonedTime = withText $ runAtto ATime.zonedTime

-- Decoding Types and Functions

-- | A Decoder is intended to be run in IO and will throw exceptions.
newtype Decoder a = Decoder { runDecoder :: ReaderT HermesEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance Alternative Decoder where
  empty = fail "Unspecified error"
  ad <|> bd = ad `catch` (\(_err :: HermesException) -> bd)

instance MonadFail Decoder where
  fail = throwHermes . T.pack

-- | Contains foreign references to the allocated simdjson::parser
-- and simdjson::document. Also maintains a path string that is updated
-- when an object field is entered and which is displayed in errors.
data HermesEnv =
  HermesEnv
    { hParser   :: !(ForeignPtr SIMDParser)
    , hDocument :: !(ForeignPtr SIMDDocument)
    , hPath     :: !ByteString
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
  let maxCap = 4000000000; -- 4GB
  ptr <- parserInit . toEnum $ fromMaybe maxCap mCap
  newForeignPtr parserDestroy ptr

mkSIMDDocument :: IO (ForeignPtr SIMDDocument)
mkSIMDDocument = mask_ $ do
  ptr <- makeDocumentImpl
  newForeignPtr deleteDocumentImpl ptr

mkSIMDPaddedStrView :: ByteString -> IO (ForeignPtr PaddedStringView)
mkSIMDPaddedStrView input = mask_ $
  Unsafe.unsafeUseAsCStringLen input $ \(cstr, len) -> do
    ptr <- makeInputViewImpl cstr (fromIntegral len)
    newForeignPtr deleteInputViewImpl ptr

-- | Construct a simdjson:padded_string_view from a Haskell `ByteString`, and pass
-- it to a monadic action. The instance is destroyed via the `bracket` pattern.
withInputBuffer :: MonadUnliftIO m => ByteString -> (InputBuffer -> m a) -> m a
withInputBuffer bs f =
  bracket acquire release $ \fPtr -> withForeignPtr fPtr $ f . InputBuffer
  where
    acquire = liftIO $ mkSIMDPaddedStrView bs
    release = liftIO . finalizeForeignPtr

-- | Internal finalizer for simdjson instances.
cleanupHermesEnv :: HermesEnv -> IO ()
cleanupHermesEnv hEnv = do
  finalizeForeignPtr (hDocument hEnv)
  finalizeForeignPtr (hParser hEnv)

-- | Run an action that is passed a `HermesEnv`.
-- The simdjson instances are created and destroyed using the `bracket` pattern.
withHermesEnv :: MonadUnliftIO m => (HermesEnv -> m a) -> m a
withHermesEnv = bracket acquire release
  where
    acquire = liftIO mkHermesEnv_
    release = liftIO . cleanupHermesEnv

-- | Construct a `HermesEnv` and use it to run a `Decoder` in IO.
-- This calls `withDocument` which gives us the initial `Value`, so this
-- cannot currently decode scalar documents due to simdjson constraints.
-- There is a small performance penalty for creating and destroying the simdjson
-- instances on each decode, so this is not recommended for running in tight loops.
decode :: (Value -> Decoder a) -> ByteString -> IO a
decode d bs =
  withHermesEnv $ \hEnv ->
    withInputBuffer bs $ \input ->
      flip runReaderT hEnv . runDecoder $ withDocument d input

-- | Helper that wraps `decode` and catches any IO exceptions before converting IO to Either.
decodeEither :: (Value -> Decoder a) -> ByteString -> Either HermesException a
decodeEither d = Unsafe.unsafePerformIO . try . decode d
