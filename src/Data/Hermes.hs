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
import           Control.Monad.Reader (MonadReader, asks, local)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC (scientific)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Time as ATime
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.DList as DList
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
-- | Phantom type for a pointer to simdjson::padded_string.
data PaddedString

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

foreign import ccall unsafe "make_input" makeInputImpl
  :: CString -> CSize -> IO (Ptr PaddedString)

foreign import ccall unsafe "&delete_input" deleteInputImpl
  :: FunPtr (Ptr PaddedString -> IO ())

-- Document parsers
foreign import ccall unsafe "get_iterator" getIterator
  :: Parser -> InputBuffer -> Document -> IO CInt

foreign import ccall unsafe "get_document_value" getDocumentValueImpl
  :: Document -> Value -> IO CInt

foreign import ccall unsafe "at_pointer" atPointerImpl
  :: CString -> Int -> Document -> Value -> IO CInt

foreign import ccall unsafe "get_object_from_value" getObjectFromValueImpl
  :: Value -> Object -> IO CInt

foreign import ccall unsafe "get_object_iter_from_value" getObjectIterFromValueImpl
  :: Value -> ObjectIter -> IO CInt

foreign import ccall unsafe "obj_iter_is_done" objectIterIsDoneImpl
  :: ObjectIter -> IO CBool

foreign import ccall unsafe "obj_iter_get_current" objectIterGetCurrentImpl
  :: ObjectIter -> Ptr CString -> Ptr CSize -> Value -> IO CInt

foreign import ccall unsafe "obj_iter_move_next" objectIterMoveNextImpl
  :: ObjectIter -> IO ()

foreign import ccall unsafe "get_array_from_value" getArrayFromValueImpl
  :: Value -> Array -> IO CInt

foreign import ccall unsafe "get_array_iter_from_value" getArrayIterFromValueImpl
  :: Value -> ArrayIter -> IO CInt

foreign import ccall unsafe "arr_iter_is_done" arrayIterIsDoneImpl
  :: ArrayIter -> IO CBool

foreign import ccall unsafe "arr_iter_get_current" arrayIterGetCurrentImpl
  :: ArrayIter -> Value -> IO CInt

foreign import ccall unsafe "arr_iter_move_next" arrayIterMoveNextImpl
  :: ArrayIter -> IO ()

foreign import ccall unsafe "find_field_unordered" findFieldUnorderedImpl
  :: Object -> CString -> Int -> Value -> IO CInt

foreign import ccall unsafe "find_field" findFieldImpl
  :: Object -> CString -> Int -> Value -> IO CInt

-- Helpers
foreign import ccall unsafe "get_error_message" getErrorMessageImpl
  :: CInt -> IO CString

foreign import ccall unsafe "current_location" currentLocationImpl
  :: Document -> Ptr CString -> IO CInt

foreign import ccall unsafe "to_debug_string" toDebugStringImpl
  :: Document -> CString -> Ptr CSize -> IO ()

foreign import ccall unsafe "is_null" isNullImpl
  :: Value -> IO CBool

-- Primitives
foreign import ccall unsafe "get_int" getIntImpl
  :: Value -> Ptr Int -> IO CInt

foreign import ccall unsafe "get_double" getDoubleImpl
  :: Value -> Ptr Double -> IO CInt

foreign import ccall unsafe "get_string" getStringImpl
  :: Value -> Ptr CString -> Ptr CSize -> IO CInt

foreign import ccall unsafe "get_bool" getBoolImpl
  :: Value -> Ptr CBool -> IO CInt

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
throwSIMD :: SIMDErrorCode -> Text -> Decoder a
throwSIMD errCode msg = buildHError (Just errCode) msg >>= liftIO . throwIO . SIMDException

-- | Throw an IO exception in the `Decoder` context.
throwHermes :: Text -> Decoder a
throwHermes msg = buildHError Nothing msg >>= liftIO . throwIO . InternalException

typePrefix :: Text -> Text
typePrefix typ = "Error while getting value of type " <> typ <> ". "

buildHError :: Maybe SIMDErrorCode -> Text -> Decoder HError
buildHError mErrCode msg = do
  docFPtr <- asks hDocument
  pth <- asks hPath
  case mErrCode of
    Just c
      | c `elem`
      [ EMPTY
      , INSUFFICIENT_PADDING
      , SCALAR_DOCUMENT_AS_VALUE
      , UTF8_ERROR
      , UNCLOSED_STRING
      , UNESCAPED_CHARS
      ]
      -> pure $ HError pth msg "" ""
    _
      ->
      withRunInIO $ \run ->
      alloca $ \locStrPtr -> run $
      alloca $ \lenPtr ->
      withDocumentPointer docFPtr $ \docPtr -> do
        err <- liftIO $ currentLocationImpl docPtr locStrPtr
        let errCode = toEnum $ fromIntegral err
        if errCode == OUT_OF_BOUNDS
          then pure $ HError pth msg "out of bounds" ""
          else allocaBytes 128 $ \dbStrPtr -> do
            locStr <- liftIO $ peekCString =<< peek locStrPtr
            liftIO $ toDebugStringImpl docPtr dbStrPtr lenPtr
            len <- fmap fromIntegral . liftIO $ peek lenPtr
            debugStr <- peekCStringLen (dbStrPtr, len)
            pure $ HError pth msg (T.pack $ Prelude.take 20 locStr) (T.pack debugStr)

handleErrorCode :: Text -> CInt -> Decoder ()
handleErrorCode pre errInt = do
  let errCode = toEnum $ fromIntegral errInt
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- peekCString =<< liftIO (getErrorMessageImpl errInt)
    throwSIMD errCode $ pre <> T.pack errStr

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

-- | A reference to an opaque simdjson::ondemand::object_iterator.
newtype ObjectIter = ObjectIter (Ptr JSONObjectIter)

-- | Public-facing type for object keys. Use OverloadedStrings for convenience.
newtype Key = Key Text
  deriving newtype (IsString, Eq, Ord, Show)

mkKey :: Text -> Key
mkKey = Key

-- | Public-facing type for a JSON pointer. Use OverloadedStrings for convenience.
newtype Pointer = Pointer Text
  deriving newtype (IsString, Eq, Ord, Show)

mkPointer :: Text -> Pointer
mkPointer = Pointer

withParserPointer :: ForeignPtr SIMDParser -> (Parser -> Decoder a) -> Decoder a
withParserPointer parserFPtr f =
  withForeignPtr parserFPtr $ f . Parser

withDocumentPointer :: ForeignPtr SIMDDocument -> (Document -> Decoder a) -> Decoder a
withDocumentPointer docFPtr f =
  withForeignPtr docFPtr $ f . Document

withDocument :: (Value -> Decoder a) -> InputBuffer -> Decoder a
withDocument f inputPtr = allocaValue $ \valPtr -> do
  docFPtr <- asks hDocument
  parserFPtr <- asks hParser
  withParserPointer parserFPtr $ \parserPtr ->
    withDocumentPointer docFPtr $ \docPtr -> do
      err <- liftIO $ getIterator parserPtr inputPtr docPtr
      handleErrorCode "" err
      err' <- liftIO $ getDocumentValueImpl docPtr valPtr
      handleErrorCode "" err'
      f valPtr

-- | Decode a value at the particular JSON pointer following RFC 6901.
-- Be careful where you use this because it rewinds the document on each
-- successive call.
atPointer :: Pointer -> (Value -> Decoder a) -> Decoder a
atPointer (Pointer jptr) f = asks hDocument >>= \docFPtr ->
  withDocumentPointer docFPtr $ \docPtr ->
  withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 jptr) $ \(cstr, len) -> run $
  allocaValue $ \vPtr -> withPath jptr $ do
    err <- liftIO $ atPointerImpl cstr len docPtr vPtr
    handleErrorCode "" err
    f vPtr

-- | Helper to work with an Object parsed from a Value.
withObject :: (Object -> Decoder a) -> Value -> Decoder a
withObject f valPtr = allocaObject $ \oPtr -> do
  err <- liftIO $ getObjectFromValueImpl valPtr oPtr
  handleErrorCode (typePrefix "object") err
  f oPtr

-- | Helper to work with an ObjectIter started from a Value assumed to be an Object.
withObjectIter :: (ObjectIter -> Decoder a) -> Value -> Decoder a
withObjectIter f valPtr = allocaObjectIter $ \iterPtr -> do
  err <- liftIO $ getObjectIterFromValueImpl valPtr iterPtr
  handleErrorCode "" err
  f iterPtr

-- | Execute a function on each Field in an ObjectIter and
-- accumulate key-value tuples into a list.
iterateOverFields :: (Text -> Decoder a) -> (Value -> Decoder b) -> ObjectIter -> Decoder [(a, b)]
iterateOverFields fk fv iterPtr = withRunInIO $ \runInIO ->
  alloca $ \lenPtr ->
  alloca $ \keyPtr -> runInIO $
  allocaValue $ \valPtr ->
  go DList.empty keyPtr lenPtr valPtr
  where
    {-# INLINE go #-}
    go !acc !keyPtr !lenPtr !valPtr = do
      isOver <- fmap toBool . liftIO $ objectIterIsDoneImpl iterPtr
      if not isOver
        then do
          err <- liftIO $ objectIterGetCurrentImpl iterPtr keyPtr lenPtr valPtr
          handleErrorCode "" err
          kLen <- fmap fromIntegral . liftIO $ peek lenPtr
          kStr <- liftIO $ peek keyPtr
          keyTxt <- parseText (kStr, kLen)
          withPath (dot keyTxt) $ do
            k <- fk keyTxt
            v <- fv valPtr
            liftIO $ objectIterMoveNextImpl iterPtr
            removePath (dot keyTxt) $
              go (acc <> DList.singleton (k, v)) keyPtr lenPtr valPtr
        else
          pure $ DList.toList acc

withUnorderedField :: (Value -> Decoder a) -> Object -> Text -> Decoder a
withUnorderedField f objPtr key = withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) -> run $
  allocaValue $ \vPtr -> withPath (dot key) $ do
    err <- liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr
    handleErrorCode "" err
    f vPtr
{-# INLINE withUnorderedField #-}

withUnorderedOptionalField :: (Value -> Decoder a) -> Object -> Text -> Decoder (Maybe a)
withUnorderedOptionalField f objPtr key = withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) -> run $
  allocaValue $ \vPtr -> withPath (dot key) $ do
    err <- liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr
    let errCode = toEnum $ fromIntegral err
    if | errCode == SUCCESS       -> Just <$> f vPtr
       | errCode == NO_SUCH_FIELD -> pure Nothing
       | otherwise                -> Nothing <$ handleErrorCode "" err
{-# INLINE withUnorderedOptionalField #-}

withField :: (Value -> Decoder a) -> Object -> Text -> Decoder a
withField f objPtr key = withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) -> run $
  allocaValue $ \vPtr -> withPath (dot key) $ do
    err <- liftIO $ findFieldImpl objPtr cstr len vPtr
    handleErrorCode "" err
    f vPtr
{-# INLINE withField #-}

withPath :: Text -> Decoder a -> Decoder a
withPath key =
  local $ \st -> st { hPath = hPath st <> key }
{-# INLINE withPath #-}

removePath :: Text -> Decoder a -> Decoder a
removePath key =
  local $ \st -> st { hPath = fromMaybe (hPath st) (T.stripSuffix key $ hPath st) }
{-# INLINE removePath #-}

withPathIndex :: Int -> Decoder a -> Decoder a
withPathIndex idx =
  local $ \st -> st
    { hPath = fromMaybe (hPath st) (T.stripSuffix (showInt $ max 0 (idx - 1)) $ hPath st)
           <> showInt idx
    }
  where showInt i = dot . T.pack $ show i
{-# INLINE withPathIndex #-}

dot :: Text -> Text
dot = T.cons '/'
{-# INLINE dot #-}

getInt :: Value -> Decoder Int
getInt valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ do
    err <- liftIO $ getIntImpl valPtr ptr
    handleErrorCode (typePrefix "int") err
    liftIO $ peek ptr
{-# INLINE getInt #-}

-- | Helper to work with an Int parsed from a Value.
withInt :: (Int -> Decoder a) -> Value -> Decoder a
withInt f = getInt >=> f

getDouble :: Value -> Decoder Double
getDouble valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ do
    err <- liftIO $ getDoubleImpl valPtr ptr
    handleErrorCode (typePrefix "double") err
    liftIO $ peek ptr
{-# INLINE getDouble #-}

-- | Helper to work with a Double parsed from a Value.
withDouble :: (Double -> Decoder a) -> Value -> Decoder a
withDouble f = getDouble >=> f

-- | Parse a Scientific using attoparsec's ByteString.Char8 parser.
parseScientific :: BS.ByteString -> Decoder Sci.Scientific
parseScientific
  = either (\err -> fail $ "Failed to parse Scientific: " <> err) pure
  . A.parseOnly (AC.scientific <* A.endOfInput)
{-# INLINE parseScientific #-}

getBool :: Value -> Decoder Bool
getBool valPtr = withRunInIO $ \run ->
  alloca $ \ptr -> run $ do
    err <- liftIO $ getBoolImpl valPtr ptr
    handleErrorCode (typePrefix "bool") err
    fmap toBool . liftIO $ peek ptr
{-# INLINE getBool #-}

-- | Helper to work with a Bool parsed from a Value.
withBool :: (Bool -> Decoder a) -> Value -> Decoder a
withBool f = getBool >=> f

fromCStringLen :: Text -> (CStringLen -> Decoder a) -> Value -> Decoder a
fromCStringLen lbl f valPtr = withRunInIO $ \run ->
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $ do
    err <- liftIO $ getStringImpl valPtr strPtr lenPtr
    handleErrorCode (typePrefix lbl) err
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
parseText cstr = do
  bs <- liftIO $ Unsafe.unsafePackCStringLen cstr
  case A.parseOnly latinTextAtto bs of
    Left err       -> fail $ "Could not parse text: " <> err
    Right Nothing  -> pure $! T.decodeUtf8 bs
    Right (Just r) -> pure $! r
{-# INLINE parseText #-}

latinTextAtto :: A.Parser (Maybe Text)
latinTextAtto = do
  s <- A.takeWhile (\w -> w /= 92 && w >= 0x20 && w < 0x80)
  let txt = T.decodeLatin1 s
  mw <- A.peekWord8
  case mw of
    Nothing -> pure $ Just txt
    _       -> pure Nothing
{-# INLINE latinTextAtto #-}

getRawByteString :: Value -> Decoder BS.ByteString
getRawByteString valPtr = withRunInIO $ \run ->
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $ do
    liftIO $ getRawJSONTokenImpl valPtr strPtr lenPtr
    len <- fmap fromIntegral . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    liftIO $ Unsafe.unsafePackCStringLen (str, len)
{-# INLINE getRawByteString #-}

-- | Helper to work with a raw ByteString.Char8 parsed from a Value.
withRawByteString :: (BS.ByteString -> Decoder a) -> Value -> Decoder a
withRawByteString f = getRawByteString >=> f
{-# INLINE withRawByteString #-}

-- | Helper to work with a String parsed from a Value.
withString :: (String -> Decoder a) -> Value -> Decoder a
withString f = getString >=> f

-- | Helper to work with a Text parsed from a Value.
withText :: (Text -> Decoder a) -> Value -> Decoder a
withText f = getText >=> f

-- | Returns True if the Value is null.
isNull :: Value -> Decoder Bool
isNull valPtr = fmap toBool . liftIO $ isNullImpl valPtr
{-# INLINE isNull #-}

-- | Helper to work with an Array parsed from a Value.
withArray :: (Array -> Decoder a) -> Value -> Decoder a
withArray f val =
  allocaArray $ \arrPtr -> do
    err <- liftIO $ getArrayFromValueImpl val arrPtr
    handleErrorCode (typePrefix "array") err
    f arrPtr

-- | Helper to work with an ArrayIter started from a Value assumed to be an Array.
withArrayIter :: (ArrayIter -> Decoder a) -> Value -> Decoder a
withArrayIter f valPtr =
  allocaArrayIter $ \iterPtr -> do
    err <- liftIO $ getArrayIterFromValueImpl valPtr iterPtr
    handleErrorCode "" err
    f iterPtr

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a list.
iterateOverArray :: (Value -> Decoder a) -> ArrayIter -> Decoder [a]
iterateOverArray f iterPtr =
  allocaValue $ \valPtr -> go (0 :: Int) DList.empty valPtr
  where
    {-# INLINE go #-}
    go !n !acc !valPtr = do
      isOver <- fmap toBool . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then withPathIndex n $ do
          err <- liftIO $ arrayIterGetCurrentImpl iterPtr valPtr
          handleErrorCode "" err
          result <- f valPtr
          liftIO $ arrayIterMoveNextImpl iterPtr
          go (n + 1) (acc <> DList.singleton result) valPtr
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
list f = withArrayIter $ iterateOverArray f

-- | Parse an object into a homogenous list of key-value tuples.
objectAsKeyValues
  :: (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> (Value -> Decoder v)
  -- ^ Decoder for the field value.
  -> Value
  -> Decoder [(k, v)]
objectAsKeyValues kf vf = withObjectIter $ iterateOverFields kf vf

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
runAttoDate :: AT.Parser a -> Text -> Decoder a
runAttoDate p t =
  case AT.parseOnly (p <* AT.endOfInput) t of
    Left err -> fail $ "Could not parse date: " <> err
    Right r  -> pure r
{-# INLINE runAttoDate #-}

-- | ISO 8601 Compatibility

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
day :: Value -> Decoder Time.Day
day = withText $ runAttoDate ATime.day

-- | Parse a date of the form @[+,-]YYYY-MM@.
month :: Value -> Decoder Time.Month
month = withText $ runAttoDate ATime.month

-- | Parse a date of the form @[+,-]YYYY-QN@.
quarter :: Value -> Decoder Time.Quarter
quarter = withText $ runAttoDate ATime.quarter

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Value -> Decoder Local.TimeOfDay
timeOfDay = withText $ runAttoDate ATime.timeOfDay

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Value -> Decoder (Maybe Local.TimeZone)
timeZone = withText $ runAttoDate ATime.timeZone

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@. The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Value -> Decoder Local.LocalTime
localTime = withText $ runAttoDate ATime.localTime

-- | Behaves as 'zonedTime', but converts any time zone offset into a UTC time.
utcTime :: Value -> Decoder Time.UTCTime
utcTime = withText $ runAttoDate ATime.utcTime

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
zonedTime = withText $ runAttoDate ATime.zonedTime

-- Decoding Types and Functions

-- | A Decoder is intended to be run in IO and will throw exceptions.
newtype Decoder a = Decoder { runDecoder :: ReaderT HermesEnv IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader HermesEnv
    , MonadUnliftIO
    )

instance Alternative Decoder where
  empty = fail "Unspecified error"
  ad <|> bd = ad `catch` (\(_err :: HermesException) -> bd)

instance MonadFail Decoder where
  {-# INLINE fail #-}
  fail = throwHermes . T.pack

-- | Contains foreign references to the allocated simdjson::parser
-- and simdjson::document. Also maintains a path string that is updated
-- when an object field is entered and which is displayed in errors.
data HermesEnv =
  HermesEnv
    { hParser   :: !(ForeignPtr SIMDParser)
    , hDocument :: !(ForeignPtr SIMDDocument)
    , hPath     :: !Text
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

mkSIMDPaddedStr :: ByteString -> IO (ForeignPtr PaddedString)
mkSIMDPaddedStr input = mask_ $
  Unsafe.unsafeUseAsCStringLen input $ \(cstr, len) -> do
    ptr <- makeInputImpl cstr (fromIntegral len)
    newForeignPtr deleteInputImpl ptr

-- | Construct a simdjson:padded_string from a Haskell `ByteString`, and pass
-- it to a monadic action. The instance lifetime is managed by the `bracket` function.
withInputBuffer :: MonadUnliftIO m => ByteString -> (InputBuffer -> m a) -> m a
withInputBuffer bs f =
  bracket acquire release $ \fPtr -> withForeignPtr fPtr $ f . InputBuffer
  where
    acquire = liftIO $ mkSIMDPaddedStr bs
    release = liftIO . finalizeForeignPtr

-- | Internal finalizer for simdjson instances.
cleanupHermesEnv :: HermesEnv -> IO ()
cleanupHermesEnv hEnv = do
  finalizeForeignPtr (hDocument hEnv)
  finalizeForeignPtr (hParser hEnv)

-- | Run an action that is passed a `HermesEnv`.
-- The simdjson instances are created and destroyed using the `bracket` function.
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
