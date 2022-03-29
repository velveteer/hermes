{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Hermes.Decoder.Value
  ( atKey
  , atKeyOptional
  , atKeyStrict
  , atPointer
  , bool
  , char
  , double
  , int
  , list
  , nullable
  , objectAsKeyValues
  , scientific
  , string
  , text
  , listOfDouble
  , listOfInt
  , isNull
  , withArray
  , withBool
  , withDocumentValue
  , withDouble
  , withInt
  , withObject
  , withRawByteString
  , withString
  , withText
  ) where

import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.IO.Unlift (withRunInIO)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC (scientific)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.DList as DList
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if MIN_VERSION_text(2,0,0)
import qualified Data.Text.Foreign as T
#endif
import           UnliftIO.Foreign
  ( CStringLen
  , alloca
  , peek
  , peekArray
  , peekCStringLen
  , toBool
  )
import qualified UnliftIO.Foreign as Foreign

import           Data.Hermes.Decoder.Path
import           Data.Hermes.Decoder.Types
import           Data.Hermes.SIMDJSON

-- | Parse the given input into a document iterator, get its
-- Value, which is either a JSON object or an array, and run the given
-- action on that Value.
withDocumentValue :: (Value -> Decoder a) -> InputBuffer -> Decoder a
withDocumentValue f inputPtr =
  allocaValue $ \valPtr ->
  withParserPointer $ \parserPtr ->
  withDocumentPointer $ \docPtr -> do
    err <- liftIO $ getDocumentValueImpl parserPtr inputPtr docPtr valPtr
    handleErrorCode "" err
    f valPtr

-- | Decode a value at the particular JSON pointer following RFC 6901.
-- Be careful where you use this because it rewinds the document on each
-- successive call.
atPointer :: Text -> (Value -> Decoder a) -> Decoder a
atPointer jptr f =
  withDocumentPointer $ \docPtr ->
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
{-# INLINE withObjectIter #-}

-- | Execute a function on each Field in an ObjectIter and
-- accumulate key-value tuples into a list.
iterateOverFields :: (Text -> Decoder a) -> (Value -> Decoder b) -> ObjectIter -> Decoder [(a, b)]
iterateOverFields fk fv iterPtr = withRunInIO $ \runInIO ->
  alloca $ \lenPtr ->
  alloca $ \keyPtr -> runInIO $
  allocaValue $ \valPtr ->
  go DList.empty keyPtr lenPtr valPtr
  where
    go !acc keyPtr lenPtr valPtr = do
      isOver <- fmap toBool . liftIO $ objectIterIsDoneImpl iterPtr
      if not isOver
        then do
          err <- liftIO $ objectIterGetCurrentImpl iterPtr keyPtr lenPtr valPtr
          handleErrorCode "" err
          kLen <- fmap fromIntegral . liftIO $ peek lenPtr
          kStr <- liftIO $ peek keyPtr
          keyTxt <- parseTextFromCStrLen (kStr, kLen)
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
  . BSC.strip
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

withCStringLen :: Text -> (CStringLen -> Decoder a) -> Value -> Decoder a
withCStringLen lbl f valPtr = withRunInIO $ \run ->
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $ do
    err <- liftIO $ getStringImpl valPtr strPtr lenPtr
    handleErrorCode (typePrefix lbl) err
    len <- fmap fromIntegral . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    f (str, len)
{-# INLINE withCStringLen #-}

getString :: Value -> Decoder String
getString = withCStringLen "string" (liftIO . peekCStringLen)
{-# INLINE getString #-}

getText :: Value -> Decoder Text
getText = withCStringLen "text" parseTextFromCStrLen
{-# INLINE getText #-}

#if MIN_VERSION_text(2,0,0)
parseTextFromCStrLen :: CStringLen -> Decoder Text
parseTextFromCStrLen (cstr, len) = liftIO $ T.fromPtr (Foreign.castPtr cstr) (fromIntegral len)
{-# INLINE parseTextFromCStrLen #-}
#else

parseTextFromCStrLen :: CStringLen -> Decoder Text
parseTextFromCStrLen cstr = do
  bs <- liftIO $ Unsafe.unsafePackCStringLen cstr
  case A.parseOnly asciiTextAtto bs of
    Left err       -> fail $ "Could not parse text: " <> err
    Right Nothing  -> pure $! T.decodeUtf8 bs
    Right (Just r) -> pure $! r
{-# INLINE parseTextFromCStrLen #-}

asciiTextAtto :: A.Parser (Maybe Text)
asciiTextAtto = do
  s <- A.takeWhile (\w -> w /= 92 && w >= 0x20 && w < 0x80)
  let txt = T.decodeLatin1 s
  mw <- A.peekWord8
  case mw of
    Nothing -> pure $ Just txt
    _       -> pure Nothing
{-# INLINE asciiTextAtto #-}
#endif

getRawByteString :: Value -> Decoder BS.ByteString
getRawByteString valPtr = withRunInIO $ \run ->
  alloca $ \strPtr ->
  alloca $ \lenPtr -> run $ do
    liftIO $ getRawJSONTokenImpl valPtr strPtr lenPtr
    len <- fmap fromIntegral . liftIO $ peek lenPtr
    str <- liftIO $ peek strPtr
    liftIO $ Unsafe.unsafePackCStringLen (str, len)
{-# INLINE getRawByteString #-}

-- | Helper to work with the raw ByteString of the JSON token parsed from the given Value.
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

-- | Helper to work with an Array and its length parsed from a Value.
withArrayLen :: ((Array, Int) -> Decoder a) -> Value -> Decoder a
withArrayLen f val =
  allocaArray $ \arrPtr ->
  withRunInIO $ \run ->
  alloca $ \outLen -> run $ do
    err <- liftIO $ getArrayLenFromValueImpl val arrPtr outLen
    handleErrorCode (typePrefix "array") err
    len <- fmap fromIntegral . liftIO $ peek outLen
    f (arrPtr, len)
{-# INLINE withArrayLen #-}

-- | Is more efficient by looping in C++ instead of Haskell.
listOfInt :: Value -> Decoder [Int]
listOfInt =
  withArrayLen $ \(arrPtr, len) ->
  Foreign.allocaArray len $ \out -> do
    err <- liftIO $ intArrayImpl arrPtr out
    handleErrorCode "Error decoding array of ints." err
    liftIO $ peekArray len out
{-# RULES "list int/listOfInt" list int = listOfInt #-}

-- | Is more efficient by looping in C++ instead of Haskell.
listOfDouble :: Value -> Decoder [Double]
listOfDouble =
  withArrayLen $ \(arrPtr, len) ->
  Foreign.allocaArray len $ \out -> do
    err <- liftIO $ doubleArrayImpl arrPtr out
    handleErrorCode "Error decoding array of doubles." err
    liftIO $ peekArray len out
{-# RULES "list double/listOfDouble" list double = listOfDouble #-}

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
{-# INLINE withArrayIter #-}

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a list.
iterateOverArray :: (Value -> Decoder a) -> ArrayIter -> Decoder [a]
iterateOverArray f iterPtr =
  allocaValue $ \valPtr -> go (0 :: Int) DList.empty valPtr
  where
    go !n !acc valPtr = do
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
atKey :: Text -> (Value -> Decoder a) -> Object -> Decoder a
atKey key parser obj = withUnorderedField parser obj key

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atKeyOptional :: Text -> (Value -> Decoder a) -> Object -> Decoder (Maybe a)
atKeyOptional key parser obj = withUnorderedOptionalField parser obj key

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atKeyStrict :: Text -> (Value -> Decoder a) -> Object -> Decoder a
atKeyStrict key parser obj = withField parser obj key

-- | Parse a homogenous JSON array into a Haskell list.
list :: (Value -> Decoder a) -> Value -> Decoder [a]
list f = withArrayIter $ iterateOverArray f
{-# INLINE[2] list #-}

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

-- | Parse a JSON boolean into a Haskell Bool.
bool :: Value -> Decoder Bool
bool = getBool

-- | Parse a JSON number into an unsigned Haskell Int.
int :: Value -> Decoder Int
int = getInt
{-# INLINE[2] int #-}

-- | Parse a JSON number into a Haskell Double.
double :: Value -> Decoder Double
double = getDouble
{-# INLINE[2] double #-}

-- | Parse a Scientific from a Value.
scientific :: Value -> Decoder Sci.Scientific
scientific = withRawByteString parseScientific
