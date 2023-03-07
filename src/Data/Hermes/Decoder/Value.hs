{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , getType
  , list
  , nullable
  , objectAsKeyValues
  , objectAsMap
  , scientific
  , string
  , text
  , listOfDouble
  , listOfInt
  , isNull
  , vector
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
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC (scientific)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as Unsafe
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.DList as DList
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if MIN_VERSION_text(2,0,0)
import qualified Data.Text.Foreign as T
import qualified Foreign.Ptr as F
#endif
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Foreign.C.String as F
import qualified Foreign.Marshal.Alloc as F
import qualified Foreign.Marshal.Array as F
import qualified Foreign.Marshal.Utils as F
import qualified Foreign.Storable as F

import           Data.Hermes.Decoder.Internal
import           Data.Hermes.Decoder.Path
import           Data.Hermes.SIMDJSON

-- | Parse the given input into a document iterator, get its Value, which is
-- either a JSON object or an array, and run the given action on that Value.
withDocumentValue :: (Value -> Decoder a) -> InputBuffer -> Decoder a
withDocumentValue f inputPtr = do
  parPtr <- asks hParser
  docPtr <- asks hDocument
  withRunInIO $ \run ->
    allocaValue $ \valPtr -> do
      err <- getDocumentValueImpl parPtr inputPtr docPtr valPtr
      run $ do
        handleErrorCode "" err
        f valPtr
{-# INLINE withDocumentValue #-}

-- | Decode a value at the particular JSON pointer following RFC 6901.
-- Be careful where you use this because it rewinds the document on each
-- successive call.
atPointer :: Text -> (Value -> Decoder a) -> Decoder a
atPointer jptr f = do
  docPtr <- asks hDocument
  withRunInIO $ \run ->
    Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 jptr) $ \(cstr, len) ->
      allocaValue $ \vPtr -> run . withPointer jptr $ do
        err <- liftIO $ atPointerImpl cstr len docPtr vPtr
        handleErrorCode "" err
        f vPtr

-- | Helper to work with an Object parsed from a Value.
withObject :: (Object -> Decoder a) -> Value -> Decoder a
withObject f valPtr =
  withRunInIO $ \ run ->
    allocaObject $ \oPtr -> do
      err <- getObjectFromValueImpl valPtr oPtr
      run $ do
        handleErrorCode (typePrefix "object") err
        f oPtr
{-# INLINE withObject #-}

-- | Helper to work with an ObjectIter started from a Value assumed to be an Object.
withObjectIter :: (ObjectIter -> Decoder a) -> Value -> Decoder a
withObjectIter f valPtr =
  withRunInIO $ \run ->
  allocaObjectIter $ \iterPtr -> do
    err <- getObjectIterFromValueImpl valPtr iterPtr
    run $ do
      handleErrorCode (typePrefix "object") err
      f iterPtr
{-# INLINE withObjectIter #-}

-- | Execute a function on each Field in an ObjectIter and accumulate into a `Map`.
iterateOverFieldsMap
  :: Ord a
  => (Text -> Decoder a)
  -> (Value -> Decoder b)
  -> ObjectIter
  -> Decoder (Map a b)
iterateOverFieldsMap fk fv iterPtr =
  withRunInIO $ \run ->
    F.alloca $ \lenPtr ->
      F.alloca $ \keyPtr ->
        allocaValue $ \valPtr -> run $ go M.empty keyPtr lenPtr valPtr
  where
    go !acc !keyPtr !lenPtr !valPtr = do
      isOver <- fmap F.toBool . liftIO $ objectIterIsDoneImpl iterPtr
      if not isOver
        then do
          err <- liftIO $ objectIterGetCurrentImpl iterPtr keyPtr lenPtr valPtr
          handleErrorCode "" err
          kLen <- fmap fromIntegral . liftIO $ F.peek lenPtr
          kStr <- liftIO $ F.peek keyPtr
          keyTxt <- parseTextFromCStrLen (kStr, kLen)
          (k, v)
            <-
              withKey keyTxt $ do
                k <- fk keyTxt
                v <- fv valPtr
                pure (k, v)
          liftIO $ objectIterMoveNextImpl iterPtr
          go (M.insert k v acc) keyPtr lenPtr valPtr
        else
          pure acc
{-# INLINE iterateOverFieldsMap #-}

-- | Execute a function on each Field in an ObjectIter and
-- accumulate key-value tuples into a list.
iterateOverFields
  :: (Text -> Decoder a)
  -> (Value -> Decoder b)
  -> ObjectIter
  -> Decoder [(a, b)]
iterateOverFields fk fv iterPtr =
  withRunInIO $ \run ->
    F.alloca $ \lenPtr ->
      F.alloca $ \keyPtr ->
        allocaValue $ \valPtr -> run $ go DList.empty keyPtr lenPtr valPtr
  where
    go !acc !keyPtr !lenPtr !valPtr = do
      isOver <- fmap F.toBool . liftIO $ objectIterIsDoneImpl iterPtr
      if not isOver
        then do
          err <- liftIO $ objectIterGetCurrentImpl iterPtr keyPtr lenPtr valPtr
          handleErrorCode "" err
          kLen <- fmap fromIntegral . liftIO $ F.peek lenPtr
          kStr <- liftIO $ F.peek keyPtr
          keyTxt <- parseTextFromCStrLen (kStr, kLen)
          kv
            <-
              withKey keyTxt $ do
                k <- fk keyTxt
                v <- fv valPtr
                pure (k, v)
          liftIO $ objectIterMoveNextImpl iterPtr
          go (acc <> DList.singleton kv) keyPtr lenPtr valPtr
        else
          pure $ DList.toList acc
{-# INLINE iterateOverFields #-}

withUnorderedField :: (Value -> Decoder a) -> Object -> Text -> Decoder a
withUnorderedField f objPtr key =
  withRunInIO $ \run ->
    Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) ->
      allocaValue $ \vPtr -> run $ withKey key $ do
        err <- liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr
        handleErrorCode "" err
        f vPtr
{-# INLINE withUnorderedField #-}

withUnorderedOptionalField :: (Value -> Decoder a) -> Object -> Text -> Decoder (Maybe a)
withUnorderedOptionalField f objPtr key =
  withRunInIO $ \run ->
  Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) ->
  allocaValue $ \vPtr -> run $ withKey key $ do
    err <- liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr
    let errCode = toEnum $ fromIntegral err
    if | errCode == SUCCESS       -> Just <$> f vPtr
       | errCode == NO_SUCH_FIELD -> pure Nothing
       | otherwise                -> Nothing <$ handleErrorCode "" err
{-# INLINE withUnorderedOptionalField #-}

withField :: (Value -> Decoder a) -> Object -> Text -> Decoder a
withField f objPtr key =
  withRunInIO $ \run ->
    Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) ->
      allocaValue $ \vPtr -> run $ withKey key $ do
        err <- liftIO $ findFieldImpl objPtr cstr len vPtr
        handleErrorCode "" err
        f vPtr
{-# INLINE withField #-}

getInt :: Value -> Decoder Int
getInt valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> run $ do
      err <- liftIO $ getIntImpl valPtr ptr
      handleErrorCode (typePrefix "int") err
      liftIO $ F.peek ptr
{-# INLINE getInt #-}

-- | Helper to work with an Int parsed from a Value.
withInt :: (Int -> Decoder a) -> Value -> Decoder a
withInt f = getInt >=> f

getDouble :: Value -> Decoder Double
getDouble valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> run $ do
      err <- liftIO $ getDoubleImpl valPtr ptr
      handleErrorCode (typePrefix "double") err
      liftIO $ F.peek ptr
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
getBool valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> run $ do
      err <- liftIO $ getBoolImpl valPtr ptr
      handleErrorCode (typePrefix "bool") err
      fmap F.toBool . liftIO $ F.peek ptr
{-# INLINE getBool #-}

-- | Helper to work with a Bool parsed from a Value.
withBool :: (Bool -> Decoder a) -> Value -> Decoder a
withBool f = getBool >=> f

withCStringLen :: Text -> (F.CStringLen -> Decoder a) -> Value -> Decoder a
withCStringLen lbl f valPtr =
  withRunInIO $ \run ->
    F.alloca $ \strPtr ->
      F.alloca $ \lenPtr -> run $ do
        err <- liftIO $ getStringImpl valPtr strPtr lenPtr
        handleErrorCode (typePrefix lbl) err
        len <- fmap fromIntegral . liftIO $ F.peek lenPtr
        str <- liftIO $ F.peek strPtr
        f (str, len)
{-# INLINE withCStringLen #-}

getString :: Value -> Decoder String
getString = withCStringLen "string" (liftIO . F.peekCStringLen)
{-# INLINE getString #-}

getText :: Value -> Decoder Text
getText = withCStringLen "text" parseTextFromCStrLen
{-# INLINE getText #-}

#if MIN_VERSION_text(2,0,0)
parseTextFromCStrLen :: F.CStringLen -> Decoder Text
parseTextFromCStrLen (cstr, len) = liftIO $ T.fromPtr (F.castPtr cstr) (fromIntegral len)
{-# INLINE parseTextFromCStrLen #-}
#else

parseTextFromCStrLen :: F.CStringLen -> Decoder Text
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
getRawByteString valPtr =
  liftIO $
    F.alloca $ \strPtr ->
      F.alloca $ \lenPtr -> do
        getRawJSONTokenImpl valPtr strPtr lenPtr
        len <- fmap fromIntegral $ F.peek lenPtr
        str <- F.peek strPtr
        Unsafe.unsafePackCStringLen (str, len)
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
isNull valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> do
      err <- isNullImpl valPtr ptr
      run $ do
        handleErrorCode "" err
        fmap F.toBool . liftIO $ F.peek ptr
{-# INLINE isNull #-}

-- | Helper to work with an Array and its length parsed from a Value.
withArrayLen :: ((Array, Int) -> Decoder a) -> Value -> Decoder a
withArrayLen f val =
  withRunInIO $ \run ->
  allocaArray $ \arrPtr ->
  F.alloca $ \outLen -> do
    err <- getArrayLenFromValueImpl val arrPtr outLen
    len <- fmap fromIntegral $ F.peek outLen
    run $ do
      handleErrorCode (typePrefix "array") err
      f (arrPtr, len)
{-# INLINE withArrayLen #-}

-- | Is more efficient by looping in C++ instead of Haskell.
listOfInt :: Value -> Decoder [Int]
listOfInt =
  withArrayLen $ \(arrPtr, len) ->
    withRunInIO $ \run ->
      F.allocaArray len $ \out -> do
        err <- intArrayImpl arrPtr out
        run $ handleErrorCode "Error decoding array of ints." err
        F.peekArray len out
{-# RULES "list int/listOfInt" list int = listOfInt #-}

-- | Is more efficient by looping in C++ instead of Haskell.
listOfDouble :: Value -> Decoder [Double]
listOfDouble =
  withArrayLen $ \(arrPtr, len) ->
    withRunInIO $ \run ->
      F.allocaArray len $ \out -> do
        err <- doubleArrayImpl arrPtr out
        run $ handleErrorCode "Error decoding array of doubles." err
        F.peekArray len out
{-# RULES "list double/listOfDouble" list double = listOfDouble #-}

-- | Helper to work with an Array parsed from a Value.
withArray :: (Array -> Decoder a) -> Value -> Decoder a
withArray f val =
  withRunInIO $ \run ->
    allocaArray $ \arrPtr -> do
      err <- getArrayFromValueImpl val arrPtr
      run $ do
        handleErrorCode (typePrefix "array") err
        f arrPtr
{-# INLINE withArray #-}

-- | Helper to work with an ArrayIter started from a Value assumed to be an Array.
withArrayIter :: (ArrayIter -> Decoder a) -> Value -> Decoder a
withArrayIter f valPtr =
  withRunInIO $ \run ->
    allocaArrayIter $ \iterPtr -> do
      err <- getArrayIterFromValueImpl valPtr iterPtr
      run $ do
        handleErrorCode (typePrefix "array") err
        f iterPtr
{-# INLINE withArrayIter #-}

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a list.
iterateOverArray :: (Value -> Decoder a) -> ArrayIter -> Decoder [a]
iterateOverArray f iterPtr =
  withRunInIO $ \run ->
    allocaValue $ \valPtr -> run $ go (0 :: Int) DList.empty valPtr
  where
    go !n !acc !valPtr = do
      isOver <- fmap F.toBool . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then do
          r <- withIndex n $ do
            err <- liftIO $ arrayIterGetCurrentImpl iterPtr valPtr
            handleErrorCode "" err
            result <- f valPtr
            liftIO $ arrayIterMoveNextImpl iterPtr
            pure result
          go (n + 1) (acc <> DList.singleton r) valPtr
        else
          pure $ DList.toList acc
{-# INLINE iterateOverArray #-}

-- | Helper to work with an ArrayIter and its length.
withArrayLenIter :: (ArrayIter -> Int -> Decoder a) -> Value -> Decoder a
withArrayLenIter f valPtr =
  withRunInIO $ \run ->
    allocaArrayIter $ \iterPtr ->
      F.alloca $ \outLen -> do
        err <- getArrayIterLenFromValueImpl valPtr iterPtr outLen
        len <- fmap fromIntegral $ F.peek outLen
        run $ do
          handleErrorCode "" err
          f iterPtr len
{-# INLINE withArrayLenIter #-}

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a generic `Vector`.
iterateOverArrayLen :: G.Vector v a => (Value -> Decoder a) -> ArrayIter -> Int -> Decoder (v a)
iterateOverArrayLen f iterPtr len =
  withRunInIO $ \run ->
    allocaValue $ \valPtr -> do
      v <- GM.new len
      _ <- run . runDecoderPrim $ go (0 :: Int) v valPtr
      G.unsafeFreeze v
  where
    go !n !acc !valPtr = do
      isOver <- fmap F.toBool . DecoderPrim . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then do
          _ <- DecoderPrim . withIndex n . runDecoderPrim $ do
            err <- DecoderPrim . liftIO $ arrayIterGetCurrentImpl iterPtr valPtr
            DecoderPrim $ handleErrorCode "" err
            result <- DecoderPrim $ f valPtr
            DecoderPrim . liftIO $ arrayIterMoveNextImpl iterPtr
            GM.unsafeWrite acc n result
          go (n + 1) acc valPtr
        else
          pure acc
{-# INLINE iterateOverArrayLen #-}

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
atKey :: Text -> (Value -> Decoder a) -> Object -> Decoder a
atKey key parser obj = withUnorderedField parser obj key
{-# INLINE atKey #-}

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atKeyOptional :: Text -> (Value -> Decoder a) -> Object -> Decoder (Maybe a)
atKeyOptional key parser obj = withUnorderedOptionalField parser obj key
{-# INLINE atKeyOptional #-}

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atKeyStrict :: Text -> (Value -> Decoder a) -> Object -> Decoder a
atKeyStrict key parser obj = withField parser obj key
{-# INLINE atKeyStrict #-}

-- | Parse a homogenous JSON array into a Haskell list.
list :: (Value -> Decoder a) -> Value -> Decoder [a]
list f = withArrayIter $ iterateOverArray f
{-# INLINE[2] list #-}

-- | Parse a homogenous JSON array into a generic `Vector`.
vector :: G.Vector v a => (Value -> Decoder a) -> Value -> Decoder (v a)
vector f = withArrayLenIter $ iterateOverArrayLen f
{-# INLINE vector #-}

-- | Parse an object into a homogenous list of key-value tuples.
objectAsKeyValues
  :: (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> (Value -> Decoder v)
  -- ^ Decoder for the field value.
  -> Value
  -> Decoder [(k, v)]
objectAsKeyValues kf vf = withObjectIter $ iterateOverFields kf vf
{-# INLINE objectAsKeyValues #-}

-- | Parse an object into a strict `Map`.
objectAsMap
  :: Ord k
  => (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> (Value -> Decoder v)
  -- ^ Decoder for the field value.
  -> Value
  -> Decoder (Map k v)
objectAsMap kf vf = withObjectIter $ iterateOverFieldsMap kf vf
{-# INLINE objectAsMap #-}

-- | Transforms a parser to return Nothing when the value is null.
nullable :: (Value -> Decoder a) -> Value -> Decoder (Maybe a)
nullable parser val = do
  nil <- isNull val
  if nil
    then pure Nothing
    else Just <$> parser val
{-# INLINE nullable #-}

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
{-# INLINE char #-}

-- | Parse a JSON string into a Haskell String.
-- For best performance you should use `text` instead.
string :: Value -> Decoder String
string = getString
{-# INLINE string #-}

-- | Parse a JSON string into Haskell Text.
text :: Value -> Decoder Text
text = getText
{-# INLINE text #-}

-- | Parse a JSON boolean into a Haskell Bool.
bool :: Value -> Decoder Bool
bool = getBool
{-# INLINE bool #-}

-- | Parse a JSON number into a signed Haskell Int.
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
{-# INLINE scientific #-}

-- | Get the simdjson type of the Value.
getType :: Value -> Decoder ValueType
getType valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> do
      err <- getTypeImpl valPtr ptr
      run $ do
        handleErrorCode "" err
        fmap (toEnum . fromIntegral) . liftIO $ F.peek ptr
{-# INLINE getType #-}
