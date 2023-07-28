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
  , parseScientific
  , scientific
  , string
  , text
  , listOfDouble
  , listOfInt
  , isNull
  , vector
  , withBool
  , withDouble
  , withInt
  , withObject
  , withObjectAsMap
  , withRawByteString
  , withScientific
  , withString
  , withText
  , withType
  , withVector
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
#endif
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Foreign.C.String as F
import qualified Foreign.ForeignPtr as F
import qualified Foreign.Marshal.Alloc as F
import qualified Foreign.Marshal.Array as F
import qualified Foreign.Marshal.Utils as F
import qualified Foreign.Ptr as F
import qualified Foreign.Storable as F

import           Data.Hermes.Decoder.Internal
import           Data.Hermes.Decoder.Path
import           Data.Hermes.SIMDJSON

-- | Decode a value at the particular JSON pointer following RFC 6901.
-- Be careful where you use this because it rewinds the document on each
-- successive call.
--
-- > decodeEither (atPointer "/statuses/99" decodeObject) input
atPointer :: Text -> Decoder a -> Decoder a
atPointer jptr (Decoder f) = Decoder $ \_ -> do
  doc <- asks hDocument
  withRunInIO $ \run ->
    F.withForeignPtr doc $ \docPtr ->
      Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 jptr) $ \(cstr, len) ->
        allocaValue $ \vPtr -> run . withPointer jptr $ do
          err <- liftIO $ atPointerImpl cstr len (Document docPtr) vPtr
          handleErrorCode "" err
          f vPtr
{-# INLINE atPointer #-}

-- | Helper to work with an Object cast from a Value.
withObject :: (Object -> Decoder a) -> Decoder a
withObject f = Decoder $ \(Value valPtr) ->
  withRunInIO $ \run ->
    allocaValue $ \vPtr -> do
      err <- getObjectFromValueImpl (Value valPtr)
      run $ do
        handleErrorCode (typePrefix "object") err
        runDecoder (f $ Object $ F.castPtr valPtr) vPtr
{-# INLINE withObject #-}

-- | Helper to work with an Int parsed from a Value.
withInt :: (Int -> Decoder a) -> Decoder a
withInt f = Decoder $ \val -> getInt val >>= \i -> runDecoder (f i) val
{-# INLINE withInt #-}

-- | Helper to work with a Double parsed from a Value.
withDouble :: (Double -> Decoder a) -> Decoder a
withDouble f = Decoder $ \val -> getDouble val >>= \d -> runDecoder (f d) val
{-# INLINE withDouble #-}

-- | Helper to work with a Bool parsed from a Value.
withBool :: (Bool -> Decoder a) -> Decoder a
withBool f = Decoder $ \val -> getBool val >>= \b -> runDecoder (f b) val
{-# INLINE withBool #-}

-- | Helper to work with the raw ByteString of the JSON token parsed from the given Value.
withRawByteString :: (BS.ByteString -> Decoder a) -> Decoder a
withRawByteString f = Decoder $ \val -> getRawByteString val >>= \b -> runDecoder (f b) val
{-# INLINE withRawByteString #-}

-- | Helper to work with a String parsed from a Value.
withString :: (String -> Decoder a) -> Decoder a
withString f = Decoder $ \val -> getString val >>= \s -> runDecoder (f s) val
{-# INLINE withString #-}

-- | Helper to work with a Text parsed from a Value.
withText :: (Text -> Decoder a) -> Decoder a
withText f = Decoder $ \val -> getText val >>= \t -> runDecoder (f t) val
{-# INLINE withText #-}

-- | Returns True if the Value is null.
isNull :: Decoder Bool
isNull = Decoder $ \valPtr ->
  withRunInIO $ \run ->
    F.alloca $ \ptr -> do
      err <- isNullImpl valPtr ptr
      run $ do
        handleErrorCode "" err
        fmap F.toBool . liftIO $ F.peek ptr
{-# INLINE isNull #-}

-- | Is more efficient by looping in C++ instead of Haskell.
listOfInt :: Decoder [Int]
listOfInt =
  withArrayLen $ \(arrPtr, len) ->
    withRunInIO $ \run ->
      F.allocaArray len $ \out -> do
        err <- intArrayImpl arrPtr out
        run $ handleErrorCode "Error decoding array of ints. " err
        F.peekArray len out
{-# RULES "list int/listOfInt" list int = listOfInt #-}

-- | Is more efficient by looping in C++ instead of Haskell.
listOfDouble :: Decoder [Double]
listOfDouble =
  withArrayLen $ \(arrPtr, len) ->
    withRunInIO $ \run ->
      F.allocaArray len $ \out -> do
        err <- doubleArrayImpl arrPtr out
        run $ handleErrorCode "Error decoding array of doubles. " err
        F.peekArray len out
{-# RULES "list double/listOfDouble" list double = listOfDouble #-}

-- | Find an object field by key, where an exception is thrown
-- if the key is missing.
atKey :: Text -> Decoder a -> Object -> Decoder a
atKey key parser obj = Decoder $ \val -> withUnorderedField val parser obj key
{-# INLINE atKey #-}

-- | Find an object field by key, where Nothing is returned
-- if the key is missing.
atKeyOptional :: Text -> Decoder a -> Object -> Decoder (Maybe a)
atKeyOptional key parser obj = Decoder $ \val -> withUnorderedOptionalField val parser obj key
{-# INLINE atKeyOptional #-}

-- | Uses find_field, which means if you access a field out-of-order
-- this will throw an exception. It also cannot support optional fields.
atKeyStrict :: Text -> Decoder a -> Object -> Decoder a
atKeyStrict key parser obj = Decoder $ \val -> withField val parser obj key
{-# INLINE atKeyStrict #-}

-- | Parse a homogenous JSON array into a Haskell list.
list :: Decoder a -> Decoder [a]
list f = withArrayIter $ iterateOverArray f
{-# INLINE[2] list #-}

-- | Parse a homogenous JSON array into a generic `Vector`.
vector :: G.Vector v a => Decoder a -> Decoder (v a)
vector f = withArrayLenIter $ iterateOverArrayLen f
{-# INLINE vector #-}

withVector :: G.Vector v a => Decoder a -> (v a -> Decoder a) -> Decoder a
withVector inner f = Decoder $ \val -> runDecoder (vector inner) val >>= \v -> runDecoder (f v) val
{-# INLINE withVector #-}

-- | Parse an object into a homogenous list of key-value tuples.
objectAsKeyValues
  :: (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> Decoder v
  -- ^ Decoder for the field value.
  -> Decoder [(k, v)]
objectAsKeyValues kf vf = withObjectIter $ iterateOverFields kf vf
{-# INLINE objectAsKeyValues #-}

-- | Parse an object into a strict `Map`.
objectAsMap
  :: Ord k
  => (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> Decoder v
  -- ^ Decoder for the field value.
  -> Decoder (Map k v)
objectAsMap kf vf = withObjectIter $ iterateOverFieldsMap kf vf
{-# INLINE objectAsMap #-}

withObjectAsMap
  :: Ord k
  => (Text -> Decoder k)
  -- ^ Parses a Text key in the Decoder monad. JSON keys are always text.
  -> Decoder v
  -- ^ Decoder for the field value.
  -> (Map k v -> Decoder a)
  -> Decoder a
withObjectAsMap kf vf f = Decoder $ \val -> runDecoder (objectAsMap kf vf) val >>= \m -> runDecoder (f m) val
{-# INLINE withObjectAsMap #-}

-- | Transforms a parser to return Nothing when the value is null.
nullable :: Decoder a -> Decoder (Maybe a)
nullable parser = Decoder $ \val -> do
  nil <- runDecoder isNull val
  if nil
    then pure Nothing
    else Just <$> runDecoder parser val
{-# INLINE nullable #-}

-- | Parse only a single character.
char :: Decoder Char
char = Decoder $ getText >=> justOne
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
string :: Decoder String
string = Decoder getString
{-# INLINE string #-}

-- | Parse a JSON string into Haskell Text.
text :: Decoder Text
text = Decoder getText
{-# INLINE text #-}

-- | Parse a JSON boolean into a Haskell Bool.
bool :: Decoder Bool
bool = Decoder getBool
{-# INLINE bool #-}

-- | Parse a JSON number into a signed Haskell Int.
int :: Decoder Int
int = Decoder getInt
{-# INLINE[2] int #-}

-- | Parse a JSON number into a Haskell Double.
double :: Decoder Double
double = Decoder getDouble
{-# INLINE[2] double #-}

-- | Parse a Scientific from a Value.
scientific :: Decoder Sci.Scientific
scientific = withRawByteString parseScientific
{-# INLINE scientific #-}

withScientific :: (Sci.Scientific -> Decoder a) -> Decoder a
withScientific f = Decoder $ \val -> runDecoder scientific val >>= \sci -> runDecoder (f sci) val
{-# INLINE withScientific #-}

-- | Get the simdjson type of the Value.
getType :: Decoder ValueType
getType =
  Decoder $ \valPtr ->
    withRunInIO $ \run ->
      F.alloca $ \ptr -> do
        err <- getTypeImpl valPtr ptr
        run $ do
          handleErrorCode "" err
          fmap (toEnum . fromIntegral) . liftIO $ F.peek ptr
{-# INLINE getType #-}

withType :: (ValueType -> Decoder a) -> Decoder a
withType f = Decoder $ \val -> runDecoder getType val >>= \ty -> runDecoder (f ty) val
{-# INLINE withType #-}

-- | Parse a Scientific using attoparsec's ByteString.Char8 parser.
parseScientific :: BS.ByteString -> Decoder Sci.Scientific
parseScientific
  = either (\err -> fail $ "Failed to parse Scientific: " <> err) pure
  . A.parseOnly (AC.scientific <* A.endOfInput)
  . BSC.strip
{-# INLINE parseScientific #-}

-- Internal Functions

-- | Helper to work with an ArrayIter started from a Value assumed to be an Array.
withArrayIter :: (ArrayIter -> DecoderM a) -> Decoder a
withArrayIter f = Decoder $ \(Value valPtr) ->
  withRunInIO $ \run -> do
    err <- getArrayIterFromValueImpl (Value valPtr)
    run $ do
      handleErrorCode (typePrefix "array") err
      f (ArrayIter $ F.castPtr valPtr)
{-# INLINE withArrayIter #-}

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a list.
iterateOverArray :: Decoder a -> ArrayIter -> DecoderM [a]
iterateOverArray f iterPtr =
  withRunInIO $ \run ->
    allocaValue $ \valPtr -> run $ go (0 :: Int) DList.empty valPtr
  where
    go !n acc valPtr = do
      isOver <- fmap F.toBool . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then do
          r <- withIndex n $ do
            err <- liftIO $ arrayIterGetCurrentImpl iterPtr valPtr
            handleErrorCode "" err
            result <- runDecoder f valPtr
            liftIO $ arrayIterMoveNextImpl iterPtr
            pure result
          go (n + 1) (acc <> DList.singleton r) valPtr
        else
          pure $ DList.toList acc
{-# INLINE iterateOverArray #-}

-- | Helper to work with an ArrayIter and its length.
withArrayLenIter :: (ArrayIter -> Int -> DecoderM a) -> Decoder a
withArrayLenIter f = Decoder $ \(Value valPtr) ->
  withRunInIO $ \run -> do
    F.alloca $ \outLen -> do
      err <- getArrayIterLenFromValueImpl (Value valPtr) outLen
      len <- fmap fromIntegral $ F.peek outLen
      run $ do
        handleErrorCode "" err
        f (ArrayIter $ F.castPtr valPtr) len
{-# INLINE withArrayLenIter #-}

-- | Execute a function on each Value in an ArrayIter and
-- accumulate the results into a generic `Vector`.
iterateOverArrayLen :: G.Vector v a => Decoder a -> ArrayIter -> Int -> DecoderM (v a)
iterateOverArrayLen f iterPtr len =
  withRunInIO $ \run ->
    allocaValue $ \valPtr -> do
      v <- GM.new len
      _ <- run . runDecoderPrimM $ go (0 :: Int) v valPtr
      G.unsafeFreeze v
  where
    go !n acc valPtr = do
      isOver <- fmap F.toBool . DecoderPrimM . liftIO $ arrayIterIsDoneImpl iterPtr
      if not isOver
        then do
          _ <- DecoderPrimM . withIndex n . runDecoderPrimM $ do
            err <- DecoderPrimM . liftIO $ arrayIterGetCurrentImpl iterPtr valPtr
            DecoderPrimM $ handleErrorCode "" err
            result <- DecoderPrimM $ runDecoder f valPtr
            DecoderPrimM . liftIO $ arrayIterMoveNextImpl iterPtr
            GM.unsafeWrite acc n result
          go (n + 1) acc valPtr
        else
          pure acc
{-# INLINE iterateOverArrayLen #-}

-- | Helper to work with an ObjectIter started from a Value assumed to be an Object.
withObjectIter :: (ObjectIter -> DecoderM a) -> Decoder a
withObjectIter f = Decoder $ \(Value valPtr) ->
  withRunInIO $ \run -> do
    err <- getObjectIterFromValueImpl (Value valPtr)
    run $ do
      handleErrorCode (typePrefix "object") err
      f (ObjectIter $ F.castPtr valPtr)
{-# INLINE withObjectIter #-}

-- | Execute a function on each Field in an ObjectIter and accumulate into a `Map`.
iterateOverFieldsMap
  :: Ord a
  => (Text -> Decoder a)
  -> Decoder b
  -> ObjectIter
  -> DecoderM (Map a b)
iterateOverFieldsMap fk fv iterPtr =
  withRunInIO $ \run ->
    F.alloca $ \lenPtr ->
      F.alloca $ \keyPtr ->
        allocaValue $ \valPtr -> run $ go M.empty keyPtr lenPtr valPtr
  where
    go acc keyPtr lenPtr valPtr = do
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
                k <- runDecoder (fk keyTxt) valPtr
                v <- runDecoder fv valPtr
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
  -> Decoder b
  -> ObjectIter
  -> DecoderM [(a, b)]
iterateOverFields fk fv iterPtr =
  withRunInIO $ \run ->
    F.alloca $ \lenPtr ->
      F.alloca $ \keyPtr ->
        allocaValue $ \valPtr -> run $ go DList.empty keyPtr lenPtr valPtr
  where
    go acc keyPtr lenPtr valPtr = do
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
                k <- runDecoder (fk keyTxt) valPtr
                v <- runDecoder fv valPtr
                pure (k, v)
          liftIO $ objectIterMoveNextImpl iterPtr
          go (acc <> DList.singleton kv) keyPtr lenPtr valPtr
        else
          pure $ DList.toList acc
{-# INLINE iterateOverFields #-}

withUnorderedField :: Value -> Decoder a -> Object -> Text -> DecoderM a
withUnorderedField vPtr f objPtr key =
  withRunInIO $ \run ->
    Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) ->
      run $ withKey key $ do
        err <- liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr
        handleErrorCode "" err
        runDecoder f vPtr
{-# INLINE withUnorderedField #-}

withUnorderedOptionalField :: Value -> Decoder a -> Object -> Text -> DecoderM (Maybe a)
withUnorderedOptionalField vPtr f objPtr key =
  withRunInIO $ \run ->
    Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) ->
      run $ withKey key $ do
        err <- liftIO $ findFieldUnorderedImpl objPtr cstr len vPtr
        let errCode = toEnum $ fromIntegral err
        if | errCode == SUCCESS       -> Just <$> runDecoder f vPtr
           | errCode == NO_SUCH_FIELD -> pure Nothing
           | otherwise                -> Nothing <$ handleErrorCode "" err
{-# INLINE withUnorderedOptionalField #-}

withField :: Value -> Decoder a -> Object -> Text -> DecoderM a
withField vPtr f objPtr key =
  withRunInIO $ \run ->
    Unsafe.unsafeUseAsCStringLen (T.encodeUtf8 key) $ \(cstr, len) ->
      run $ withKey key $ do
        err <- liftIO $ findFieldImpl objPtr cstr len vPtr
        handleErrorCode "" err
        runDecoder f vPtr
{-# INLINE withField #-}

getInt :: Value -> DecoderM Int
getInt valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> run $ do
      err <- liftIO $ getIntImpl valPtr ptr
      handleErrorCode (typePrefix "int") err
      liftIO $ F.peek ptr
{-# INLINE getInt #-}

getDouble :: Value -> DecoderM Double
getDouble valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> run $ do
      err <- liftIO $ getDoubleImpl valPtr ptr
      handleErrorCode (typePrefix "double") err
      liftIO $ F.peek ptr
{-# INLINE getDouble #-}

getBool :: Value -> DecoderM Bool
getBool valPtr =
  withRunInIO $ \run ->
    F.alloca $ \ptr -> run $ do
      err <- liftIO $ getBoolImpl valPtr ptr
      handleErrorCode (typePrefix "bool") err
      fmap F.toBool . liftIO $ F.peek ptr
{-# INLINE getBool #-}

withCStringLen :: Text -> (F.CStringLen -> DecoderM a) -> Value -> DecoderM a
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

getString :: Value -> DecoderM String
getString = withCStringLen "string" (liftIO . F.peekCStringLen)
{-# INLINE getString #-}

getText :: Value -> DecoderM Text
getText = withCStringLen "text" parseTextFromCStrLen
{-# INLINE getText #-}

#if MIN_VERSION_text(2,0,0)
parseTextFromCStrLen :: F.CStringLen -> DecoderM Text
parseTextFromCStrLen (cstr, len) = liftIO $ T.fromPtr (F.castPtr cstr) (fromIntegral len)
{-# INLINE parseTextFromCStrLen #-}
#else

parseTextFromCStrLen :: F.CStringLen -> DecoderM Text
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

getRawByteString :: Value -> DecoderM BS.ByteString
getRawByteString valPtr =
  liftIO $
    F.alloca $ \strPtr ->
      F.alloca $ \lenPtr -> do
        getRawJSONTokenImpl valPtr strPtr lenPtr
        len <- fmap fromIntegral $ F.peek lenPtr
        str <- F.peek strPtr
        Unsafe.unsafePackCStringLen (str, len)
{-# INLINE getRawByteString #-}

-- | Helper to work with an Array and its length parsed from a Value.
withArrayLen :: ((Array, Int) -> DecoderM a) -> Decoder a
withArrayLen f = Decoder $ \(Value val) ->
  withRunInIO $ \run ->
    F.alloca $ \outLen -> do
      err <- getArrayLenFromValueImpl (Value val) outLen
      len <- fmap fromIntegral $ F.peek outLen
      run $ do
        handleErrorCode (typePrefix "array") err
        f (Array $ F.castPtr val, len)
{-# INLINE withArrayLen #-}
