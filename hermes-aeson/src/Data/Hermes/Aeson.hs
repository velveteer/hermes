{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Hermes.Aeson
  ( aesonValue
  , parseAesonValue
  ) where

import Control.Exception (throwIO, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Hermes as H
import qualified Data.Hermes.SIMDJSON as HS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Word (Word32, Word8)
import qualified Foreign.C as F
import qualified Foreign.ForeignPtr as F
import qualified Foreign.Marshal.Alloc as F
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff, poke)
import qualified System.IO.Unsafe as Unsafe

aesonValue :: H.Decoder A.Value
aesonValue = H.withAesonTape $ \fp _len ->
  F.withForeignPtr fp $ \base ->
    readValue base $ \v _ -> pure v
{-# INLINE aesonValue #-}

readValue :: Ptr Word8 -> (A.Value -> Ptr Word8 -> IO r) -> IO r
readValue p k = do
  tag <- peekByteOff @Word8 p 0
  let p1 = p `plusPtr` 1
  case tag of
    0x00 -> k A.Null p1
    0x01 -> k (A.Bool True) p1
    0x02 -> k (A.Bool False) p1
    0x03 -> do
      i <- peekByteOff @Int64 p1 0
      k (A.Number (fromIntegral i)) (p1 `plusPtr` 8)
    0x05 -> readStrRef p1 $ \t p2 ->
      case H.parseScientificText t of
        Right s -> k (A.Number s) p2
        Left e -> throwTapeErr ("parseScientificText: " <> T.pack e)
    0x06 -> readStrRef p1 $ \t p2 -> k (A.String t) p2
    0x07 -> do
      n <- peekByteOff @Word32 p1 0
      readArray (fromIntegral n) (p1 `plusPtr` 4) k
    0x08 -> do
      n <- peekByteOff @Word32 p1 0
      readObject (fromIntegral n) (p1 `plusPtr` 4) k
    _ -> throwTapeErr ("Aeson tape: unknown tag " <> T.pack (show tag))

throwTapeErr :: Text -> IO a
throwTapeErr msg = throwIO (H.InternalException (H.DocumentError "" msg))
{-# INLINE throwTapeErr #-}

readStrRef :: Ptr Word8 -> (Text -> Ptr Word8 -> IO r) -> IO r
readStrRef p k = do
  len <- peekByteOff @Word32 p 0
  cstr <- peekByteOff @(Ptr Word8) p 4
  t <- T.fromPtr (castPtr cstr) (fromIntegral len)
  k t (p `plusPtr` (4 + 8))
{-# INLINE readStrRef #-}

readArray :: Int -> Ptr Word8 -> (A.Value -> Ptr Word8 -> IO r) -> IO r
readArray 0 !p k = k (A.Array V.empty) p
readArray !n !p0 k = do
  mv <- VM.new n
  let go !i !p
        | i == n = do
            v <- V.unsafeFreeze mv
            k (A.Array v) p
        | otherwise = readValue p $ \x p' -> do
            VM.unsafeWrite mv i $! x
            go (i + 1) p'
  go 0 p0

readObject :: Int -> Ptr Word8 -> (A.Value -> Ptr Word8 -> IO r) -> IO r
readObject 0 !p k = k (A.Object KM.empty) p
readObject !n !p0 k = go KM.empty 0 p0
  where
    go !acc !i !p
      | i == n =
          k (A.Object acc) p
      | otherwise =
          readStrRef p $ \kt p1 ->
            readValue p1 $ \v p2 ->
              go (KM.insert (K.fromText kt) v acc) (i + 1) p2

-- | Decode any JSON document into an Aeson 'A.Value', including
-- top-level scalars (number, string, boolean, null). Calls into a
-- document-level C++ walker that dispatches on @document.type()@
-- rather than going through @document.get_value()@, which simdjson
-- rejects for scalar documents with @SCALAR_DOCUMENT_AS_VALUE@.
parseAesonValue :: H.HermesEnv -> BS.ByteString -> Either H.HermesException A.Value
parseAesonValue env bs = Unsafe.unsafeDupablePerformIO . try $ parseAesonValueIO env bs

parseAesonValueIO :: H.HermesEnv -> BS.ByteString -> IO A.Value
parseAesonValueIO env bs =
  HS.withInputBuffer bs $ \inputBuf ->
    F.withForeignPtr (H.hParser env) $ \parserPtr ->
      F.withForeignPtr (H.hDocument env) $ \docPtr ->
        F.alloca $ \pData ->
          F.alloca $ \pLen ->
            F.alloca $ \pLoc -> do
              -- Defensive zero-init: the C wrapper also sets these,
              -- but pre-poking guarantees @peek pData@ returns a
              -- defined value if the FFI ever errors before writing.
              poke pData nullPtr
              poke pLen 0
              poke pLoc nullPtr
              err <-
                HS.buildAesonTapeFromDocImpl
                  (HS.Parser parserPtr)
                  inputBuf
                  (HS.Document docPtr)
                  pData
                  pLen
                  pLoc
              base <- peek pData
              -- Wrap the (possibly null) buffer pointer in a
              -- 'ForeignPtr' so its finalizer reclaims any partial
              -- buffer the walker may have allocated.
              -- 'free_aeson_tape' on @nullPtr@ is a no-op.
              fp <- F.newForeignPtr HS.freeAesonTapeImpl base
              if err == 0
                then F.withForeignPtr fp $ \basePtr ->
                  readValue basePtr $ \v _ -> pure v
                else do
                  msg <- formatErrMsg pLoc =<< F.peekCString =<< HS.getErrorMessageImpl err
                  throwIO (H.SIMDException (H.DocumentError "" msg))

-- | Format a simdjson error message, prepending a source snippet
-- if the C wrapper recorded a @current_location@. Trailing NUL bytes
-- (padded_string padding) are stripped from the snippet.
formatErrMsg :: Ptr F.CString -> String -> IO Text
formatErrMsg pLoc msg = do
  loc <- peek pLoc
  if loc == nullPtr
    then pure (T.pack msg)
    else do
      snippet <- BS.packCStringLen (loc, 32)
      let trimmed = BS.dropWhileEnd (== 0x00) snippet
      pure $ "near `" <> T.decodeUtf8Lenient trimmed <> "`: " <> T.pack msg
