-- | Contains functions for constructing and working with foreign simdjson instances.

module Data.Hermes.SIMDJSON.Wrapper
  ( getDocumentInfo
  , mkSIMDParser
  , mkSIMDDocument
  , mkSIMDPaddedStr
  , withInputBuffer
  )
  where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as Unsafe
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Exception (bracket, mask_)
import qualified Foreign.C as F
import qualified Foreign.ForeignPtr as F
import qualified Foreign.Marshal.Alloc as F
import qualified Foreign.Storable as F

import           Data.Hermes.SIMDJSON.Bindings
  ( currentLocationImpl
  , deleteDocumentImpl
  , deleteInputImpl
  , makeDocumentImpl
  , makeInputImpl
  , parserDestroy
  , parserInit
  , toDebugStringImpl
  )
import           Data.Hermes.SIMDJSON.Types
  ( Document
  , InputBuffer(..)
  , PaddedString
  , SIMDDocument
  , SIMDErrorCode(..)
  , SIMDParser
  )

mkSIMDParser :: Maybe Int -> IO (F.ForeignPtr SIMDParser)
mkSIMDParser mCap = mask_ $ do
  let maxCap = 4000000000; -- 4GB
  ptr <- parserInit . toEnum $ fromMaybe maxCap mCap
  F.newForeignPtr parserDestroy ptr

mkSIMDDocument :: IO (F.ForeignPtr SIMDDocument)
mkSIMDDocument = mask_ $ do
  ptr <- makeDocumentImpl
  F.newForeignPtr deleteDocumentImpl ptr

mkSIMDPaddedStr :: ByteString -> IO (F.ForeignPtr PaddedString)
mkSIMDPaddedStr input = mask_ $
  Unsafe.unsafeUseAsCStringLen input $ \(cstr, len) -> do
    ptr <- makeInputImpl cstr (fromIntegral len)
    F.newForeignPtr deleteInputImpl ptr

-- | Construct a simdjson:padded_string from a Haskell `ByteString`, and pass
-- it to an IO action. The instance lifetime is managed by the `bracket` function.
withInputBuffer :: ByteString -> (InputBuffer -> IO a) -> IO a
withInputBuffer bs f =
  bracket acquire release $ \fPtr -> F.withForeignPtr fPtr $ f . InputBuffer
  where
    acquire = mkSIMDPaddedStr bs
    release = F.finalizeForeignPtr

-- | Read the document location and debug string. If the iterator is out of bounds
-- then we abort reading from the iterator buffers to prevent reading garbage.
getDocumentInfo :: Document -> IO (Text, Text)
getDocumentInfo docPtr = F.alloca $ \locStrPtr -> F.alloca $ \lenPtr -> do
  err <- liftIO $ currentLocationImpl docPtr locStrPtr
  let errCode = toEnum $ fromIntegral err
  if errCode == OUT_OF_BOUNDS
    then pure ("out of bounds", "")
    else F.allocaBytes 128 $ \dbStrPtr -> do
      locStr <- fmap T.pack $ F.peekCString =<< F.peek locStrPtr
      toDebugStringImpl docPtr dbStrPtr lenPtr
      len <- fmap fromIntegral $ F.peek lenPtr
      debugStr <- fmap T.pack $ F.peekCStringLen (dbStrPtr, len)
      pure (locStr, debugStr)
