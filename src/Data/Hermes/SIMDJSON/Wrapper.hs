-- | Contains functions for constructing and working with foreign simdjson instances.

module Data.Hermes.SIMDJSON.Wrapper
  ( allocaArray
  , allocaArrayIter
  , allocaObject
  , allocaObjectIter
  , allocaValue
  , mkSIMDParser
  , mkSIMDDocument
  , mkSIMDPaddedStr
  , withInputBuffer
  )
  where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as Unsafe
import           Data.Maybe (fromMaybe)
import           Control.Exception (bracket, mask_)
import qualified Foreign.ForeignPtr as F
import qualified Foreign.Marshal.Alloc as F
import qualified Foreign.Ptr as F

import           Data.Hermes.SIMDJSON.Bindings
  ( deleteDocumentImpl
  , deleteInputImpl
  , makeDocumentImpl
  , makeInputImpl
  , parserDestroy
  , parserInit
  )
import           Data.Hermes.SIMDJSON.Types

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

allocaValue :: (Value -> IO a) -> IO a
allocaValue f = allocaBytes 24 $ \val -> f (Value val)
{-# INLINE allocaValue #-}

allocaObject :: (Object -> IO a) -> IO a
allocaObject f = allocaBytes 24 $ \objPtr -> f (Object objPtr)
{-# INLINE allocaObject #-}

allocaArray :: (Array -> IO a) -> IO a
allocaArray f = allocaBytes 24 $ \arr -> f (Array arr)
{-# INLINE allocaArray #-}

allocaArrayIter :: (ArrayIter -> IO a) -> IO a
allocaArrayIter f = allocaBytes 24 $ \iter -> f (ArrayIter iter)
{-# INLINE allocaArrayIter #-}

allocaObjectIter :: (ObjectIter -> IO a) -> IO a
allocaObjectIter f = allocaBytes 24 $ \iter -> f (ObjectIter iter)
{-# INLINE allocaObjectIter #-}

allocaBytes :: Int -> (F.Ptr a -> IO b) -> IO b
allocaBytes size action = F.allocaBytes size action
{-# INLINE allocaBytes #-}
