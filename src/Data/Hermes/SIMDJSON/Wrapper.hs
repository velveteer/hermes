-- | Contains functions for constructing and working with foreign simdjson instances.

module Data.Hermes.SIMDJSON.Wrapper
  ( allocaValue
  , mkSIMDParser
  , mkSIMDDocument
  , mkSIMDPaddedStr
  , withInputBuffer
  )
  where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as Unsafe
import           Data.Maybe (fromMaybe)
import           Control.Exception (mask_)
import qualified Foreign.ForeignPtr as F
import qualified Foreign.Marshal.Alloc as F

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
{-# INLINE mkSIMDParser #-}

mkSIMDDocument :: IO (F.ForeignPtr SIMDDocument)
mkSIMDDocument = mask_ $ do
  ptr <- makeDocumentImpl
  F.newForeignPtr deleteDocumentImpl ptr
{-# INLINE mkSIMDDocument #-}

mkSIMDPaddedStr :: ByteString -> IO (F.ForeignPtr PaddedString)
mkSIMDPaddedStr input = mask_ $
  Unsafe.unsafeUseAsCStringLen input $ \(cstr, len) -> do
    ptr <- makeInputImpl cstr (fromIntegral len)
    F.newForeignPtr deleteInputImpl ptr
{-# INLINE mkSIMDPaddedStr #-}

withInputBuffer :: ByteString -> (InputBuffer -> IO a) -> IO a
withInputBuffer bs f = do
  pStr <- mkSIMDPaddedStr bs
  F.withForeignPtr pStr $ f . InputBuffer
{-# INLINE withInputBuffer #-}

allocaValue :: (Value -> IO a) -> IO a
allocaValue f = F.allocaBytes 24 $ \val -> f (Value val)
{-# INLINE allocaValue #-}
