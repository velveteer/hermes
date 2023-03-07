module Data.Hermes.Decoder.Path
  ( withKey
  , withIndex
  , withPointer
  ) where

import           Data.Text (Text)

import           Data.Hermes.Decoder.Internal (DecoderM, HermesEnv(hPath), Path(..), local)

withKey :: Text -> DecoderM a -> DecoderM a
withKey key = local $ \st -> st { hPath = Key key : hPath st }
{-# INLINE withKey #-}

withPointer :: Text -> DecoderM a -> DecoderM a
withPointer path = local $ \st -> st { hPath = [Pointer path] }
{-# INLINE withPointer #-}

withIndex :: Int -> DecoderM a -> DecoderM a
withIndex idx = local $ \st -> st { hPath = Idx idx : hPath st }
{-# INLINE withIndex #-}
