module Data.Hermes.Decoder.Path
  ( withKey
  , withIndex
  , withPointer
  ) where

import           Data.Text (Text)

import           Data.Hermes.Decoder.Internal (Decoder, HermesEnv(hPath), Path(..), local)

withKey :: Text -> Decoder a -> Decoder a
withKey key = local $ \st -> st { hPath = Key key : hPath st }
{-# INLINE withKey #-}

withPointer :: Text -> Decoder a -> Decoder a
withPointer path = local $ \st -> st { hPath = [Pointer path] }
{-# INLINE withPointer #-}

withIndex :: Int -> Decoder a -> Decoder a
withIndex idx = local $ \st -> st { hPath = Idx idx : hPath st }
{-# INLINE withIndex #-}
