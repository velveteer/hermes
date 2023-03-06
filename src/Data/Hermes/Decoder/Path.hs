{-# LANGUAGE FlexibleContexts #-}

module Data.Hermes.Decoder.Path
  ( withPath
  , withPathIndex
  , withPathOverride
  ) where

import           Control.Monad.Reader (MonadReader, local)
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Hermes.Decoder.Internal (HermesEnv(hPath))

withPath :: MonadReader HermesEnv m => Text -> m a -> m a
withPath key = local $ \st -> st { hPath = hPath st <> "/" <> key }
{-# INLINE withPath #-}

withPathOverride :: MonadReader HermesEnv m => Text -> m a -> m a
withPathOverride path = local $ \st -> st { hPath = path }
{-# INLINE withPathOverride #-}

withPathIndex :: MonadReader HermesEnv m => Int -> m a -> m a
withPathIndex idx = local $ \st -> st { hPath = hPath st <> showInt idx }
  where showInt i = T.cons '/' . T.pack $ show i
{-# INLINE withPathIndex #-}
