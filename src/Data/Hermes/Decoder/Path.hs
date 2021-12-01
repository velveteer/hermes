module Data.Hermes.Decoder.Path
  ( dot
  , removePath
  , withPath
  , withPathIndex
  ) where

import           Control.Monad.Reader (local)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Hermes.Decoder.Types (Decoder, HermesEnv(hPath))

withPath :: Text -> Decoder a -> Decoder a
withPath key =
  local $ \st -> st { hPath = hPath st <> key }
{-# INLINE withPath #-}

removePath :: Text -> Decoder a -> Decoder a
removePath key =
  local $ \st -> st { hPath = fromMaybe (hPath st) (T.stripSuffix key $ hPath st) }
{-# INLINE removePath #-}

withPathIndex :: Int -> Decoder a -> Decoder a
withPathIndex idx =
  local $ \st -> st
    { hPath = fromMaybe (hPath st) (T.stripSuffix (showInt $ max 0 (idx - 1)) $ hPath st)
           <> showInt idx
    }
  where showInt i = dot . T.pack $ show i
{-# INLINE withPathIndex #-}

dot :: Text -> Text
dot = T.cons '/'
{-# INLINE dot #-}
