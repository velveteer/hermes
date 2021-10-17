{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.DeepSeq      (NFData)
import           Criterion.Main
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           GHC.Generics         (Generic)

import           Data.SIMDJSON

main :: IO ()
main = do
  input      <- BS.readFile "benchmarks/test.json"
  oooInput   <- BS.readFile "benchmarks/test_ooo.json"
  arrInput   <- BS.readFile "benchmarks/test_array.json"
  pPtr       <- mkSIMDParser
  dPtr       <- mkSIMDDocument
  pStrPtr    <- mkSIMDPaddedStr input
  pStrPtrOOO <- mkSIMDPaddedStr oooInput
  pStrPtrArr <- mkSIMDPaddedStr arrInput
  defaultMain
    [ bgroup "Partial Decode Ordered Keys"
      [ bench "Aeson Decode Lazy" $
          nf (Aeson.decode :: BSL.ByteString -> Maybe Test) (BSL.fromStrict input)
      , bench "Aeson Decode Strict" $
          nf (Aeson.decode' :: BSL.ByteString -> Maybe Test) (BSL.fromStrict input)
      , bench "SIMD Decode" $
          nfIO (decodeWith pPtr dPtr pStrPtr :: IO Test)
      ]

    , bgroup "Partial Decode Out-of-Order Keys"
      [ bench "Aeson Decode Lazy" $
          nf (Aeson.decode :: BSL.ByteString -> Maybe [Test]) (BSL.fromStrict oooInput)
      , bench "Aeson Decode Strict" $
          nf (Aeson.decode' :: BSL.ByteString -> Maybe Test) (BSL.fromStrict oooInput)
      , bench "SIMD Decode" $
          nfIO (decodeWith pPtr dPtr pStrPtrOOO :: IO TestOOO)
      ]

    , bgroup "Full Decode Array of Objects"
      [ bench "Aeson Decode Lazy" $
          nf (Aeson.decode :: BSL.ByteString -> Maybe [Test]) (BSL.fromStrict arrInput)
      , bench "Aeson Decode Strict" $
          nf (Aeson.decode' :: BSL.ByteString -> Maybe [Test]) (BSL.fromStrict arrInput)
      , bench "SIMD Decode" $
          nfIO (decodeWith pPtr dPtr pStrPtrArr :: IO [Test])
      ]
    ]

data Test =
  Test
    { intField     :: Int
    , stringField  :: String
    , listIntField :: [Int]
    , booleanField :: Bool
    , nested       :: Test2
    } deriving (Generic, NFData, Show)

data Test2 =
  Test2
    { more    :: [Int]
    , another :: String
    } deriving (Generic, NFData, Show)

newtype TestOOO = TestOOO Test
  deriving newtype NFData

instance FromJSON Test where
  parseJSON valPtr = withObject valPtr $ \obj -> do
    str <- obj .:> "world"
    boolean <- obj .:> "boolField"
    int <- obj .:> "hello"
    ints <- obj .:> "list"
    nested' <- obj .:> "nested"
    pure Test
      { intField = int
      , stringField = str
      , listIntField = ints
      , booleanField = boolean
      , nested = nested'
      }

instance FromJSON TestOOO where
  parseJSON valPtr = withObject valPtr $ \obj -> do
    str <- obj .: "world"
    boolean <- obj .: "boolField"
    int <- obj .: "hello"
    ints <- obj .: "list"
    nested' <- obj .: "nested"
    pure . TestOOO $ Test
      { intField = int
      , stringField = str
      , listIntField = ints
      , booleanField = boolean
      , nested = nested'
      }

instance FromJSON Test2 where
  parseJSON valPtr = withObject valPtr $ \ obj -> do
    ints <- obj .:> "more"
    str <- obj .:> "another"
    pure Test2
      { more = ints
      , another = str
      }

instance Aeson.FromJSON Test where
  parseJSON = Aeson.withObject "test" $ \obj -> do
    str <- obj Aeson..: "world"
    boolean <- obj Aeson..: "boolField"
    int <- obj Aeson..: "hello"
    ints <- obj Aeson..: "list"
    nested' <- obj Aeson..: "nested"
    pure Test
      { intField = int
      , stringField = str
      , listIntField = ints
      , booleanField = boolean
      , nested = nested'
      }

instance Aeson.FromJSON Test2 where
  parseJSON = Aeson.withObject "test2" $ \obj -> do
    ints <- obj Aeson..: "more"
    str <- obj Aeson..: "another"
    pure Test2
      { more = ints
      , another = str
      }
