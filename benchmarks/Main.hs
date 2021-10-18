{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.DeepSeq      (NFData)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import           GHC.ForeignPtr       (ForeignPtr)
import           GHC.Generics         (Generic)
import           Test.Tasty           (withResource)
import           Test.Tasty.Bench

import           Data.SIMDJSON

setupSIMD
  :: IO BS.ByteString
  -> IO (ForeignPtr SIMDParser, ForeignPtr SIMDDocument, ForeignPtr PaddedString)
setupSIMD input = do
  p <- mkSIMDParser
  d <- mkSIMDDocument
  i <- mkSIMDPaddedStr =<< input
  pure (p, d, i)

main :: IO ()
main = do
  defaultMain
    [ withResource (BS.readFile "benchmarks/persons.json") (const $ pure ()) $ \input ->
        bgroup "Decode 12MB Array of Objects"
        [ bench "Aeson Decode Strict" $
            nfIO ((Aeson.decode' . BSL.fromStrict <$> input) :: IO (Maybe [Person]))
        , bench "Aeson Decode Lazy" $
            nfIO ((Aeson.decode . BSL.fromStrict <$> input) :: IO (Maybe [Person]))
        , withResource (setupSIMD input) (const $ pure ()) $ \ptrs ->
          bench "SIMD Decode" $
            nfIO (do { (p, d, i) <- ptrs; decodeWith p d i :: IO [Person] })
        ]
    ]

data Person =
  Person
    { _id           :: Text
    , index         :: Int
    , guid          :: Text
    , isActive      :: Bool
    , balance       :: Text
    , picture       :: Text
    , age           :: Int
    , eyeColor      :: Text
    , name          :: Text
    , gender        :: Text
    , company       :: Text
    , email         :: Text
    , phone         :: Text
    , address       :: Text
    , about         :: Text
    , registered    :: Text
    , latitude      :: Double
    , longitude     :: Double
    , tags          :: [Text]
    , friends       :: [Friend]
    , greeting      :: Text
    , favoriteFruit :: Text
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

data Friend =
  Friend
    { id   :: Int
    , name :: Text
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

instance FromJSON Friend where
  parseJSON valPtr = withObject valPtr $ \obj ->
    Friend
    <$> obj .:> "id"
    <*> obj .:> "name"

instance FromJSON Person where
  parseJSON valPtr = withObject valPtr $ \obj ->
    Person
      <$> obj .:> "_id"
      <*> obj .:> "index"
      <*> obj .:> "guid"
      <*> obj .:> "isActive"
      <*> obj .:> "balance"
      <*> obj .:> "picture"
      <*> obj .:> "age"
      <*> obj .:> "eyeColor"
      <*> obj .:> "name"
      <*> obj .:> "gender"
      <*> obj .:> "company"
      <*> obj .:> "email"
      <*> obj .:> "phone"
      <*> obj .:> "address"
      <*> obj .:> "about"
      <*> obj .:> "registered"
      <*> obj .:> "latitude"
      <*> obj .:> "longitude"
      <*> obj .:> "tags"
      <*> obj .:> "friends"
      <*> obj .:> "greeting"
      <*> obj .:> "favoriteFruit"
