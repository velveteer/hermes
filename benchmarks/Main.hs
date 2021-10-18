{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
main = defaultMain [
  withResource (BS.readFile "benchmarks/persons.json") (const $ pure ()) $ \input ->
    bgroup "Decode 12MB Array of Objects"
    [ bgroup "Ordered Keys"
      [ withResource (setupSIMD input) (const $ pure ()) $ \ptrs ->
        bench "SIMD Decode" $
          nfIO (do { (p, d, i) <- ptrs; decodeWith p d i :: IO [Person] })
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decode' . BSL.fromStrict <$> input) :: IO (Maybe [Person]))
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decode . BSL.fromStrict <$> input) :: IO (Maybe [Person]))
      ]
    , bgroup "Unordered Keys"
      [ withResource (setupSIMD input) (const $ pure ()) $ \ptrs ->
        bench "SIMD Decode" $
          nfIO (do { (p, d, i) <- ptrs; decodeWith p d i :: IO [PersonUnordered] })
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decode' . BSL.fromStrict <$> input) :: IO (Maybe [PersonUnordered]))
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decode . BSL.fromStrict <$> input) :: IO (Maybe [PersonUnordered]))
      ]
    ]
  ]

data Person =
  Person
    { _id           :: Text
    , index         :: Int
    , guid          :: Text
    , isActive      :: Bool
    , balance       :: Text
    , picture       :: Maybe Text
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
    , greeting      :: Maybe Text
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

data PersonUnordered =
  PersonUnordered
    { favoriteFruit :: Text
    , isActive      :: Bool
    , longitude     :: Double
    , balance       :: Text
    , email         :: Text
    , latitude      :: Double
    , age           :: Int
    , eyeColor      :: Text
    , index         :: Int
    , guid          :: Text
    , name          :: Text
    , greeting      :: Text
    , company       :: Text
    , picture       :: Text
    , phone         :: Text
    , _id           :: Text
    , address       :: Text
    , about         :: Text
    , gender        :: Text
    , registered    :: Text
    , tags          :: [Text]
    , friends       :: [Friend]
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

instance FromJSON PersonUnordered where
  parseJSON valPtr = withObject valPtr $ \obj ->
    PersonUnordered
      <$> obj .: "favoriteFruit"
      <*> obj .: "isActive"
      <*> obj .: "longitude"
      <*> obj .: "balance"
      <*> obj .: "email"
      <*> obj .: "latitude"
      <*> obj .: "age"
      <*> obj .: "eyeColor"
      <*> obj .: "index"
      <*> obj .: "guid"
      <*> obj .: "name"
      <*> obj .: "greeting"
      <*> obj .: "company"
      <*> obj .: "picture"
      <*> obj .: "phone"
      <*> obj .: "_id"
      <*> obj .: "address"
      <*> obj .: "about"
      <*> obj .: "gender"
      <*> obj .: "registered"
      <*> obj .: "tags"
      <*> obj .: "friends"
