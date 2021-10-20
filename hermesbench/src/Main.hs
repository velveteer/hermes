{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.DeepSeq       (NFData)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as BS
import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Test.Tasty            (withResource)
import           Test.Tasty.Bench

import           Data.Hermes

main :: IO ()
main = defaultMain
  [ withResource (BS.readFile "json/persons9000.json") (const $ pure ()) $ \input ->
    bgroup "Full Decode Persons Array JSON"
    [ bgroup "Ordered Keys"
      [ bench "Hermes Decode" $
          nfIO (decode =<< input :: IO [Person])
      , withResource (mkHermesEnv =<< input) (const $ pure ()) $ \envIO ->
        bench "Hermes DecodeWith" $
          nfIO (decodeWith =<< envIO :: IO [Person])
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [Person]))
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [Person]))
      ]
    , bgroup "Unordered Keys"
      [ bench "Hermes Decode" $
          nfIO (decode =<< input :: IO [PersonUnordered])
      , withResource (mkHermesEnv =<< input) (const $ pure ()) $ \envIO ->
        bench "Hermes DecodeWith" $
          nfIO (decodeWith =<< envIO :: IO [PersonUnordered])
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [PersonUnordered]))
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [PersonUnordered]))
      ]
    ]
  , withResource (BS.readFile "json/twitter100.json") (const $ pure ()) $ \input ->
    bgroup "Partial Decode Twitter JSON"
    [ bench "Hermes Decode" $
        nfIO (decode =<< input :: IO Twitter)
    , withResource (mkHermesEnv =<< input) (const $ pure ()) $ \envIO ->
      bench "Hermes DecodeWith" $
        nfIO (decodeWith =<< envIO :: IO Twitter)
    , bench "Aeson Decode Lazy" $
        nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe Twitter))
    , bench "Aeson Decode Strict" $
        nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe Twitter))
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
    , phone         :: Identity Text
    , address       :: Text
    , about         :: Text
    , registered    :: Text
    , latitude      :: Double
    , longitude     :: Double
    , tags          :: [Text]
    , friends       :: Identity [Friend]
    , greeting      :: Maybe Text
    , favoriteFruit :: Text
    } deriving (Show, Generic, NFData)

data Friend =
  Friend
    { id   :: Int
    , name :: Text
    } deriving (Show, Generic, NFData)

instance FromJSON Friend where
  parseJSON = withObject $ \obj ->
    Friend
      <$> obj .:> "id"
      <*> obj .:> "name"

instance FromJSON Person where
  parseJSON = withObject $ \obj ->
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

instance Aeson.FromJSON Person where
  parseJSON = Aeson.withObject "Person" $ \obj ->
    Person
      <$> obj Aeson..: "_id"
      <*> obj Aeson..: "index"
      <*> obj Aeson..: "guid"
      <*> obj Aeson..: "isActive"
      <*> obj Aeson..: "balance"
      <*> obj Aeson..: "picture"
      <*> obj Aeson..: "age"
      <*> obj Aeson..: "eyeColor"
      <*> obj Aeson..: "name"
      <*> obj Aeson..: "gender"
      <*> obj Aeson..: "company"
      <*> obj Aeson..: "email"
      <*> obj Aeson..: "phone"
      <*> obj Aeson..: "address"
      <*> obj Aeson..: "about"
      <*> obj Aeson..: "registered"
      <*> obj Aeson..: "latitude"
      <*> obj Aeson..: "longitude"
      <*> obj Aeson..: "tags"
      <*> obj Aeson..: "friends"
      <*> obj Aeson..: "greeting"
      <*> obj Aeson..: "favoriteFruit"

instance Aeson.FromJSON Friend where
  parseJSON = Aeson.withObject "Friend" $ \obj ->
    Friend
      <$> obj Aeson..: "id"
      <*> obj Aeson..: "name"

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
    , nonexistent   :: Maybe Text
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
  parseJSON = withObject $ \obj ->
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
      <*> obj .:? "nonexistent"
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

data Twitter =
  Twitter
    { statuses :: [Status]
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

data Status =
  Status
    { metadata :: Metadata
    , user     :: User
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

data User =
  User
    { screen_name :: Text
    , location    :: Text
    , description :: Text
    , url         :: Maybe Text
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

data Metadata =
  Metadata
    { result_type :: Text
    , iso_language_code :: Text
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

instance FromJSON Twitter where
  parseJSON = withObject $ \obj ->
    Twitter
      <$> obj .:> "statuses"

instance FromJSON Status where
  parseJSON = withObject $ \obj ->
    Status
      <$> obj .:> "metadata"
      <*> obj .:> "user"

instance FromJSON Metadata where
  parseJSON = withObject $ \obj ->
    Metadata
      <$> obj .:> "result_type"
      <*> obj .:> "iso_language_code"

instance FromJSON User where
  parseJSON = withObject $ \obj ->
    User
      <$> obj .:> "screen_name"
      <*> obj .:> "location"
      <*> obj .:> "description"
      <*> obj .:> "url"

