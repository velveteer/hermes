{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.DeepSeq       (NFData)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as BS
import           Data.Functor.Identity (Identity(..))
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Test.Tasty            (withResource)
import           Test.Tasty.Bench

import           Data.Hermes

main :: IO ()
main = defaultMain
  [ withResource (BS.readFile "json/persons9000.json") (const $ pure ()) $ \input ->
    withResource mkHermesEnv_ (const $ pure ()) $ \envIO ->
    bgroup "Full Decode Persons Array JSON"
    [ bgroup "Ordered Keys"
      [ bench "Hermes Decode" $
          nfIO (flip decode (list decodePerson) =<< input :: IO [Person])
      , bench "Hermes DecodeWith" $
          nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (list decodePerson)})
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [Person]))
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [Person]))
      ]
    , bgroup "Unordered Keys"
      [ bench "Hermes Decode" $
          nfIO (flip decode (list decodePersonUnordered) =<< input :: IO [PersonUnordered])
      , bench "Hermes DecodeWith" $
          nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (list decodePersonUnordered)})
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [PersonUnordered]))
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [PersonUnordered]))
      ]
    ]
  , withResource (BS.readFile "json/twitter100.json") (const $ pure ()) $ \input ->
    withResource mkHermesEnv_ (const $ pure ()) $ \envIO ->
    bgroup "Partial Decode Twitter JSON"
    [ bench "Hermes Decode" $
        nfIO (flip decode decodeTwitter =<< input :: IO Twitter)
    , bench "Hermes DecodeWith" $
        nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs decodeTwitter})
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

decodePerson :: Value -> IO Person
decodePerson = withObject $ \obj ->
  Person
    <$> atOrderedKey "_id" text obj
    <*> atOrderedKey "index" int obj
    <*> atOrderedKey "guid" text obj
    <*> atOrderedKey "isActive" bool obj
    <*> atOrderedKey "balance" text obj
    <*> atOrderedKey "picture" (nullable text) obj
    <*> atOrderedKey "age" int obj
    <*> atOrderedKey "eyeColor" text obj
    <*> atOrderedKey "name" text obj
    <*> atOrderedKey "gender" text obj
    <*> atOrderedKey "company" text obj
    <*> atOrderedKey "email" text obj
    <*> (Identity <$> atOrderedKey "phone" text obj)
    <*> atOrderedKey "address" text obj
    <*> atOrderedKey "about" text obj
    <*> atOrderedKey "registered" text obj
    <*> atOrderedKey "latitude" double obj
    <*> atOrderedKey "longitude" double obj
    <*> atOrderedKey "tags" (list text) obj
    <*> (Identity <$> atOrderedKey "friends" (list decodeFriend) obj)
    <*> atOrderedKey "greeting" (nullable text) obj
    <*> atOrderedKey "favoriteFruit" text obj

decodeFriend :: Value -> IO Friend
decodeFriend = withObject $ \obj ->
  Friend
    <$> atOrderedKey "id" int obj
    <*> atOrderedKey "name" text obj

data Friend =
  Friend
    { id   :: Int
    , name :: Text
    } deriving (Show, Generic, NFData)

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

decodePersonUnordered :: Value -> IO PersonUnordered
decodePersonUnordered = withObject $ \obj ->
  PersonUnordered
    <$> atKey "favoriteFruit" text obj
    <*> atKey "isActive" bool obj
    <*> atKey "longitude" double obj
    <*> atKey "balance" text obj
    <*> atKey "email" text obj
    <*> atKey "latitude" double obj
    <*> atKey "age" int obj
    <*> atKey "eyeColor" text obj
    <*> atKey "index" int obj
    <*> atKey "guid" text obj
    <*> atKey "name" text obj
    <*> atKey "greeting" text obj
    <*> atOptionalKey "nonexistent" text obj
    <*> atKey "company" text obj
    <*> atKey "picture" text obj
    <*> atKey "phone" text obj
    <*> atKey "_id" text obj
    <*> atKey "address" text obj
    <*> atKey "about" text obj
    <*> atKey "gender" text obj
    <*> atKey "registered" text obj
    <*> atKey "tags" (list text) obj
    <*> atKey "friends" (list decodeFriend) obj

data Twitter =
  Twitter
    { statuses :: [Status]
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

data Status =
  Status
    { user              :: User
    -- metadata object fields
    , result_type       :: Text
    , iso_language_code :: Text
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

data User =
  User
    { screen_name :: Text
    , location    :: Text
    , description :: Text
    , url         :: Maybe Text
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

decodeTwitter :: Value -> IO Twitter
decodeTwitter = withObject $ \obj ->
  Twitter
    <$> atKey "statuses" (list decodeStatus) obj

decodeStatus :: Value -> IO Status
decodeStatus = withObject $ \obj -> do
    u <- atKey "user" decodeUser obj
    md <- atKey "metadata" pure obj
    flip withObject md $ \m -> do
      result <- atKey "result_type" text m
      iso <- atKey "iso_language_code" text m
      pure $ Status u result iso

decodeUser :: Value -> IO User
decodeUser = withObject $ \obj ->
  User
    <$> atOrderedKey "screen_name" text obj
    <*> atOrderedKey "location" text obj
    <*> atOrderedKey "description" text obj
    <*> atOrderedKey "url" (nullable text) obj

