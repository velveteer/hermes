{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Applicative (many)
import           Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Decoding as A.D
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.JsonStream.Parser as J
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Test.Tasty.Bench (defaultMain, bench, bgroup, nf, env)

import           Data.Hermes
import qualified Data.Hermes as H
import qualified Data.Hermes.Aeson as H

main :: IO ()
main = defaultMain [
  env (BS.readFile "json/persons9000.json") $ \persons ->
  env (BS.readFile "json/twitter100.json") $ \twitter ->
  env (pure $ genDoubles 1000000 1.23456789) $ \doubles ->
  env H.mkHermesEnv_ $ \hEnv ->
    bgroup "Decode" [
      bgroup "Arrays" [
        bench "Hermes" $ nf (H.parseByteString hEnv decodeDoubles) doubles
      , bench "Aeson" $ nf (A.D.decodeStrict @[[Double]]) doubles
      ]
    , bgroup "Persons" [
        bench "Hermes" $ nf (H.parseByteString hEnv (list decodePerson)) persons
      , bench "Aeson" $ nf (A.D.decodeStrict @[Person]) persons
      ]
    , bgroup "Partial Twitter" [
        bench "Hermes" $ nf (H.parseByteString hEnv decodeTwitter) twitter
      , bench "JsonStream" $ nf (J.parseByteString parseTwitter) twitter
      , bench "Aeson" $ nf (A.D.decodeStrict @Twitter) twitter
      ]
    , bgroup "Persons (Aeson Value)" [
        bench "Hermes" $ nf (H.parseByteString hEnv H.hValueToAeson) persons
      , bench "Aeson" $ nf (A.D.decodeStrict @Aeson.Value) persons
      ]
    , bgroup "Twitter (Aeson Value)" [
        bench "Hermes" $ nf (H.parseByteString hEnv H.hValueToAeson) twitter
      , bench "Aeson" $ nf (A.D.decodeStrict @Aeson.Value) twitter
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
    , latitude      :: Scientific
    , longitude     :: Scientific
    , tags          :: [Text]
    , friends       :: [Friend]
    , greeting      :: Maybe Text
    , favoriteFruit :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData

decodePerson :: Decoder Person
decodePerson = withObject $ \obj ->
  Person
    <$> atKey "_id" text obj
    <*> atKey "index" int obj
    <*> atKey "guid" text obj
    <*> atKey "isActive" bool obj
    <*> atKey "balance" text obj
    <*> atKey "picture" (nullable text) obj
    <*> atKey "age" int obj
    <*> atKey "eyeColor" text obj
    <*> atKey "name" text obj
    <*> atKey "gender" text obj
    <*> atKey "company" text obj
    <*> atKey "email" text obj
    <*> atKey "phone" text obj
    <*> atKey "address" text obj
    <*> atKey "about" text obj
    <*> atKey "registered" text obj
    <*> atKey "latitude" scientific obj
    <*> atKey "longitude" scientific obj
    <*> atKey "tags" (list text) obj
    <*> atKey "friends" (list decodeFriend) obj
    <*> atKey "greeting" (nullable text) obj
    <*> atKey "favoriteFruit" text obj

decodeFriend :: Decoder Friend
decodeFriend = withObject $ \obj ->
  Friend
    <$> atKey "id" int obj
    <*> atKey "name" text obj

data Friend =
  Friend
    { id   :: Int
    , name :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData

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

data Twitter =
  Twitter
    { statuses :: [Status]
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData, Aeson.FromJSON)

data Status =
  Status
    { user     :: User
    , metadata :: Map Text Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData, Aeson.FromJSON)

data User =
  User
    { screen_name :: Text
    , location    :: Text
    , description :: Text
    , url         :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData, Aeson.FromJSON)

decodeTwitter :: Decoder Twitter
decodeTwitter = withObject $ \obj ->
  Twitter
    <$> atKey "statuses" (list decodeStatus) obj

decodeStatus :: Decoder Status
decodeStatus = withObject $ \obj -> do
  u <- atKey "user" decodeUser obj
  mdMap <- atKey "metadata" (objectAsMap pure text) obj
  pure $ Status u mdMap

decodeUser :: Decoder User
decodeUser = withObject $ \obj -> do
  User
    <$> atKey "screen_name" text obj
    <*> atKey "location" text obj
    <*> atKey "description" text obj
    <*> atKey "url" (nullable text) obj

parseTwitter :: J.Parser Twitter
parseTwitter =
  J.objectOf $
    Twitter
      <$> "statuses" J..: many (J.arrayOf parseStatus)

parseStatus :: J.Parser Status
parseStatus =
  J.objectOf $
    Status
      <$> "user" J..: parseUser
      <*> "metadata" J..: fmap Map.fromList (many $ J.objectItems J.string)

parseUser :: J.Parser User
parseUser =
  J.objectOf $
    User
      <$> "screen_name" J..: J.string
      <*> "location" J..: J.string
      <*> "description" J..: J.string
      <*> "url" J..: J.nullable J.string

genDoubles :: Int -> Double -> BS.ByteString
genDoubles x v = BSL.toStrict . Aeson.encode $ replicate x (replicate 3 v)

decodeDoubles :: Decoder [[Double]]
decodeDoubles = list $ list double
