{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Applicative (many)
import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
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
import           Test.Tasty.Bench (defaultMain, bench, nf)

import           Data.Hermes
import qualified Data.Hermes as H
import qualified Data.Hermes.Aeson as H

main :: IO ()
main = do
  persons <- BS.readFile "json/persons9000.json"
  twitter <- BS.readFile "json/twitter100.json"
  doubles <- evaluate . force $ genDoubles 10000 1.23456789
  hEnv <- H.mkHermesEnv_
  defaultMain
    [ bench "Hermes Arrays" $ nf (H.parseByteString hEnv decodeDoubles) doubles
    , bench "Aeson Arrays" $ nf (A.D.decodeStrict @[[Double]]) doubles
    , bench "Hermes Persons" $ nf (H.parseByteString hEnv (list decodePerson)) persons
    , bench "Aeson Persons" $ nf (A.D.decodeStrict @[Person]) persons
    , bench "Hermes Partial Twitter" $ nf (H.parseByteString hEnv decodeTwitter) twitter
    , bench "Aeson Partial Twitter" $ nf (A.D.decodeStrict @Twitter) twitter
    , bench "JsonStream Partial Twitter" $ nf (J.parseByteString parseTwitter) twitter
    , bench "Hermes Persons (Aeson Value)" $ nf (H.parseByteString hEnv H.hValueToAeson) persons
    , bench "Aeson Persons (Aeson Value)" $ nf (A.D.decodeStrict @Aeson.Value) persons
    , bench "Hermes Twitter (Aeson Value)" $ nf (H.parseByteString hEnv H.hValueToAeson) twitter
    , bench "Aeson Twitter (Aeson Value)" $ nf (A.D.decodeStrict @Aeson.Value) twitter
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
decodePerson = object $
  Person
    <$> atKey "_id" text
    <*> atKey "index" int
    <*> atKey "guid" text
    <*> atKey "isActive" bool
    <*> atKey "balance" text
    <*> atKey "picture" (nullable text)
    <*> atKey "age" int
    <*> atKey "eyeColor" text
    <*> atKey "name" text
    <*> atKey "gender" text
    <*> atKey "company" text
    <*> atKey "email" text
    <*> atKey "phone" text
    <*> atKey "address" text
    <*> atKey "about" text
    <*> atKey "registered" text
    <*> atKey "latitude" scientific
    <*> atKey "longitude" scientific
    <*> atKey "tags" (list text)
    <*> atKey "friends" (list decodeFriend)
    <*> atKey "greeting" (nullable text)
    <*> atKey "favoriteFruit" text

decodeFriend :: Decoder Friend
decodeFriend = object $
  Friend
    <$> atKey "id" int
    <*> atKey "name" text

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
decodeTwitter = object $
  Twitter
    <$> atKey "statuses" (list decodeStatus)

decodeStatus :: Decoder Status
decodeStatus = object $ do
  u <- atKey "user" decodeUser
  mdMap <- atKey "metadata" (objectAsMap pure text)
  pure $ Status u mdMap

decodeUser :: Decoder User
decodeUser = object $
  User
    <$> atKey "screen_name" text
    <*> atKey "location" text
    <*> atKey "description" text
    <*> atKey "url" (nullable text)

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
