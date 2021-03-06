{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Test.Tasty (withResource)
import           Test.Tasty.Bench
-- import qualified Waargonaut.Attoparsec as WA
-- import qualified Waargonaut.Decode as D
-- ^ TODO waargonaut does not yet compile on GHC 9.2.1

import           Data.Hermes

genArr :: Aeson.ToJSON v => Int -> v -> BS.ByteString
genArr x v = "{\"_\": " <> BSL.toStrict (Aeson.encode (replicate x (replicate 3 v))) <> "}"

main :: IO ()
main = defaultMain
  [ bgroup "1 Million 3-Arrays"
      [ bench "Hermes [[Double]]" $ nf
        (decodeEither (withObject $ atKey "_" (list $ list double))) $
          genArr @Double 1000000 1.23456789
      , bench "Aeson [[Double]]" $ nf
        (Aeson.decodeStrict' @[[Double]]) $
          genArr @Double 1000000 1.23456789
      ]
  , withResource (BS.readFile "json/small_map.json") (const $ pure ()) $ \input ->
    bgroup "Small Object to Map"
      [ bench "Hermes Decode" $
          nfIO (decodeEither (objectAsKeyValues pure int) <$> input)
      -- , bench "Hermes DecodeWith" $
      --     nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (objectAsKeyValues pure int)})
      , bench "Aeson Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe (Map Text Int)))
      , bench "Aeson Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe (Map Text Int)))
      -- , bench "Waargonaut Attoparsec" $
      --     nfIO (void $ WA.decodeAttoparsecByteString (D.objectAsKeyValues D.text D.int) =<< input)
      ]
  , withResource (BS.readFile "json/persons9000.json") (const $ pure ()) $ \input ->
    bgroup "Full Persons Array"
    [ bgroup "Ordered Keys"
      [ bench "Hermes Decode" $
          nfIO (decodeEither (list decodePerson) <$> input)
      -- , bench "Hermes DecodeWith" $
      --     nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (list decodePerson)})
      , bench "Aeson Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [Person]))
      , bench "Aeson Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [Person]))
      -- , bench "Waargonaut Decode Attoparsec" $
      --     nfIO (void $ WA.decodeAttoparsecByteString (D.list personDecoder) =<< input)
      ]
    , bgroup "Unordered Keys"
      [ bench "Hermes Decode" $
          nfIO (decodeEither (list decodePersonUnordered) <$> input)
      -- , bench "Hermes DecodeWith" $
      --     nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (list decodePersonUnordered)})
      , bench "Aeson Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [PersonUnordered]))
      , bench "Aeson Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [PersonUnordered]))
      -- , bench "Waargonaut Decode Attoparsec" $
      --     nfIO (void $ WA.decodeAttoparsecByteString (D.list personUnorderedDecoder) =<< input)
      ]
    ]
  , withResource (BS.readFile "json/twitter100.json") (const $ pure ()) $ \input ->
    bgroup "Partial Twitter"
    [ bench "Hermes Decode" $
        nfIO (decodeEither decodeTwitter <$> input)
    -- , bench "Hermes DecodeWith" $
    --     nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs decodeTwitter})
    , bench "Aeson Lazy" $
        nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe Twitter))
    , bench "Aeson Strict" $
        nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe Twitter))
    -- , bench "Waargonaut Attoparsec" $
    --     nfIO (void $ WA.decodeAttoparsecByteString twitterDecoder =<< input)
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

decodePerson :: Value -> Decoder Person
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

decodeFriend :: Value -> Decoder Friend
decodeFriend = withObject $ \obj ->
  Friend
    <$> atKey "id" int obj
    <*> atKey "name" text obj

-- personDecoder :: Monad f => D.Decoder f Person
-- personDecoder =
--   Person
--     <$> D.atKey "_id" D.text
--     <*> D.atKey "index" D.int
--     <*> D.atKey "guid" D.text
--     <*> D.atKey "isActive" D.bool
--     <*> D.atKey "balance" D.text
--     <*> D.atKey "picture" (D.maybeOrNull D.text)
--     <*> D.atKey "age" D.int
--     <*> D.atKey "eyeColor" D.text
--     <*> D.atKey "name" D.text
--     <*> D.atKey "gender" D.text
--     <*> D.atKey "company" D.text
--     <*> D.atKey "email" D.text
--     <*> D.atKey "phone" D.text
--     <*> D.atKey "address" D.text
--     <*> D.atKey "about" D.text
--     <*> D.atKey "registered" D.text
--     <*> D.atKey "latitude" D.scientific
--     <*> D.atKey "longitude" D.scientific
--     <*> D.atKey "tags" (D.list D.text)
--     <*> D.atKey "friends" (D.list friendDecoder)
--     <*> D.atKey "greeting" (D.maybeOrNull D.text)
--     <*> D.atKey "favoriteFruit" D.text

-- friendDecoder :: Monad f => D.Decoder f Friend
-- friendDecoder =
--   Friend
--     <$> D.atKey "id" D.int
--     <*> D.atKey "name" D.text

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

data PersonUnordered =
  PersonUnordered
    { favoriteFruit :: Text
    , isActive      :: Bool
    , longitude     :: Scientific
    , balance       :: Text
    , email         :: Text
    , latitude      :: Scientific
    , age           :: Int
    , eyeColor      :: Text
    , index         :: Int
    , guid          :: Text
    , name          :: Text
    , greeting      :: Maybe Text
    , company       :: Text
    , picture       :: Maybe Text
    , phone         :: Text
    , _id           :: Text
    , address       :: Text
    , about         :: Text
    , gender        :: Text
    , registered    :: Text
    , tags          :: [Text]
    , friends       :: [Friend]
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData

instance Aeson.FromJSON PersonUnordered where
  parseJSON = Aeson.withObject "Person" $ \obj ->
    PersonUnordered
      <$> obj Aeson..: "favoriteFruit"
      <*> obj Aeson..: "isActive"
      <*> obj Aeson..: "longitude"
      <*> obj Aeson..: "balance"
      <*> obj Aeson..: "email"
      <*> obj Aeson..: "latitude"
      <*> obj Aeson..: "age"
      <*> obj Aeson..: "eyeColor"
      <*> obj Aeson..: "index"
      <*> obj Aeson..: "guid"
      <*> obj Aeson..: "name"
      <*> obj Aeson..: "greeting"
      <*> obj Aeson..: "company"
      <*> obj Aeson..: "picture"
      <*> obj Aeson..: "phone"
      <*> obj Aeson..: "_id"
      <*> obj Aeson..: "address"
      <*> obj Aeson..: "about"
      <*> obj Aeson..: "gender"
      <*> obj Aeson..: "registered"
      <*> obj Aeson..: "tags"
      <*> obj Aeson..: "friends"

decodePersonUnordered :: Value -> Decoder PersonUnordered
decodePersonUnordered = withObject $ \obj ->
  PersonUnordered
    <$> atKey "favoriteFruit" text obj
    <*> atKey "isActive" bool obj
    <*> atKey "longitude" scientific obj
    <*> atKey "balance" text obj
    <*> atKey "email" text obj
    <*> atKey "latitude" scientific obj
    <*> atKey "age" int obj
    <*> atKey "eyeColor" text obj
    <*> atKey "index" int obj
    <*> atKey "guid" text obj
    <*> atKey "name" text obj
    <*> atKey "greeting" (nullable text) obj
    <*> atKey "company" text obj
    <*> atKey "picture" (nullable text) obj
    <*> atKey "phone" text obj
    <*> atKey "_id" text obj
    <*> atKey "address" text obj
    <*> atKey "about" text obj
    <*> atKey "gender" text obj
    <*> atKey "registered" text obj
    <*> atKey "tags" (list text) obj
    <*> atKey "friends" (list decodeFriend) obj

-- personUnorderedDecoder :: Monad f => D.Decoder f PersonUnordered
-- personUnorderedDecoder =
--   PersonUnordered
--     <$> D.atKey "favoriteFruit" D.text
--     <*> D.atKey "isActive" D.bool
--     <*> D.atKey "longitude" D.scientific
--     <*> D.atKey "balance" D.text
--     <*> D.atKey "email" D.text
--     <*> D.atKey "latitude" D.scientific
--     <*> D.atKey "age" D.int
--     <*> D.atKey "eyeColor" D.text
--     <*> D.atKey "index" D.int
--     <*> D.atKey "guid" D.text
--     <*> D.atKey "name" D.text
--     <*> D.atKey "greeting" (D.maybeOrNull D.text)
--     <*> D.atKey "company" D.text
--     <*> D.atKey "picture" (D.maybeOrNull D.text)
--     <*> D.atKey "phone" D.text
--     <*> D.atKey "_id" D.text
--     <*> D.atKey "address" D.text
--     <*> D.atKey "about" D.text
--     <*> D.atKey "gender" D.text
--     <*> D.atKey "registered" D.text
--     <*> D.atKey "tags" (D.list D.text)
--     <*> D.atKey "friends" (D.list friendDecoder)

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

decodeTwitter :: Value -> Decoder Twitter
decodeTwitter = withObject $ \obj ->
  Twitter
    <$> atKey "statuses" (list decodeStatus) obj

decodeStatus :: Value -> Decoder Status
decodeStatus = withObject $ \obj -> do
  u <- atKey "user" decodeUser obj
  mdMap <- Map.fromList <$> atKey "metadata" (objectAsKeyValues pure text) obj
  pure $ Status u mdMap

decodeUser :: Value -> Decoder User
decodeUser = withObject $ \obj ->
  User
    <$> atKey "screen_name" text obj
    <*> atKey "location" text obj
    <*> atKey "description" text obj
    <*> atKey "url" (nullable text) obj

-- twitterDecoder :: Monad f => D.Decoder f Twitter
-- twitterDecoder =
--   Twitter
--     <$> D.atKey "statuses" (D.list statusDecoder)

-- statusDecoder :: Monad f => D.Decoder f Status
-- statusDecoder = D.withCursor $ \curs -> do
--   u       <- D.fromKey "user" userDecoder curs
--   mdMap   <- Map.fromList <$> D.fromKey "metadata" (D.objectAsKeyValues D.text D.text) curs
--   newCurs <- D.moveToKey "metadata" curs
--   result  <- D.fromKey "result_type" D.text newCurs
--   iso     <- D.fromKey "iso_language_code" D.text newCurs
--   pure $ Status u mdMap result iso

-- userDecoder :: Monad f => D.Decoder f User
-- userDecoder =
--   User
--     <$> D.atKey "screen_name" D.text
--     <*> D.atKey "location" D.text
--     <*> D.atKey "description" D.text
--     <*> D.atKey "url" (D.maybeOrNull D.text)
