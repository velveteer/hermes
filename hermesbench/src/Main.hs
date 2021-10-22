{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.DeepSeq (NFData)
import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import           Data.Functor.Identity (Identity(..))
import           Data.Scientific (Scientific, toRealFloat)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Test.Tasty (withResource)
import           Test.Tasty.Bench
import qualified Waargonaut.Attoparsec as WA
import qualified Waargonaut.Decode as D

import           Data.Hermes

main :: IO ()
main = defaultMain
  [ withResource (BS.readFile "json/persons9000.json") (const $ pure ()) $ \input ->
    withResource mkHermesEnv_ (const $ pure ()) $ \envIO ->
    bgroup "Full Decode Persons Array JSON"
    [ bgroup "Ordered Keys"
      [ bench "Hermes Decode" $
          nfIO (flip decode (list decodePerson) =<< input)
      , bench "Hermes DecodeWith" $
          nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (list decodePerson)})
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [Person]))
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [Person]))
      -- , bench "Waargonaut Decode Attoparsec" $
      --     nfIO (void $ WA.decodeAttoparsecByteString (D.list personDecoder) =<< input)
      ]
    , bgroup "Unordered Keys"
      [ bench "Hermes Decode" $
          nfIO (flip decode (list decodePersonUnordered) =<< input)
      , bench "Hermes DecodeWith" $
          nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs (list decodePersonUnordered)})
      , bench "Aeson Decode Lazy" $
          nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe [PersonUnordered]))
      , bench "Aeson Decode Strict" $
          nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe [PersonUnordered]))
      -- , bench "Waargonaut Decode Attoparsec" $
      --     nfIO (void $ WA.decodeAttoparsecByteString (D.list personUnorderedDecoder) =<< input)
      ]
    ]
  , withResource (BS.readFile "json/twitter100.json") (const $ pure ()) $ \input ->
    withResource mkHermesEnv_ (const $ pure ()) $ \envIO ->
    bgroup "Partial Decode Twitter JSON"
    [ bench "Hermes Decode" $
        nfIO (flip decode decodeTwitter =<< input)
    , bench "Hermes DecodeWith" $
        nfIO (do { env' <- envIO; bs <- input; decodeWith env' bs decodeTwitter})
    , bench "Aeson Decode Lazy" $
        nfIO ((Aeson.decodeStrict <$> input) :: IO (Maybe Twitter))
    , bench "Aeson Decode Strict" $
        nfIO ((Aeson.decodeStrict' <$> input) :: IO (Maybe Twitter))
    , bench "Waargonaut Decode Attoparsec" $
        nfIO (void $ WA.decodeAttoparsecByteString twitterDecoder =<< input)
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
    , latitude      :: Scientific
    , longitude     :: Double
    , tags          :: [Text]
    , friends       :: Identity [Friend]
    , greeting      :: Maybe Text
    , favoriteFruit :: Text
    } deriving (Show, Generic, NFData)

decodePerson :: Value -> Decoder Person
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
    <*> atOrderedKey "latitude" scientific obj
    <*> (toRealFloat <$> atOrderedKey "longitude" scientific obj)
    <*> atOrderedKey "tags" (list text) obj
    <*> (Identity <$> atOrderedKey "friends" (list decodeFriend) obj)
    <*> atOrderedKey "greeting" (nullable text) obj
    <*> atOrderedKey "favoriteFruit" text obj

decodeFriend :: Value -> Decoder Friend
decodeFriend = withObject $ \obj ->
  Friend
    <$> atOrderedKey "id" int obj
    <*> atOrderedKey "name" text obj

personDecoder :: Monad f => D.Decoder f Person
personDecoder =
  Person
    <$> D.atKey "_id" D.text
    <*> D.atKey "index" D.int
    <*> D.atKey "guid" D.text
    <*> D.atKey "isActive" D.bool
    <*> D.atKey "balance" D.text
    <*> D.atKey "picture" (D.maybeOrNull D.text)
    <*> D.atKey "age" D.int
    <*> D.atKey "eyeColor" D.text
    <*> D.atKey "name" D.text
    <*> D.atKey "gender" D.text
    <*> D.atKey "company" D.text
    <*> D.atKey "email" D.text
    <*> (Identity <$> D.atKey "phone" D.text)
    <*> D.atKey "address" D.text
    <*> D.atKey "about" D.text
    <*> D.atKey "registered" D.text
    <*> D.atKey "latitude" D.scientific
    <*> (toRealFloat <$> D.atKey "longitude" D.scientific)
    <*> D.atKey "tags" (D.list D.text)
    <*> (Identity <$> D.atKey "friends" (D.list friendDecoder))
    <*> D.atKey "greeting" (D.maybeOrNull D.text)
    <*> D.atKey "favoriteFruit" D.text

friendDecoder :: Monad f => D.Decoder f Friend
friendDecoder =
  Friend
    <$> D.atKey "id" D.int
    <*> D.atKey "name" D.text

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
    , latitude      :: Scientific
    , age           :: Int
    , eyeColor      :: Text
    , index         :: Int
    , guid          :: Text
    , name          :: Text
    , greeting      :: Maybe Text
    , nonexistent   :: Maybe Text
    , company       :: Text
    , picture       :: Maybe Text
    , phone         :: Identity Text
    , _id           :: Text
    , address       :: Text
    , about         :: Text
    , gender        :: Text
    , registered    :: Text
    , tags          :: [Text]
    , friends       :: Identity [Friend]
    } deriving (Show, Generic, NFData, Aeson.FromJSON)

decodePersonUnordered :: Value -> Decoder PersonUnordered
decodePersonUnordered = withObject $ \obj ->
  PersonUnordered
    <$> atKey "favoriteFruit" text obj
    <*> atKey "isActive" bool obj
    <*> (toRealFloat <$> atKey "longitude" scientific obj)
    <*> atKey "balance" text obj
    <*> atKey "email" text obj
    <*> atKey "latitude" scientific obj
    <*> atKey "age" int obj
    <*> atKey "eyeColor" text obj
    <*> atKey "index" int obj
    <*> atKey "guid" text obj
    <*> atKey "name" text obj
    <*> atKey "greeting" (nullable text) obj
    <*> atOptionalKey "nonexistent" text obj
    <*> atKey "company" text obj
    <*> atKey "picture" (nullable text) obj
    <*> (Identity <$> atKey "phone" text obj)
    <*> atKey "_id" text obj
    <*> atKey "address" text obj
    <*> atKey "about" text obj
    <*> atKey "gender" text obj
    <*> atKey "registered" text obj
    <*> atKey "tags" (list text) obj
    <*> (Identity <$> atKey "friends" (list decodeFriend) obj)

personUnorderedDecoder :: Monad f => D.Decoder f PersonUnordered
personUnorderedDecoder =
  PersonUnordered
    <$> D.atKey "favoriteFruit" D.text
    <*> D.atKey "isActive" D.bool
    <*> (toRealFloat <$> D.atKey "longitude" D.scientific)
    <*> D.atKey "balance" D.text
    <*> D.atKey "email" D.text
    <*> D.atKey "latitude" D.scientific
    <*> D.atKey "age" D.int
    <*> D.atKey "eyeColor" D.text
    <*> D.atKey "index" D.int
    <*> D.atKey "guid" D.text
    <*> D.atKey "name" D.text
    <*> D.atKey "greeting" (D.maybeOrNull D.text)
    <*> D.atKeyOptional "nonexistent" D.text
    <*> D.atKey "company" D.text
    <*> D.atKey "picture" (D.maybeOrNull D.text)
    <*> (Identity <$> D.atKey "phone" D.text)
    <*> D.atKey "_id" D.text
    <*> D.atKey "address" D.text
    <*> D.atKey "about" D.text
    <*> D.atKey "gender" D.text
    <*> D.atKey "registered" D.text
    <*> D.atKey "tags" (D.list D.text)
    <*> (Identity <$> D.atKey "friends" (D.list friendDecoder))

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

decodeTwitter :: Value -> Decoder Twitter
decodeTwitter = withObject $ \obj ->
  Twitter
    <$> atKey "statuses" (list decodeStatus) obj

decodeStatus :: Value -> Decoder Status
decodeStatus = withObject $ \obj -> do
  u <- atKey "user" decodeUser obj
  md <- atKey "metadata" pure obj
  flip withObject md $ \m -> do
    result <- atKey "result_type" text m
    iso <- atKey "iso_language_code" text m
    pure $ Status u result iso

decodeUser :: Value -> Decoder User
decodeUser = withObject $ \obj ->
  User
    <$> atOrderedKey "screen_name" text obj
    <*> atOrderedKey "location" text obj
    <*> atOrderedKey "description" text obj
    <*> atOrderedKey "url" (nullable text) obj

twitterDecoder :: Monad f => D.Decoder f Twitter
twitterDecoder =
  Twitter
    <$> D.atKey "statuses" (D.list statusDecoder)

statusDecoder :: Monad f => D.Decoder f Status
statusDecoder = D.withCursor $ \curs -> do
  u       <- D.fromKey "user" userDecoder curs
  newCurs <- D.moveToKey "metadata" curs
  result  <- D.fromKey "result_type" D.text newCurs
  iso     <- D.fromKey "iso_language_code" D.text newCurs
  pure $ Status u result iso

userDecoder :: Monad f => D.Decoder f User
userDecoder =
  User
    <$> D.atKey "screen_name" D.text
    <*> D.atKey "location" D.text
    <*> D.atKey "description" D.text
    <*> D.atKey "url" (D.maybeOrNull D.text)
