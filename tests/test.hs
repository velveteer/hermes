{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Sci
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Time as Time
import           GHC.Generics (Generic)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           Data.Hermes

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties" [rtProp, rtPropOptional, rtErrors]

units :: TestTree
units = testGroup "Units" [aesonIEEE754]

rtProp :: TestTree
rtProp = testProperty "Round Trip With Aeson.ToJSON" $
  withTests 1000 . property $ do
    p <- forAll genPerson
    encoded <- pure . BSL.toStrict . A.encode $ p
    dp <- evalIO $ decode decodePerson encoded
    p === dp

rtPropOptional :: TestTree
rtPropOptional = testProperty "Round Trip With Aeson.ToJSON (Optional Keys)" $
  withTests 1000 . property $ do
    p <- forAll genPersonOptional
    encoded <- pure . BSL.toStrict . A.encode $ p
    dp <- evalIO $ decode decodePersonOptional encoded
    p === dp

rtErrors :: TestTree
rtErrors = testProperty "Errors Should Not Break Referential Transparency" $
  withTests 1000 . property $ do
    p <- forAll
       $ Gen.element
       [ "{"
       , "}"
       , "{\"}"
       , "true"
       , "{\"key\": }"
       , "["
       , "]"
       , "\x00"
       , "{\"_id\": false }"
       ]
    let d1 = decodeEither decodePerson p
        d2 = decodeEither decodePerson p
    d1 === d2

makeDummyObj :: A.ToJSON value => value -> BS.ByteString
makeDummyObj v = "{ \"_\": " <> (BSL.toStrict . A.encode $ v) <> "}"

dummyDecoder :: (Value -> Decoder a) -> Value -> Decoder a
dummyDecoder d = withObject $ atKey "_" d

aesonIEEE754 :: TestTree
aesonIEEE754 = testGroup "Decodes IEEE 754 Floating Point"
  [ testCase "Infinity" $
      decodeEither (dummyDecoder doubleNonFinite) (makeDummyObj @Double (1/0))
        @?= (Right (1/0))
  , testCase "-Infinity" $
      decodeEither (dummyDecoder doubleNonFinite) (makeDummyObj @Double ((-1)/0))
        @?= (Right ((-1)/0))
  , testCase "NaN" $
      fmap isNaN
        (decodeEither (dummyDecoder doubleNonFinite) (makeDummyObj @Double (0/0)))
        @?= (Right True)
  , testCase "null" $
      fmap isNaN
        (decodeEither (dummyDecoder doubleNonFinite) (makeDummyObj @(Maybe Double) Nothing))
        @?= (Right True)
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
    , latitude      :: Double
    , longitude     :: Double
    , tags          :: [Text]
    , friends       :: [Friend]
    , doubles       :: [[Double]]
    , greeting      :: Maybe Text
    , favoriteFruit :: Text
    , employer      :: Employer
    , mapOfInts     :: Map KeyType Int
    , utcTimeField  :: Time.UTCTime
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (A.ToJSON)

data PersonOptional =
  PersonOptional
    { _id           :: Maybe Text
    , index         :: Maybe Int
    , guid          :: Maybe Text
    , isActive      :: Maybe Bool
    , balance       :: Maybe Text
    , picture       :: Maybe (Maybe Text)
    , age           :: Maybe Int
    , latitude      :: Maybe Double
    , longitude     :: Maybe Double
    , tags          :: Maybe [Text]
    , friends       :: Maybe [Friend]
    , doubles       :: Maybe [[Double]]
    , greeting      :: Maybe (Maybe Text)
    , favoriteFruit :: Maybe Text
    , employer      :: Maybe Employer
    , mapOfInts     :: Maybe (Map KeyType Int)
    , utcTimeField  :: Maybe Time.UTCTime
    }
    deriving stock (Eq, Show, Generic)

instance A.ToJSON PersonOptional where
  toJSON = A.genericToJSON A.defaultOptions { A.omitNothingFields = True }

newtype KeyType = KeyType Text
  deriving newtype (Eq, Ord, Show, A.ToJSON, A.ToJSONKey)

data Friend =
  Friend
    { id   :: Int
    , name :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass A.ToJSON

data Employer =
  Employer
    { inefficient :: String
    , exp :: Scientific
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass A.ToJSON

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
    <*> atKey "latitude" double obj
    <*> atKey "longitude" double obj
    <*> atKey "tags" (list text) obj
    <*> atKey "friends" (list decodeFriend) obj
    <*> atKey "doubles" (list (list double)) obj
    <*> atKey "greeting" (nullable text) obj
    <*> atKey "favoriteFruit" text obj
    <*> atKey "employer" decodeEmployer obj
    <*> (Map.fromList <$> atKey "mapOfInts"
          (objectAsKeyValues (pure . KeyType) int) obj)
    <*> atKey "utcTimeField" utcTime obj

decodePersonOptional :: Value -> Decoder PersonOptional
decodePersonOptional = withObject $ \obj ->
  PersonOptional
    <$> atOptionalKey "_id" text obj
    <*> atOptionalKey "index" int obj
    <*> atOptionalKey "guid" text obj
    <*> atOptionalKey "isActive" bool obj
    <*> atOptionalKey "balance" text obj
    <*> atOptionalKey "picture" (nullable text) obj
    <*> atOptionalKey "age" int obj
    <*> atOptionalKey "latitude" double obj
    <*> atOptionalKey "longitude" double obj
    <*> atOptionalKey "tags" (list text) obj
    <*> atOptionalKey "friends" (list decodeFriend) obj
    <*> atOptionalKey "doubles" (list (list double)) obj
    <*> atOptionalKey "greeting" (nullable text) obj
    <*> atOptionalKey "favoriteFruit" text obj
    <*> atOptionalKey "employer" decodeEmployer obj
    <*> (fmap Map.fromList <$> atOptionalKey "mapOfInts"
          (objectAsKeyValues (pure . KeyType) int) obj)
    <*> atOptionalKey "utcTimeField" utcTime obj

decodeFriend :: Value -> Decoder Friend
decodeFriend = withObject $ \obj ->
  Friend
    <$> atKey "id" int obj
    <*> atKey "name" text obj

genPerson :: Gen Person
genPerson = Person
  <$> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.int (Range.linear minBound maxBound)
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.bool
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.int (Range.linear 0 100)
  <*> genDouble
  <*> genDouble
  <*> Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.list (Range.linear 0 100) genFriend
  <*> Gen.list (Range.linear 0 100) (Gen.list (Range.linear 0 100) genDouble)
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> genEmployer
  <*> Gen.map (Range.linear 0 100)
      ((,) <$> fmap KeyType (Gen.text (Range.linear 0 100) Gen.unicode)
           <*> Gen.int (Range.linear 0 10000))
  <*> utcTimeGenerator

genPersonOptional :: Gen PersonOptional
genPersonOptional = PersonOptional
  <$> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.maybe (Gen.int (Range.linear minBound maxBound))
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.maybe Gen.bool
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.maybe (Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode))
  <*> Gen.maybe (Gen.int (Range.linear 0 100))
  <*> Gen.maybe genDouble
  <*> Gen.maybe genDouble
  <*> Gen.maybe (Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 100) Gen.unicode))
  <*> Gen.maybe (Gen.list (Range.linear 0 100) genFriend)
  <*> Gen.maybe (Gen.list (Range.linear 0 100) (Gen.list (Range.linear 0 100) genDouble))
  <*> Gen.maybe (Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode))
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.maybe genEmployer
  <*> Gen.maybe (Gen.map (Range.linear 0 100)
      ((,) <$> fmap KeyType (Gen.text (Range.linear 0 100) Gen.unicode)
           <*> Gen.int (Range.linear 0 10000)))
  <*> Gen.maybe utcTimeGenerator

genFriend :: Gen Friend
genFriend = Friend
  <$> Gen.int (Range.linear 0 100)
  <*> Gen.text (Range.linear 0 100) Gen.unicode

genEmployer :: Gen Employer
genEmployer = Employer
  <$> Gen.string (Range.linear 0 1000) Gen.unicode
  <*> genScientific

decodeEmployer :: Value -> Decoder Employer
decodeEmployer = withObject $ \obj ->
  Employer
    <$> atKey "inefficient" string obj
    <*> atKey "exp" scientific obj

utcTimeGenerator :: Gen Time.UTCTime
utcTimeGenerator =
  Time.UTCTime
    <$> dayGenerator
    <*> timeOfDayGenerator

dayGenerator :: Gen Time.Day
dayGenerator =
  Time.fromGregorian
    <$> Gen.integral (Range.linearFrom 2020 1980 2060)
    <*> Gen.integral (Range.constant 1 12)
    <*> Gen.integral (Range.constant 1 28)

timeOfDayGenerator :: Gen Time.DiffTime
timeOfDayGenerator =
  fmap (\x -> fromIntegral (floor $ (x :: Time.DiffTime) * 1000000 :: Int) / 1000000)
    $ Gen.realFrac_ (Range.constant 0 86400)

genDouble :: Gen Double
genDouble =
  Gen.double $ Range.linearFracFrom
    0
    (-1000000000000000000000000)
    1000000000000000000000000

genScientific :: Gen Scientific
genScientific = fmap Sci.fromFloatDigits $ genDouble
