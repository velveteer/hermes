{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
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
import           Test.Tasty.Hedgehog

import           Data.Hermes

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [rtProp]

rtProp :: TestTree
rtProp = testProperty "Round Trip With Aeson.ToJSON" $
  withTests 1000 . property $ do
    p <- forAll genPerson
    encoded <- pure . BSL.toStrict . A.encode $ p
    dp <- evalIO $ decode encoded decodePerson
    p === dp

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
    , greeting      :: Maybe Text
    , favoriteFruit :: Text
    , employer      :: Employer
    , mapOfInts     :: Map KeyType Int
    , utcTimeField  :: Time.UTCTime
    } deriving (Eq, Show, Generic, A.ToJSON)

newtype KeyType = KeyType Text
  deriving newtype (Eq, Ord, Show, A.ToJSON, A.ToJSONKey)

data Friend =
  Friend
    { id   :: Int
    , name :: Text
    } deriving (Eq, Show, A.ToJSON, Generic)

data Employer =
  Employer
    { inefficient :: String
    , exp :: Scientific
    } deriving (Eq, Show, A.ToJSON, Generic)

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
    <*> atKey "greeting" (nullable text) obj
    <*> atKey "favoriteFruit" text obj
    <*> atKey "employer" decodeEmployer obj
    <*> (Map.fromList <$> atKey "mapOfInts" (objectAsKeyValues (pure . KeyType) int) obj)
    <*> atKey "utcTimeField" utcTime obj

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
  <*> Gen.double (Range.constant 0 10000000000000000)
  <*> Gen.double (Range.constant 0 10000000000000000)
  <*> Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.list (Range.linear 0 100) genFriend
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> genEmployer
  <*> Gen.map (Range.linear 0 100)
      ((,) <$> (fmap KeyType $ Gen.text (Range.linear 0 100) Gen.unicode)
           <*> (Gen.int (Range.linear 0 10000)))
  <*> utcTimeGenerator

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
  -- Although the UTCTime docs allow for leap seconds to be represented and
  -- Aeson is happy to *serialize* values with leap seconds, we will fail to
  -- parse values with leap seconds. If we ever need to support decoding times
  -- with leap seconds, we will need to do some work.
  --
  -- This issue discusses the UTCTime serialization/deserialization asymmetry:
  --
  --   https://github.com/bos/aeson/issues/500
  fmap (\x -> fromIntegral (floor $ (x :: Time.DiffTime) * 1000000 :: Int) / 1000000)
    $ Gen.realFrac_ (Range.constant 0 86400)

genScientific :: Gen Scientific
genScientific =
  fmap Sci.fromFloatDigits $
    Gen.double $
      Range.linearFracFrom
        0
        (-1000000000000000000000000)
        1000000000000000000000000
