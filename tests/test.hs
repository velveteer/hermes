{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
import qualified Data.Vector as V
import           Data.Word (Word16)
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
properties = testGroup "Properties" [rtProp, rtPropOptional, rtErrors, rtRecursiveDataType]

genPeano :: MonadGen m => m Peano
genPeano = Peano <$> Gen.word16 Range.linearBounded

rtRecursiveDataType :: TestTree
rtRecursiveDataType = testProperty "Round Trip With Recursive Data Type" $
  property $ do
    t <- forAll genPeano
    dt <- roundtrip decodePeano t
    t === dt

roundtrip :: A.ToJSON a => Decoder a -> a -> PropertyT IO a
roundtrip decoder =
  either (fail . show) pure . decodeEither decoder . BSL.toStrict . A.encode

newtype Peano = Peano Word16
  deriving (Eq, Show)

instance A.ToJSON Peano where
  toJSON (Peano 0) = A.object []
  toJSON (Peano suc) = A.object [("suc", A.toJSON (Peano $ suc - 1))]

decodePeano :: Decoder Peano
decodePeano = withObject $ \obj -> do
  mPeano <- atKeyOptional "suc" decodePeano obj
  pure $
    case mPeano of
      Just (Peano subTree) -> Peano $ 1 + subTree
      Nothing -> Peano 0

rtProp :: TestTree
rtProp = testProperty "Round Trip With Aeson.ToJSON" $
  withTests 1000 . property $ do
    p <- forAll genPerson
    dp <- roundtrip decodePerson p
    p === dp

rtPropOptional :: TestTree
rtPropOptional = testProperty "Round Trip With Aeson.ToJSON (Optional Keys)" $
  withTests 1000 . property $ do
    p <- forAll genPersonOptional
    dp <- roundtrip decodePersonOptional p
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
    , tags          :: V.Vector Text
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
    , tags          :: Maybe (V.Vector Text)
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
    <*> atKey "latitude" double obj
    <*> atKey "longitude" double obj
    <*> atKey "tags" (vector text) obj
    <*> atKey "friends" (list decodeFriend) obj
    <*> atKey "doubles" (list (list double)) obj
    <*> atKey "greeting" (nullable text) obj
    <*> atKey "favoriteFruit" text obj
    <*> atKey "employer" decodeEmployer obj
    <*> (Map.fromList <$> atKey "mapOfInts"
          (objectAsKeyValues (pure . KeyType) int) obj)
    <*> atKey "utcTimeField" utcTime obj

decodePersonOptional :: Decoder PersonOptional
decodePersonOptional = withObject $ \obj ->
  PersonOptional
    <$> atKeyOptional "_id" text obj
    <*> atKeyOptional "index" int obj
    <*> atKeyOptional "guid" text obj
    <*> atKeyOptional "isActive" bool obj
    <*> atKeyOptional "balance" text obj
    <*> atKeyOptional "picture" (nullable text) obj
    <*> atKeyOptional "age" int obj
    <*> atKeyOptional "latitude" double obj
    <*> atKeyOptional "longitude" double obj
    <*> atKeyOptional "tags" (vector text) obj
    <*> atKeyOptional "friends" (list decodeFriend) obj
    <*> atKeyOptional "doubles" (list (list double)) obj
    <*> atKeyOptional "greeting" (nullable text) obj
    <*> atKeyOptional "favoriteFruit" text obj
    <*> atKeyOptional "employer" decodeEmployer obj
    <*> (fmap Map.fromList <$> atKeyOptional "mapOfInts"
          (objectAsKeyValues (pure . KeyType) int) obj)
    <*> atKeyOptional "utcTimeField" utcTime obj

decodeFriend :: Decoder Friend
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
  <*> (V.fromList <$> Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 100) Gen.unicode))
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
  <*> Gen.maybe (V.fromList <$> Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 100) Gen.unicode))
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

decodeEmployer :: Decoder Employer
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
