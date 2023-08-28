{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
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
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           Data.Hermes

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties" [rtProp, rtPropOptional, rtErrors, rtRecursiveDataType]

units :: TestTree
units = testGroup "Units" [altCases, objectFields]

rtRecursiveDataType :: TestTree
rtRecursiveDataType = testProperty "Round Trip With Recursive Data Type" $
  property $ do
    t <- forAll genPeano
    dt <- roundtrip decodePeano t
    t === dt

rtProp :: TestTree
rtProp = testProperty "Round Trip With Aeson.ToJSON" $
  property $ do
    p <- forAll genPerson
    dp <- roundtrip decodePerson p
    p === dp

rtPropOptional :: TestTree
rtPropOptional = testProperty "Round Trip With Aeson.ToJSON (Optional Keys)" $
  property $ do
    p <- forAll genPersonOptional
    dp <- roundtrip decodePersonOptional p
    p === dp

rtErrors :: TestTree
rtErrors = testProperty "Errors Should Not Break Referential Transparency" $
  property $ do
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

roundtrip :: A.ToJSON a => Decoder a -> a -> PropertyT IO a
roundtrip decoder =
  either (fail . show) pure . decodeEither decoder . BSL.toStrict . A.encode

altCases :: TestTree
altCases = testGroup "Alternative"
  [ testCase "Object Iterator" $
      decodeEither
        (objectAsMap pure (1 <$ bool) <|> objectAsMap pure int)
        "{ \"key1\": 1, \"key2\": 2 }"
      @?= Right (Map.fromList [("key1", 1), ("key2", 2)])

  , testCase "Array Iterator" $
      decodeEither
        (([1] <$ list bool) <|> list int)
        "[1, 2, 3]"
      @?= Right [1,2,3]

  , testCase "Number Arrays (Not Iterator)" $
      decodeEither
        (listOfDouble <|> ([1.0] <$ listOfInt))
        "[1, true, 3]"
      @?= Left (SIMDException (DocumentError
        { path = ""
        , errorMsg = "Error decoding array of ints. \
                      \INCORRECT_TYPE: The JSON element \
                      \does not have the requested type."
        }))

  , testCase "Inner Array Decoder" $
      decodeEither
        (list ((1 <$ bool) <|> int))
        "[1, 2, 3]"
      @?= Right [1,2,3]

  , testCase "Inner Object Decoder" $
      decodeEither
        (objectAsMap (\txt -> fail "nope" <|> pure txt) ((1 <$ bool) <|> int))
        "{ \"key1\": 1, \"key2\": 2 }"
      @?= Right (Map.fromList [("key1", 1), ("key2", 2)])

  , testCase "Alternative Keys (value error)" $
      decodeEither
        (object $ atKey "key1" (1 <$ bool) <|> atKey "key2" int)
        "{ \"key1\": 1, \"key2\": 2 }"
      @?= Right 2

  , testCase "Alternative Keys (key error)" $
      decodeEither
        (object $ atKey "nope" int <|> atKey "key2" int)
        "{ \"key1\": 1, \"key2\": 2 }"
      @?= Right 2

  , testCase "Alternative Objects" $
      decodeEither
        (object (atKey "key1" (2 <$ bool)) <|> object (atKey "key1" int))
        "{ \"key1\": 1, \"key2\": 2 }"
      @?= Right 1
  ]

objectFields :: TestTree
objectFields = testGroup "Object Fields"
  [ testCase "liftObjectDecoder" $
      decodeEither decodeFriendSlurp "{ \"id\": 1, \"first\": \"Bob\", \"last\": \"Kelso\" }"
    @?= Right (FriendWithMap 1 (Map.fromList [("first", "Bob"), ("last", "Kelso")]))
  ]

data Person =
  Person
    { _id           :: Text
    , index         :: Int
    , guid          :: Text
    , isActive      :: Bool
    , balance       :: Text
    , picture       :: Maybe Text
    , age           :: Word
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
    , age           :: Maybe Word
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
decodePerson = object $
  Person
    <$> atKey "_id" text
    <*> atKey "index" int
    <*> atKey "guid" text
    <*> atKey "isActive" bool
    <*> atKey "balance" text
    <*> atKey "picture" (nullable text)
    <*> atKey "age" uint
    <*> atKey "latitude" double
    <*> atKey "longitude" double
    <*> atKey "tags" (vector text)
    <*> atKey "friends" (list decodeFriend)
    <*> atKey "doubles" (list (list double))
    <*> atKey "greeting" (nullable text)
    <*> atKey "favoriteFruit" text
    <*> atKey "employer" decodeEmployer
    <*> (Map.fromList <$> atKey "mapOfInts"
          (objectAsKeyValues (pure . KeyType) int))
    <*> atKey "utcTimeField" utcTime

decodePersonOptional :: Decoder PersonOptional
decodePersonOptional = object $
  PersonOptional
    <$> atKeyOptional "_id" text
    <*> atKeyOptional "index" int
    <*> atKeyOptional "guid" text
    <*> atKeyOptional "isActive" bool
    <*> atKeyOptional "balance" text
    <*> atKeyOptional "picture" (nullable text)
    <*> atKeyOptional "age" uint
    <*> atKeyOptional "latitude" double
    <*> atKeyOptional "longitude" double
    <*> atKeyOptional "tags" (vector text)
    <*> atKeyOptional "friends" (list decodeFriend)
    <*> atKeyOptional "doubles" (list (list double))
    <*> atKeyOptional "greeting" (nullable text)
    <*> atKeyOptional "favoriteFruit" text
    <*> atKeyOptional "employer" decodeEmployer
    <*> (fmap Map.fromList <$> atKeyOptional "mapOfInts"
          (objectAsKeyValues (pure . KeyType) int))
    <*> atKeyOptional "utcTimeField" utcTime

decodeFriend :: Decoder Friend
decodeFriend = object $
  Friend
    <$> atKey "id" int
    <*> atKey "name" text

genPerson :: Gen Person
genPerson = Person
  <$> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.int (Range.linear minBound maxBound)
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.bool
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.word (Range.linear minBound maxBound)
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
  <*> Gen.maybe (Gen.word (Range.linear 0 100))
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
decodeEmployer = object $
  Employer
    <$> atKey "inefficient" string
    <*> atKey "exp" scientific

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
genScientific = fmap Sci.fromFloatDigits genDouble

newtype Peano = Peano Word16
  deriving (Eq, Show)

instance A.ToJSON Peano where
  toJSON (Peano 0) = A.object []
  toJSON (Peano suc) = A.object [("suc", A.toJSON (Peano $ suc - 1))]

genPeano :: MonadGen m => m Peano
genPeano = Peano <$> Gen.word16 Range.linearBounded

decodePeano :: Decoder Peano
decodePeano = object $ do
  mPeano <- atKeyOptional "suc" decodePeano
  pure $
    case mPeano of
      Just (Peano subTree) -> Peano $ 1 + subTree
      Nothing -> Peano 0

data FriendWithMap =
  FriendWithMap
    { id   :: Int
    , rest :: Map Text Text
    } deriving (Eq, Show)

decodeFriendSlurp :: Decoder FriendWithMap
decodeFriendSlurp = object $
  FriendWithMap
    <$> atKey "id" int
    <*> liftObjectDecoder (objectAsMapExcluding ["id"] pure text)
