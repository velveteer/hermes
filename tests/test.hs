{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.IO.Unsafe (unsafePerformIO)
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
    let encoded = BSL.toStrict . A.encode $ p
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
    } deriving (Eq, Show, Generic, A.ToJSON)

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

decodeFriend :: Value -> Decoder Friend
decodeFriend = withObject $ \obj ->
  Friend
    <$> atKey "id" int obj
    <*> atKey "name" text obj

genPerson :: Gen Person
genPerson = Person
  <$> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.int (Range.linear 0 1000000000)
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.bool
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.int (Range.linear 0 100)
  <*> Gen.double (Range.constant 0 1000)
  <*> Gen.double (Range.constant 0 1000)
  <*> Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.list (Range.linear 0 100) genFriend
  <*> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> Gen.text (Range.linear 0 100) Gen.unicode
  <*> genEmployer

genFriend :: Gen Friend
genFriend = Friend
  <$> Gen.int (Range.linear 0 100)
  <*> Gen.text (Range.linear 0 100) Gen.unicode

genEmployer :: Gen Employer
genEmployer = Employer
  <$> Gen.string (Range.linear 0 1000) Gen.unicode
  <*> Gen.realFrac_ (Range.constant 0 1000)

decodeEmployer :: Value -> Decoder Employer
decodeEmployer = withObject $ \obj ->
  Employer
    <$> atKey "inefficient" string obj
    <*> atKey "exp" scientific obj
