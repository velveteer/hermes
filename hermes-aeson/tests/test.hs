{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Decoding as A.D
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Hermes as H
import Data.Int (Int64)
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.IO.Unsafe as Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import qualified Data.Hermes.Aeson as H

-- Shared parser/document across all property tests to avoid 500
-- fresh simdjson allocations.
sharedEnv :: H.HermesEnv
sharedEnv = Unsafe.unsafePerformIO H.mkHermesEnv_
{-# NOINLINE sharedEnv #-}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ aesonValueMatches
    , errorIncludesLocation
    , agreesWithAeson
    , fuzzNoUnexpectedException
    , fuzzDecoderNoUnexpectedException
    ]

aesonValueMatches :: TestTree
aesonValueMatches =
  withResource (BS.readFile "./tests/twitter100.json") (const $ pure ()) $ \twitter ->
    testCase "aesonValue matches Aeson's decoder" $ do
      Right hv <- H.decodeEither H.aesonValue <$> twitter
      Right ha <- A.D.eitherDecodeStrict @A.Value <$> twitter
      hv @?= ha

-- An invalid escape passes top-level parse but fails inside
-- get_string(), so the error fires from the bulk walker. We put the
-- bad escape in the middle of a longer document so the 32-byte
-- snippet recorded at simdjson's current_location actually contains
-- meaningful source bytes (rather than padded_string null padding).
errorIncludesLocation :: TestTree
errorIncludesLocation =
  testCase "Error includes source snippet at the failing location" $ do
    let badJson = BSC.pack "[1, 2, 3, 4, 5, 6, \"\\x\", 7, 8, 9, 10]"
    case H.decodeEither H.aesonValue badJson of
      Right v -> assertFailure ("expected an error, got: " <> show v)
      Left e -> do
        let msg = T.pack (show e)
        assertBool
          ("error should be prefixed with 'near `': " <> show msg)
          ("near `" `T.isInfixOf` msg)
        -- simdjson's current_location for STRING_ERROR lands just
        -- past the offending string, so the snippet should contain
        -- the source bytes that follow it.
        assertBool
          ("snippet should contain source bytes after the bad escape: " <> show msg)
          (", 7, 8, 9, 10]" `T.isInfixOf` msg)
        assertBool
          ("error message should mention STRING_ERROR: " <> show msg)
          ("STRING_ERROR" `T.isInfixOf` msg)

-- Generated Aeson Value round-trips through encoding identically via
-- both Aeson's own decoder and 'parseAesonValue'. Covers nested
-- containers, all leaf types, and the integer / big-integer /
-- floating-point number paths.
agreesWithAeson :: TestTree
agreesWithAeson = testProperty "Agrees with Aeson on generated Values" $
  withTests 500 $
    property $ do
      val <- forAll genValue
      let bytes = BSL.toStrict (A.encode val)
      let hermes = H.parseAesonValue sharedEnv bytes
      let aeson = A.D.eitherDecodeStrict @A.Value bytes
      case (hermes, aeson) of
        (Right h, Right a) -> h === a
        (Left eh, Right _) -> annotate ("hermes failed: " <> show eh) >> failure
        (Right _, Left ea) -> annotate ("aeson failed: " <> ea) >> failure
        (Left eh, Left ea) ->
          annotate
            ( "both failed: hermes="
                <> show eh
                <> ", aeson="
                <> ea
            )
            >> failure

genValue :: Gen A.Value
genValue =
  Gen.recursive
    Gen.choice
    -- Leaves
    [ pure A.Null
    , A.Bool <$> Gen.bool
    , A.Number <$> genScientific
    , A.String <$> genText
    ]
    -- Containers (depth shrinks automatically)
    [ A.Array . V.fromList <$> Gen.list (Range.linear 0 5) genValue
    , A.Object . KM.fromList <$> Gen.list (Range.linear 0 5) genKV
    ]
  where
    genKV = (,) . K.fromText <$> genText <*> genValue

genScientific :: Gen Sci.Scientific
genScientific =
  Gen.choice
    [ -- Int64-range integers exercise the int fast path.
      flip Sci.scientific 0 . toInteger
        <$> Gen.integral @_ @Int64 Range.constantBounded
    , -- Integers outside Int64 exercise the big_integer / raw-token path.
      flip Sci.scientific 0
        <$> Gen.integral (Range.linearFrom 0 ((-1) * 10 ^ (30 :: Int)) (10 ^ (30 :: Int)))
    , -- General Scientific with non-zero exponent exercises the
      -- floating-point / raw-token path.
      Sci.scientific
        <$> Gen.integral (Range.linearFrom 0 (-1000000) 1000000)
        <*> Gen.int (Range.linearFrom 0 (-30) 30)
    ]

genText :: Gen T.Text
genText = Gen.text (Range.linear 0 16) Gen.unicode

-- Random bytes must produce either a successful 'Value' or a typed
-- 'HermesException'. Anything else (pattern-match failure, IOError,
-- segfault) is a bug. Pure errors and uncaught exceptions would
-- crash a real program using this as a decoder. The 'cover'
-- assertion guards against a regression where the decoder silently
-- accepts garbage as a valid Value.
fuzzNoUnexpectedException :: TestTree
fuzzNoUnexpectedException =
  testProperty "parseAesonValue: random bytes never produce unexpected exceptions" $
    withTests 1000 $
      property $ do
        bytes <- forAll (Gen.bytes (Range.linear 0 256))
        result <-
          evalIO . try @SomeException $
            evaluate (force (H.parseAesonValue sharedEnv bytes))
        cover 90 "rejected as HermesException" $ case result of
          Right (Left (_ :: H.HermesException)) -> True
          _ -> False
        case result of
          Right (Right _) -> success
          Right (Left (_ :: H.HermesException)) -> success
          Left ex -> annotate ("unexpected: " <> show ex) >> failure

-- Same fuzz coverage for the 'Decoder'-based path. 'decodeEither'
-- only catches 'HermesException', so any other exception escaping
-- would indicate a bug in the value-level tape interpreter.
fuzzDecoderNoUnexpectedException :: TestTree
fuzzDecoderNoUnexpectedException =
  testProperty "decodeEither aesonValue: random bytes never produce unexpected exceptions" $
    withTests 1000 $
      property $ do
        bytes <- forAll (Gen.bytes (Range.linear 0 256))
        result <-
          evalIO . try @SomeException $
            evaluate (force (H.decodeEither H.aesonValue bytes))
        cover 90 "rejected as HermesException" $ case result of
          Right (Left (_ :: H.HermesException)) -> True
          _ -> False
        case result of
          Right (Right _) -> success
          Right (Left (_ :: H.HermesException)) -> success
          Left ex -> annotate ("unexpected: " <> show ex) >> failure
