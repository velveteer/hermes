{-# LANGUAGE TypeApplications #-}

import qualified Data.Aeson as A
import qualified Data.Aeson.Decoding as A.D
import qualified Data.ByteString as BS
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Hermes as H
import qualified Data.Hermes.Aeson as H

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [aesonEq]

aesonEq :: TestTree
aesonEq =
  withResource (BS.readFile "./tests/twitter100.json") (const $ pure ()) $ \twitter ->
    testCase "Matches Aeson Value" $ do
      Right hv <- H.decodeEither H.hValueToAeson <$> twitter
      Right ha <- A.D.eitherDecodeStrict @A.Value <$> twitter
      hv @?= ha
