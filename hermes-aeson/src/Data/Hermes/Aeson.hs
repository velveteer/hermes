module Data.Hermes.Aeson where

import qualified Data.Hermes as H
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

hValueToAeson :: H.Decoder A.Value
hValueToAeson = H.withType $ \ty ->
  case ty of
    H.VArray   -> H.withVector hValueToAeson (pure . A.Array)
    H.VObject  -> H.withObjectAsMap (pure . K.fromText) hValueToAeson (pure . A.Object . KM.fromMap)
    H.VNumber  -> H.withScientific (pure . A.Number)
    H.VString  -> H.withText (pure . A.String)
    H.VBoolean -> H.withBool (pure . A.Bool)
    H.VNull    -> H.withNull (\isNil -> if isNil then pure A.Null else fail "expected null")
