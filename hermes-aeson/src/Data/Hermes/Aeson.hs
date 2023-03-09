module Data.Hermes.Aeson where

import qualified Data.Hermes as H
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

hValueToAeson :: H.Decoder A.Value
hValueToAeson = do
  ty <- H.getType
  case ty of
    H.VArray   -> A.Array <$> H.vector hValueToAeson
    H.VObject  -> A.Object . KM.fromMap <$> H.objectAsMap (pure . K.fromText) hValueToAeson
    H.VNumber  -> A.Number <$> H.scientific
    H.VString  -> A.String <$> H.text
    H.VBoolean -> A.Bool <$> H.bool
    H.VNull    -> pure A.Null
