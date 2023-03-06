module Data.Hermes.Aeson where

import qualified Data.Hermes as H
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

hValueToAeson :: H.Value -> H.Decoder A.Value
hValueToAeson v = do
  ty <- H.getType v
  case ty of
    H.VArray   -> A.Array <$> H.vector hValueToAeson v
    H.VObject  -> A.Object . KM.fromMap <$> H.objectAsMap (pure . K.fromText) hValueToAeson v
    H.VNumber  -> A.Number <$> H.scientific v
    H.VString  -> A.String <$> H.text v
    H.VBoolean -> A.Bool <$> H.bool v
    H.VNull    -> pure A.Null
