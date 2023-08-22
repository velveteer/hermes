-- | Exposes functions for building JSON decoders that harness the power
-- of the simdjson::ondemand API.
--
-- A decoder is really a function from a simdjson `Value` to some Haskell type in the `DecoderM` monad.
-- It looks like
-- [Data.Aeson.parseJSON](https://hackage.haskell.org/package/aeson-2.0.2.0/docs/Data-Aeson.html#v:parseJSON),
-- except the `Value` is opaque and can only be used when it's passed by
-- reference across the C FFI.
--
-- `decodeEither` provides the quickest way to feed the initial `Value` to your decoder.
-- It does this by obtaining a top-level `Value` from the simdjson document
-- instance. Decoding a document into a scalar from a `Value` is not supported
-- by simdjson. While simdjson can cast a document directly to a scalar, this
-- library currently exposes no interface for this.

module Data.Hermes
  ( -- * Decoding from ByteString input
    decodeEither
  , decodeEitherIO
  , parseByteString
  , parseByteStringIO
  , Decoder(runDecoder)
  , FieldsDecoder(runFieldsDecoder)
    -- * Decoder monad
  , DecoderM(runDecoderM)
  , HermesEnv(hPath, hDocument, hParser)
  , mkHermesEnv
  , mkHermesEnv_
  , withHermesEnv
  , withHermesEnv_
    -- * Object field decoders
  , atKey
  , atKeyOptional
  , atKeyStrict
    -- * Decoders
    -- ** JSON pointer
  , atPointer
    -- ** Values
  , bool
  , char
  , double
  , int
  , uint
  , object
  , scientific
  , string
  , text
  , list
  , listOfInt
  , listOfDouble
  , vector
  , nullable
  , objectAsKeyValues
  , objectAsMap
    -- ** Date and time
    -- | Parses date and time types from Data.Time using the
    -- same Text parsers as Data.Aeson via
    -- <https://hackage.haskell.org/package/text-iso8601>.
  , day
  , month
  , quarter
  , timeOfDay
  , timeZone
  , localTime
  , utcTime
  , zonedTime
    -- * Error Types
  , HermesException(..)
  , DocumentError(..)
    -- * Value helpers
  , getType
  , isNull
  , withBool
  , withDouble
  , withInt
  , withObjectAsMap
  , withScientific
  , withString
  , withText
  , withType
  , withVector
  -- * Raw ByteString access
  , withRawByteString
    -- * simdjson Opaque Types
  , Array
  , ArrayIter
  , Document
  , InputBuffer
  , Object
  , Parser
  , Value
  , ValueType(..)
  ) where

import           Data.Hermes.Decoder
import           Data.Hermes.Decoder.Internal
  ( DecoderM(runDecoderM)
  , Decoder(runDecoder)
  , FieldsDecoder(runFieldsDecoder)
  , DocumentError(..)
  , HermesEnv(hDocument, hParser, hPath)
  , HermesException(..)
  , decodeEither
  , decodeEitherIO
  , mkHermesEnv
  , mkHermesEnv_
  , parseByteString
  , parseByteStringIO
  , withHermesEnv
  , withHermesEnv_
  )
import           Data.Hermes.SIMDJSON.Types
