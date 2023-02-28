-- | Exposes functions for building JSON decoders that harness the power
-- of the simdjson::ondemand API.
--
-- A decoder is really a function from a `Value` to some Haskell type in the `Decoder` monad.
-- It looks like [Data.Aeson.parseJSON](https://hackage.haskell.org/package/aeson-2.0.2.0/docs/Data-Aeson.html#v:parseJSON), except the `Value` is opaque and can only be used
-- when it's passed by reference across the C FFI.
--
-- `decodeEither` provides the quickest way to feed the initial `Value` to your decoder.
-- It does this by obtaining a top-level `Value` from the simdjson document
-- instance. Decoding a document into a scalar from a `Value` is not supported
-- by simdjson. While simdjson can cast a document directly to a scalar, this
-- library currently exposes no interface for this.

module Data.Hermes
  ( -- * Decoding from ByteString input
    decodeEither
    -- * Decoder monad
  , Decoder(runDecoder)
  , HermesEnv
    -- * Object field accessors
    -- | Obtain an object using `withObject` that can be passed
    -- to these field lookup functions.
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
  , scientific
  , string
  , text
  , list
  , nullable
  , objectAsKeyValues
    -- ** Date and time
    -- | Parses date and time types from Data.Time using the
    -- same attoparsec parsers as Data.Aeson via
    -- <https://hackage.haskell.org/package/attoparsec-iso8601>.
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
  , isNull
  , withArray
  , withBool
  , withDocumentValue
  , withDouble
  , withInt
  , withObject
  , withString
  , withText
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
  ) where

import           Control.Exception (try)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import           Data.ByteString (ByteString)
import qualified System.IO.Unsafe as Unsafe

import           Data.Hermes.Decoder
import           Data.Hermes.Decoder.Internal
  ( Decoder(..)
  , DocumentError(..)
  , HermesEnv
  , HermesException(..)
  , withHermesEnv
  )
import           Data.Hermes.SIMDJSON.Types
import           Data.Hermes.SIMDJSON.Wrapper (withInputBuffer)

-- | Decode a strict `ByteString` using the simdjson::ondemand bindings.
decodeEither :: (Value -> Decoder a) -> ByteString -> Either HermesException a
decodeEither d bs =
  Unsafe.unsafePerformIO . try .  withHermesEnv $ \hEnv ->
    withInputBuffer bs $ \input ->
      flip runReaderT hEnv . runDecoder $ withDocumentValue d input
