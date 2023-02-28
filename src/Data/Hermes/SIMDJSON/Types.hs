module Data.Hermes.SIMDJSON.Types
  ( -- * simdjson Opaque Types
    Array(..)
  , ArrayIter(..)
  , Document(..)
  , InputBuffer(..)
  , JSONArray
  , JSONArrayIter
  , JSONObject
  , JSONObjectIter
  , JSONValue
  , Object(..)
  , ObjectIter(..)
  , PaddedString
  , Parser(..)
  , SIMDDocument
  , SIMDErrorCode(..)
  , SIMDParser
  , Value(..)
  )
  where

import           Foreign.Ptr (Ptr)

-- | A reference to an opaque simdjson::ondemand::parser.
newtype Parser = Parser (Ptr SIMDParser)

-- | A reference to an opaque simdjson::ondemand::document.
newtype Document = Document (Ptr SIMDDocument)

-- | A reference to an opaque simdjson::padded_string.
newtype InputBuffer = InputBuffer (Ptr PaddedString)

-- | A reference to an opaque simdjson::ondemand::value.
newtype Value = Value (Ptr JSONValue)

-- | A reference to an opaque simdjson::ondemand::object.
newtype Object = Object (Ptr JSONObject)

-- | A reference to an opaque simdjson::ondemand::array.
newtype Array = Array (Ptr JSONArray)

-- | A reference to an opaque simdjson::ondemand::array_iterator.
newtype ArrayIter = ArrayIter (Ptr JSONArrayIter)

-- | A reference to an opaque simdjson::ondemand::object_iterator.
newtype ObjectIter = ObjectIter (Ptr JSONObjectIter)

-- | Phantom type for a pointer to simdjson::ondemand::parser.
data SIMDParser

-- | Phantom type for a pointer to simdjson::ondemand::document.
data SIMDDocument

-- | Phantom type for a pointer to simdjson::padded_string.
data PaddedString

-- | Phantom type for a pointer to simdjson::ondemand::value.
data JSONValue

-- | Phantom type for a pointer to simdjson::ondemand::object.
data JSONObject

-- | Phantom type for a pointer to simdjson::ondemand::array.
data JSONArray

-- | Phantom type for a pointer to simdjson::ondemand::array_iterator
data JSONArrayIter

-- | Phantom type for a pointer to simdjson::ondemand::object_iterator
data JSONObjectIter

-- | Enum for simdjson errors.
data SIMDErrorCode =
    SUCCESS
  | CAPACITY
  | MEMALLOC
  | TAPE_ERROR
  | DEPTH_ERROR
  | STRING_ERROR
  | T_ATOM_ERROR
  | F_ATOM_ERROR
  | N_ATOM_ERROR
  | NUMBER_ERROR
  | UTF8_ERROR
  | UNINITIALIZED
  | EMPTY
  | UNESCAPED_CHARS
  | UNCLOSED_STRING
  | UNSUPPORTED_ARCHITECTURE
  | INCORRECT_TYPE
  | NUMBER_OUT_OF_RANGE
  | INDEX_OUT_OF_BOUNDS
  | NO_SUCH_FIELD
  | IO_ERROR
  | INVALID_JSON_POINTER
  | INVALID_URI_FRAGMENT
  | UNEXPECTED_ERROR
  | PARSER_IN_USE
  | OUT_OF_ORDER_ITERATION
  | INSUFFICIENT_PADDING
  | INCOMPLETE_ARRAY_OR_OBJECT
  | SCALAR_DOCUMENT_AS_VALUE
  | OUT_OF_BOUNDS
  | NUM_ERROR_CODES
  deriving (Eq, Show, Bounded, Enum)
