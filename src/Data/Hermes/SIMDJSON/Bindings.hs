-- | Contains Haskell bindings to C++ wrappers around simdjson methods.

module Data.Hermes.SIMDJSON.Bindings
  ( arrayIterGetCurrentImpl
  , arrayIterIsDoneImpl
  , arrayIterMoveNextImpl
  , atPointerImpl
  , deleteDocumentImpl
  , deleteInputImpl
  , doubleArrayImpl
  , findFieldImpl
  , findFieldUnorderedImpl
  , getArrayFromValueImpl
  , getArrayIterFromValueImpl
  , getArrayIterLenFromValueImpl
  , getArrayLenFromValueImpl
  , getBoolImpl
  , getDocumentValueImpl
  , getDoubleImpl
  , getErrorMessageImpl
  , getIntImpl
  , getObjectFromValueImpl
  , getObjectIterFromValueImpl
  , getRawJSONTokenImpl
  , getStringImpl
  , getTypeImpl
  , intArrayImpl
  , isNullImpl
  , makeDocumentImpl
  , makeInputImpl
  , objectIterGetCurrentImpl
  , objectIterIsDoneImpl
  , objectIterMoveNextImpl
  , parserInit
  , parserDestroy
  , resetArrayImpl
  , resetObjectImpl
  ) where

import           Foreign.C (CBool(..), CInt(..), CSize(..), CString)
import           Foreign.Ptr (FunPtr, Ptr)

import           Data.Hermes.SIMDJSON.Types

-- Constructor/destructors
foreign import ccall unsafe "parser_init" parserInit
  :: CSize -> IO (Ptr SIMDParser)

foreign import ccall unsafe "&parser_destroy" parserDestroy
  :: FunPtr (Ptr SIMDParser -> IO ())

foreign import ccall unsafe "make_document" makeDocumentImpl
  :: IO (Ptr SIMDDocument)

foreign import ccall unsafe "&delete_document" deleteDocumentImpl
  :: FunPtr (Ptr SIMDDocument -> IO ())

foreign import ccall unsafe "make_input" makeInputImpl
  :: CString -> CSize -> IO (Ptr PaddedString)

foreign import ccall unsafe "&delete_input" deleteInputImpl
  :: FunPtr (Ptr PaddedString -> IO ())

-- Document parsers
foreign import ccall unsafe "get_document_value" getDocumentValueImpl
  :: Parser -> InputBuffer -> Document -> Value -> IO CInt

foreign import ccall unsafe "at_pointer" atPointerImpl
  :: CString -> Int -> Document -> Value -> IO CInt

foreign import ccall unsafe "get_object_from_value" getObjectFromValueImpl
  :: Value -> IO CInt

foreign import ccall unsafe "get_object_iter_from_value" getObjectIterFromValueImpl
  :: Value -> IO CInt

foreign import ccall unsafe "obj_iter_is_done" objectIterIsDoneImpl
  :: ObjectIter -> IO CBool

foreign import ccall unsafe "obj_iter_get_current" objectIterGetCurrentImpl
  :: ObjectIter -> Ptr CString -> Ptr CSize -> Value -> IO CInt

foreign import ccall unsafe "obj_iter_move_next" objectIterMoveNextImpl
  :: ObjectIter -> IO ()

foreign import ccall unsafe "get_array_from_value" getArrayFromValueImpl
  :: Value -> Array -> IO CInt

foreign import ccall unsafe "get_array_len_from_value" getArrayLenFromValueImpl
  :: Value -> Ptr CSize -> IO CInt

foreign import ccall unsafe "int_array" intArrayImpl
  :: Array -> Ptr Int -> IO CInt

foreign import ccall unsafe "double_array" doubleArrayImpl
  :: Array -> Ptr Double -> IO CInt

foreign import ccall unsafe "get_array_iter_from_value" getArrayIterFromValueImpl
  :: Value -> IO CInt

foreign import ccall unsafe "get_array_iter_len_from_value" getArrayIterLenFromValueImpl
  :: Value -> Ptr CSize -> IO CInt

foreign import ccall unsafe "arr_iter_is_done" arrayIterIsDoneImpl
  :: ArrayIter -> IO CBool

foreign import ccall unsafe "arr_iter_get_current" arrayIterGetCurrentImpl
  :: ArrayIter -> Value -> IO CInt

foreign import ccall unsafe "arr_iter_move_next" arrayIterMoveNextImpl
  :: ArrayIter -> IO ()

foreign import ccall unsafe "reset_array" resetArrayImpl
  :: Array -> IO ()

foreign import ccall unsafe "reset_object" resetObjectImpl
  :: Object -> IO ()

foreign import ccall unsafe "find_field_unordered" findFieldUnorderedImpl
  :: Object -> CString -> Int -> Value -> IO CInt

foreign import ccall unsafe "find_field" findFieldImpl
  :: Object -> CString -> Int -> Value -> IO CInt

-- Helpers
foreign import ccall unsafe "get_error_message" getErrorMessageImpl
  :: CInt -> IO CString

-- Primitives
foreign import ccall unsafe "get_int" getIntImpl
  :: Value -> Ptr Int -> IO CInt

foreign import ccall unsafe "get_double" getDoubleImpl
  :: Value -> Ptr Double -> IO CInt

foreign import ccall unsafe "get_string" getStringImpl
  :: Value -> Ptr CString -> Ptr CSize -> IO CInt

foreign import ccall unsafe "get_bool" getBoolImpl
  :: Value -> Ptr CBool -> IO CInt

foreign import ccall unsafe "get_raw_json_token" getRawJSONTokenImpl
  :: Value -> Ptr CString -> Ptr CSize -> IO ()

foreign import ccall unsafe "get_type" getTypeImpl
  :: Value -> Ptr CInt -> IO CInt

foreign import ccall unsafe "is_null" isNullImpl
  :: Value -> Ptr CBool -> IO CInt
