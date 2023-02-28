{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hermes.Decoder.Internal
  ( Decoder(..)
  , HermesEnv(..)
  , HermesException(..)
  , DocumentError(..)
  , withHermesEnv
  , allocaArray
  , allocaArrayIter
  , allocaObject
  , allocaObjectIter
  , allocaValue
  , typePrefix
  , handleErrorCode
  , withParserPointer
  , withDocumentPointer
  , liftIO
  , withRunInIO
  ) where

import           Control.Applicative (Alternative(..))
import           Control.DeepSeq (NFData)
import           Control.Exception (Exception, bracket, catch, throwIO)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Foreign.C as F
import qualified Foreign.ForeignPtr as F
import qualified Foreign.Marshal.Alloc as F
import qualified Foreign.Ptr as F
import           GHC.Generics (Generic)

import           Data.Hermes.SIMDJSON.Bindings (getErrorMessageImpl)
import           Data.Hermes.SIMDJSON.Types
  ( Array(..)
  , ArrayIter(..)
  , Document(..)
  , Object(..)
  , ObjectIter(..)
  , Parser(..)
  , SIMDDocument
  , SIMDErrorCode(..)
  , SIMDParser
  , Value(..)
  )
import           Data.Hermes.SIMDJSON.Wrapper

-- | A Decoder is some context around the IO needed by the C FFI to allocate local memory.
-- Users have no access to the underlying IO, since this could allow decoders to launch nukes.
-- Using `Data.Hermes.decodeEither` discharges the IO and returns us to purity,
-- since we know decoding a document is referentially transparent.
newtype Decoder a = Decoder { runDecoder :: ReaderT HermesEnv IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader HermesEnv
    )

instance Alternative Decoder where
  empty = fail "Unspecified error"
  ad <|> bd = withRunInIO $ \u -> u ad `catch` (\(_err :: HermesException) -> u bd)

instance MonadFail Decoder where
  {-# INLINE fail #-}
  fail = throwHermes . T.pack

-- | Contains foreign references to the allocated simdjson::parser
-- and simdjson::document. Also maintains a path string that is updated
-- when an object field or array value is entered and which is displayed in errors.
data HermesEnv =
  HermesEnv
    { hParser   :: !(F.ForeignPtr SIMDParser)
    , hDocument :: !(F.ForeignPtr SIMDDocument)
    , hPath     :: !Text
    }

-- | Make a new HermesEnv. This allocates foreign references to
-- a simdjson::ondemand::parser and a simdjson::ondemand::document.
-- The optional capacity argument sets the max capacity in bytes for the
-- simdjson::ondemand::parser, which defaults to 4GB.
mkHermesEnv :: Maybe Int -> IO HermesEnv
mkHermesEnv mCapacity = do
  parser   <- mkSIMDParser mCapacity
  document <- mkSIMDDocument
  pure HermesEnv
    { hParser   = parser
    , hDocument = document
    , hPath     = ""
    }

-- | Shortcut for constructing a default `HermesEnv`.
mkHermesEnv_ :: IO HermesEnv
mkHermesEnv_ = mkHermesEnv Nothing

-- | Internal finalizer for simdjson instances.
cleanupHermesEnv :: HermesEnv -> IO ()
cleanupHermesEnv hEnv = do
  F.finalizeForeignPtr (hDocument hEnv)
  F.finalizeForeignPtr (hParser hEnv)

-- | Run an action in IO that is passed a `HermesEnv`.
withHermesEnv :: (HermesEnv -> IO a) -> IO a
withHermesEnv = bracket acquire release
  where
    acquire = mkHermesEnv_
    release = cleanupHermesEnv

-- | The library can throw exceptions from simdjson in addition to
-- its own exceptions.
data HermesException =
    SIMDException DocumentError
    -- ^ An exception thrown from the simdjson library.
  | InternalException DocumentError
    -- ^ An exception thrown from an internal library function.
  deriving stock (Eq, Show, Generic)

instance Exception HermesException
instance NFData HermesException

-- | Record containing all pertinent information for troubleshooting an exception.
data DocumentError =
  DocumentError
    { path        :: !Text
    -- ^ The path to the current element determined by the decoder.
    -- Formatted in the JSON Pointer standard per RFC 6901.
    , errorMsg    :: !Text
    -- ^ An error message.
    , docLocation :: !Text
    -- ^ Truncated location of the simdjson document iterator.
    , docDebug    :: !Text
    -- ^ Debug information from simdjson::document.
    }
    deriving stock (Eq, Show, Generic)

instance NFData DocumentError

mkDocumentError :: Text -> Text -> Text -> Text -> DocumentError
mkDocumentError pth msg locStr debugStr = DocumentError pth msg (T.take 20 locStr) debugStr

typePrefix :: Text -> Text
typePrefix typ = "Error while getting value of type " <> typ <> ". "

-- | Re-throw an exception caught from the simdjson library.
throwSIMD :: SIMDErrorCode -> Text -> Decoder a
throwSIMD errCode msg = do
  pth <- asks hPath
  if errCode `elem`
    [ EMPTY
    , INSUFFICIENT_PADDING
    , SCALAR_DOCUMENT_AS_VALUE
    , UTF8_ERROR
    , UNCLOSED_STRING
    , UNESCAPED_CHARS
    ]
  then
    liftIO . throwIO . SIMDException $
      mkDocumentError pth msg "" ""
  else do
    withDocumentPointer $ \docPtr -> do
      (locTxt, debugTxt) <- liftIO $ getDocumentInfo docPtr
      liftIO . throwIO . SIMDException $
        mkDocumentError pth msg locTxt debugTxt

-- | Throw an IO exception in the `Decoder` context.
throwHermes :: Text -> Decoder a
throwHermes msg = do
  pth <- asks hPath
  liftIO . throwIO . InternalException $
    mkDocumentError pth msg "" ""

handleErrorCode :: Text -> F.CInt -> Decoder ()
handleErrorCode pre errInt = do
  let errCode = toEnum $ fromIntegral errInt
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- liftIO $ F.peekCString =<< getErrorMessageImpl errInt
    throwSIMD errCode $ pre <> T.pack errStr
{-# INLINE handleErrorCode #-}

withParserPointer :: (Parser -> Decoder a) -> Decoder a
withParserPointer f =
  asks hParser >>= \parserFPtr -> withRunInIO $ \u -> F.withForeignPtr parserFPtr $ u . f . Parser
{-# INLINE withParserPointer #-}

withDocumentPointer :: (Document -> Decoder a) -> Decoder a
withDocumentPointer f =
  asks hDocument >>= \docFPtr -> withRunInIO $ \u -> F.withForeignPtr docFPtr $ u . f . Document
{-# INLINE withDocumentPointer #-}

allocaValue :: (Value -> Decoder a) -> Decoder a
allocaValue f = allocaBytes 24 $ \val -> f (Value val)
{-# INLINE allocaValue #-}

allocaObject :: (Object -> Decoder a) -> Decoder a
allocaObject f = allocaBytes 24 $ \objPtr -> f (Object objPtr)
{-# INLINE allocaObject #-}

allocaArray :: (Array -> Decoder a) -> Decoder a
allocaArray f = allocaBytes 24 $ \arr -> f (Array arr)
{-# INLINE allocaArray #-}

allocaArrayIter :: (ArrayIter -> Decoder a) -> Decoder a
allocaArrayIter f = allocaBytes 24 $ \iter -> f (ArrayIter iter)
{-# INLINE allocaArrayIter #-}

allocaObjectIter :: (ObjectIter -> Decoder a) -> Decoder a
allocaObjectIter f = allocaBytes 24 $ \iter -> f (ObjectIter iter)
{-# INLINE allocaObjectIter #-}

allocaBytes :: Int -> (F.Ptr a -> Decoder b) -> Decoder b
allocaBytes size action = withRunInIO (\u -> F.allocaBytes size (u . action))
{-# INLINE allocaBytes #-}

withRunInIO :: ((forall a. Decoder a -> IO a) -> IO b) -> Decoder b
withRunInIO inner =
  Decoder . ReaderT $ \r ->
    inner (flip runReaderT r . runDecoder)
{-# INLINE withRunInIO #-}

liftIO :: IO a -> Decoder a
liftIO = Decoder . lift
{-# INLINE liftIO #-}
