{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Hermes.Decoder.Internal
  ( DecoderM(..)
  , DecoderPrimM(..)
  , HermesEnv(..)
  , HermesException(..)
  , DocumentError(..)
  , Path(..)
  , Decoder(..)
  , asks
  , local
  , decodeEither
  , decodeEitherIO
  , mkHermesEnv
  , mkHermesEnv_
  , withHermesEnv
  , withHermesEnv_
  , typePrefix
  , handleErrorCode
  , parseByteString
  , parseByteStringIO
  , liftIO
  , withRunInIO
  ) where

import           Control.Applicative (Alternative(..))
import           Control.DeepSeq (NFData(..))
import           Control.Exception (Exception, catch, throwIO, try)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Foreign.C as F
import qualified Foreign.ForeignPtr as F
import           GHC.Generics (Generic)
import qualified System.IO.Unsafe as Unsafe

import           Data.Hermes.SIMDJSON.Bindings (getDocumentValueImpl, getErrorMessageImpl)
import           Data.Hermes.SIMDJSON.Types
  ( Document(..)
  , Parser(..)
  , SIMDDocument
  , SIMDErrorCode(..)
  , SIMDParser
  , Value(..)
  )
import           Data.Hermes.SIMDJSON.Wrapper

-- | DecoderM is some context around the IO needed by the C FFI to allocate local memory.
-- Users have no access to the underlying IO, since this could allow decoders to launch nukes.
-- Using `Data.Hermes.decodeEither` discharges the IO and returns us to purity,
-- since we know decoding a document is referentially transparent.
newtype DecoderM a = DecoderM { runDecoderM :: ReaderT HermesEnv IO a }
  deriving newtype (Functor, Applicative, Monad)

instance Alternative DecoderM where
  empty = fail "Unspecified error"
  {-# INLINE (<|>) #-}
  ad <|> bd = withRunInIO $ \u -> u ad `catch` (\(_err :: HermesException) -> u bd)

instance MonadFail DecoderM where
  {-# INLINE fail #-}
  fail = throwHermes . T.pack

newtype DecoderPrimM a = DecoderPrimM { runDecoderPrimM :: DecoderM a }
  deriving newtype (Functor, Applicative, Monad)

instance PrimMonad DecoderPrimM where
  type PrimState DecoderPrimM = PrimState (ReaderT HermesEnv IO)
  primitive = DecoderPrimM . DecoderM . lift . primitive
  {-# INLINE primitive #-}

-- | Contains foreign references to the allocated simdjson::parser and
-- simdjson::document. Also maintains state for error reporting that is updated
-- when an object field or array value is entered.
data HermesEnv =
  HermesEnv
    { hParser   :: !(F.ForeignPtr SIMDParser)
    , hDocument :: !(F.ForeignPtr SIMDDocument)
    , hPath     :: ![Path]
    } deriving (Eq, Generic)

instance NFData HermesEnv where
  rnf HermesEnv{..} = rnf hPath `seq` ()

data Path =
    Key !Text
  | Idx !Int
  | Pointer !Text
  deriving (Eq, Show, Generic)

instance NFData Path

newtype Decoder a = Decoder { runDecoder :: Value -> DecoderM a }

instance Functor Decoder where
  {-# INLINE fmap #-}
  fmap f d = Decoder $ \val -> f <$> runDecoder d val

instance Applicative Decoder where
  {-# INLINE pure #-}
  pure a = Decoder $ \_ -> pure a
  {-# INLINE (<*>) #-}
  (Decoder f) <*> (Decoder e) = Decoder $ \val -> f val <*> e val

instance Monad Decoder where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  (Decoder d) >>= f = Decoder $ \val -> do
    x <- d val
    runDecoder (f x) val

instance Alternative Decoder where
  {-# INLINE (<|>) #-}
  (Decoder a) <|> (Decoder b) = Decoder $ \val -> a val <|> b val
  {-# INLINE empty #-}
  empty = Decoder $ const empty

instance MonadFail Decoder where
  {-# INLINE fail #-}
  fail e = Decoder $ \_ -> fail e

-- | Decode a strict `ByteString` using the simdjson::ondemand bindings.
-- Creates simdjson instances on each decode.
decodeEither :: Decoder a -> BS.ByteString -> Either HermesException a
decodeEither d bs = Unsafe.unsafeDupablePerformIO . try $ decodeEitherIO d bs
{-# NOINLINE decodeEither #-}

-- | Decode a strict `ByteString` using the simdjson::ondemand bindings.
-- Creates simdjson instances on each decode. Runs in IO instead of discharging it.
decodeEitherIO :: Decoder a -> BS.ByteString -> IO a
decodeEitherIO d bs = withHermesEnv_ $ \hEnv -> parseByteStringIO hEnv d bs

-- Given a HermesEnv, decode a strict ByteString.
parseByteString :: HermesEnv -> Decoder a -> BS.ByteString -> Either HermesException a
parseByteString hEnv d bs = Unsafe.unsafeDupablePerformIO . try $ parseByteStringIO hEnv d bs
{-# NOINLINE parseByteString #-}

-- Given a HermesEnv, decode a strict ByteString in IO.
parseByteStringIO :: HermesEnv -> Decoder a -> BS.ByteString -> IO a
parseByteStringIO hEnv d bs =
  allocaValue $ \valPtr ->
    withInputBuffer bs $ \inputPtr -> do
      F.withForeignPtr (hParser hEnv) $ \parserPtr ->
        F.withForeignPtr (hDocument hEnv) $ \docPtr -> do
          err <- getDocumentValueImpl (Parser parserPtr) inputPtr (Document docPtr) valPtr
          flip runReaderT hEnv . runDecoderM $ do
            handleErrorCode "" err
            runDecoder d valPtr

-- | Allocates foreign references to a simdjson::ondemand::parser and a
-- simdjson::ondemand::document. The optional capacity argument sets the max
-- capacity in bytes for the simdjson::ondemand::parser, which defaults to 4GB.
-- It is preferable to use `withHermesEnv` to keep foreign references in scope.
-- Be careful using this, the foreign references can be finalized if the
-- `HermesEnv` goes out of scope.
--
-- Do _not_ share a `HermesEnv` across multiple threads. Each thread should get its own.
mkHermesEnv :: Maybe Int -> IO HermesEnv
mkHermesEnv mCapacity = do
  parser   <- mkSIMDParser mCapacity
  document <- mkSIMDDocument
  pure HermesEnv
    { hParser   = parser
    , hDocument = document
    , hPath     = []
    }

mkHermesEnv_ :: IO HermesEnv
mkHermesEnv_ = mkHermesEnv Nothing

withHermesEnv :: Maybe Int -> (HermesEnv -> IO a) -> IO a
withHermesEnv mCapacity f = mkHermesEnv mCapacity >>= f

withHermesEnv_ :: (HermesEnv -> IO a) -> IO a
withHermesEnv_ = withHermesEnv Nothing

-- | The library can throw exceptions from simdjson in addition to its own exceptions.
data HermesException =
    SIMDException !DocumentError
    -- ^ An exception thrown from the simdjson library.
  | InternalException !DocumentError
    -- ^ An exception thrown from an internal library function.
  deriving stock (Eq, Show, Generic)

instance Exception HermesException
instance NFData HermesException

-- | Record containing all pertinent information for troubleshooting an exception.
data DocumentError =
  DocumentError
    { path     :: !Text
    -- ^ The path to the current element determined by the decoder.
    -- Formatted in the JSON Pointer standard per RFC 6901.
    , errorMsg :: !Text
    -- ^ An error message.
    }
    deriving stock (Eq, Show, Generic)

instance NFData DocumentError

typePrefix :: Text -> Text
typePrefix typ = "Error while getting value of type " <> typ <> ". "
{-# INLINE typePrefix #-}

-- | Re-throw an exception caught from the simdjson library.
throwSIMD :: Text -> DecoderM a
throwSIMD msg = do
  pth <- formatPath <$> asks hPath
  liftIO . throwIO . SIMDException $ DocumentError pth msg

-- | Throw an IO exception in the `Decoder` context.
throwHermes :: Text -> DecoderM a
throwHermes msg = do
  pth <- formatPath <$> asks hPath
  liftIO . throwIO . InternalException $ DocumentError pth msg

-- | Format path using JSON Pointer spec: https://www.rfc-editor.org/rfc/rfc6901
formatPath :: [Path] -> Text
formatPath dl =
  case els of
    [] -> ""
    xs -> T.concat $ fmap escapeKey xs
  where
    els                   = reverse dl
    escapeKey (Key txt)   = "/" <> T.concatMap escChar txt
    escapeKey (Idx int)   = "/" <> (T.pack . show $ int)
    escapeKey (Pointer p) = p
    escChar '/'           = "~1"
    escChar '~'           = "~0"
    escChar x             = T.singleton x

handleErrorCode :: Text -> F.CInt -> DecoderM ()
handleErrorCode pre errInt = do
  let errCode = toEnum $ fromIntegral errInt
  if errCode == SUCCESS
  then pure ()
  else do
    errStr <- liftIO $ F.peekCString =<< getErrorMessageImpl errInt
    throwSIMD $ pre <> T.pack errStr
{-# INLINE handleErrorCode #-}

withRunInIO :: ((forall a. DecoderM a -> IO a) -> IO b) -> DecoderM b
withRunInIO inner =
  DecoderM . ReaderT $ \r ->
    inner (flip runReaderT r . runDecoderM)
{-# INLINE withRunInIO #-}

liftIO :: IO a -> DecoderM a
liftIO = DecoderM . lift
{-# INLINE liftIO #-}

asks :: (HermesEnv -> a) -> DecoderM a
asks f = DecoderM . ReaderT $ pure . f
{-# INLINE asks #-}

local :: (HermesEnv -> HermesEnv) -> DecoderM a -> DecoderM a
local f (DecoderM m) = DecoderM . ReaderT $ runReaderT m . f
{-# INLINE local #-}
