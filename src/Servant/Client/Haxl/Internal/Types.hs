{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Client.Haxl.Internal.Types where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.QSem  (QSem, waitQSem, signalQSem, newQSem)
import Control.Exception        (SomeException, bracket_, try)
{-import Control.Monad.Catch      (MonadCatch, MonadThrow)-}
{-import Control.Monad.Except-}
import Data.Foldable            (toList)
import Data.Hashable            (Hashable (..))
import Data.Monoid
{-import GHC.Generics             (Generic)-}
import Network.HTTP.Client      (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Media       (MediaType, renderHeader)
import Network.HTTP.Types       (HttpVersion (..))
import Servant.Client.Core      (BaseUrl, RequestBody (..), RequestF (..),
                                 Response, RunClient (..), ServantError(..))

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as BSL
import qualified Haxl.Core               as Haxl
import qualified Servant.Client          as HttpClient

data ServantHaxlRequest a where
  ServantHaxlRequest :: RequestF BSL.ByteString
                     -> ServantHaxlRequest (Either ServantError Response)

deriving instance Eq (ServantHaxlRequest a)
deriving instance Show (ServantHaxlRequest a)

{-newtype ClientM a-}
  {-= ClientM { runClientM' :: ExceptT ServantError (Haxl.GenHaxl ()) a }-}
  {-deriving ( Functor, Applicative, Monad, Generic-}
           {-, MonadThrow, MonadCatch, MonadError ServantError)-}

instance Haxl.DataSourceName ServantHaxlRequest where
  dataSourceName _ = "ServantHaxlRequest"

instance Haxl.ShowP ServantHaxlRequest where
  showp x = show x

instance Hashable (ServantHaxlRequest a) where
  hashWithSalt s (ServantHaxlRequest (Request a b c d e f g))
    = s
    % a                           -- requestPath        :: BSL.ByteString
    % toList b                    -- requestQueryString :: Seq.Seq QueryItem
    % fmap hashBody c             -- requestBody        :: Maybe (RequestBody, MediaType)
    % fmap renderHeader toList d  -- requestAccept      :: Seq.Seq MediaType
    % toList e                    -- requestHeaders     :: Seq.Seq Header
    % httpMajor f % httpMinor f   -- requestHttpVersion :: HttpVersion
    % g                           -- requestMethod      :: Method
    where
      infixl %
      x % y = x `hashWithSalt` y
      hashBody :: (RequestBody, MediaType) -> BSL.ByteString
      hashBody (RequestBodyLBS x, m) = x <> BSL.fromStrict (renderHeader m)

instance Haxl.StateKey ServantHaxlRequest where
  data State ServantHaxlRequest = ServantHaxlRequestState
      { numThreads :: Int
      , manager    :: Manager
      , baseUrl    :: BaseUrl
      }

initDataSource :: Int -> BaseUrl -> IO (Haxl.State ServantHaxlRequest)
initDataSource threads baseUrl'
   = ServantHaxlRequestState
 <$> pure threads
 <*> newManager defaultManagerSettings
 <*> pure baseUrl'


instance Haxl.DataSource u ServantHaxlRequest where
  fetch requestState _flags _ requests = Haxl.AsyncFetch $ \inner -> do
    sem <- newQSem (numThreads requestState)
    asyncs <- mapM (go sem) requests
    inner
    mapM_ wait asyncs
    where
      go :: QSem -> Haxl.BlockedFetch ServantHaxlRequest -> IO (Async ())
      go sem (Haxl.BlockedFetch (ServantHaxlRequest request) rvar) = async $ do
        bracket_ (waitQSem sem) (signalQSem sem) $ do
          response <- try $ HttpClient.runClientM
            (runRequest $ fmap Builder.lazyByteString request)
            (HttpClient.ClientEnv (manager requestState)
                                  (baseUrl requestState))
          case response of
            Left  e -> Haxl.putFailure rvar (e :: SomeException)
            Right r -> Haxl.putSuccess rvar r

instance RunClient (Haxl.GenHaxl u) where
  runRequest request = do
    eresp <- Haxl.dataFetch . ServantHaxlRequest $ Builder.toLazyByteString <$> request
    case eresp of
      Left e -> Haxl.throw e
      Right v -> return v

{-instance RunClient ClientM where-}
  {-runRequest request = ClientM . ExceptT . Haxl.dataFetch . ServantHaxlRequest-}
    {-$ Builder.toLazyByteString <$> request-}
