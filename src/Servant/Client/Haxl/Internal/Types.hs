{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Servant.Client.Haxl.Internal.Types where

import Control.Monad.Catch  (MonadCatch, MonadThrow)
import Control.Monad.Except
import Data.Foldable        (toList)
import Data.Hashable        (Hashable (..))
import Data.Monoid
import GHC.Generics         (Generic)
import Network.HTTP.Client  (Manager)
import Network.HTTP.Media   (MediaType, renderHeader)
import Network.HTTP.Types   (HttpVersion (..))
import Servant.Client.Core  (BaseUrl, RequestBody (..), RequestF (..),
                             Response, RunClient (..), ServantError)

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as BSL
import qualified Haxl.Core               as Haxl
import qualified Servant.Client          as HttpClient

data ServantHaxlRequest a where
  ServantHaxlRequest :: RequestF BSL.ByteString
                     -> ServantHaxlRequest (Either ServantError Response)

deriving instance Eq (ServantHaxlRequest a)
deriving instance Show (ServantHaxlRequest a)

newtype ClientM a
  = ClientM { runClientM' :: ExceptT ServantError (Haxl.GenHaxl ()) a }
  deriving ( Functor, Applicative, Monad, Generic
           , MonadThrow, MonadCatch, MonadError ServantError)

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
      { shrsNumThreads :: Int
      , shrsManager    :: Manager
      , shrsBaseUrl    :: BaseUrl
      } -- deriving (Eq, Show, Generic, Hashable)

instance Haxl.DataSource u ServantHaxlRequest where
  fetch requestState flags _ requests
    | shrsNumThreads requestState == 1 = Haxl.SyncFetch $ mapM_ go requests
      {-runRequest (shrsManager requestState)-}
    {-| otherwise                        = Haxl.AsyncFetch $ _-}
      {-runRequest (shrsManager requestState)-}
    where
      go :: Haxl.BlockedFetch ServantHaxlRequest -> IO ()
      go (Haxl.BlockedFetch (ServantHaxlRequest request) rvar) = do
        response <- HttpClient.runClientM
          (runRequest $ fmap Builder.lazyByteString request)
          (HttpClient.ClientEnv (shrsManager requestState)
                                (shrsBaseUrl requestState))
        Haxl.putSuccess rvar response


instance RunClient ClientM where
  runRequest request = ClientM . ExceptT . Haxl.dataFetch . ServantHaxlRequest
    $ Builder.toLazyByteString <$> request

