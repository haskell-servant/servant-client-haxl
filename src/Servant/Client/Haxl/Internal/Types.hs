module Servant.Client.Haxl.Internal.Types where

import Data.Hashable          (Hashable)
import GHC.Generics           (Generic)
import Network.HTTP.Client    (Manager)
import Network.HTTP.Types     (Method)
import Servant.Common.BaseUrl (BaseUrl)
import Servant.Common.Req     (Req)

import qualified Haxl.Core as Haxl

data ServantHaxlRequest a
  = ServantHaxlRequest
  { shrMethod :: Method
  , shrReq    :: Req
  {-, shrWanted :: WantedStatusCode-}
  , shrUrl    :: BaseUrl
  } deriving (Eq, Show, Generic, Hashable)

instance Haxl.DataSourceName ServantHaxlRequest where
  dataSourceName _ = "ServantHaxlRequest"


instance Haxl.StateKey ServantHaxlRequest where
  data State ServantHaxlRequest = ServantHaxlRequestState
      { shrsNumThreads :: Int
      , shrsManager    :: Manager
      } -- deriving (Eq, Show, Generic, Hashable)

instance Haxl.DataSource u ServantHaxlRequest where
  fetch requestState flags _ requests
    | shrsNumThreads requestState == 1 = Haxl.SyncFetch $ do
      runRequest (shrsManager requestState)
    | otherwise                        = Haxl.AsyncFetch $ do
      runRequest (shrsManager requestState)
    where
      go :: Haxl.BlockedFetch ServantHaxlRequest -> IO ()
      go (Haxl.BlockedFetch shr rvar) = do
        runRequest proxy (shrMethod shr)


{-instance (MimeUnrender a)-}
  {-=> RunClient (GenHaxl ()) ct ([HTTP.Header, a) where-}

