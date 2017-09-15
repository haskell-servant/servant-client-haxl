module Servant.Client.Haxl.Internal.Client where

import Data.Proxy (Proxy(..))
import Haxl.Core (GenHaxl)
import Servant.Client.Core (clientIn, Client, HasClient)

client :: forall u cli api .
  ( HasClient (GenHaxl u) api, Client (GenHaxl u) api ~ GenHaxl u cli
  ) => Proxy api -> GenHaxl u cli
client api = api `clientIn` (Proxy :: Proxy (GenHaxl u))
