{-# LANGUAGE DeriveAnyClass #-}
module Servant.Client.HaxlSpec (spec) where

import Data.Aeson
import Data.Proxy               (Proxy (..))
import GHC.Generics             (Generic)
import Haxl.Core                (GenHaxl, initEnv, stateEmpty, stateSet, runHaxl)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.API
import Servant.Server
import Test.Hspec

import Servant.Client.Haxl

spec :: Spec
spec = do
  simpleGetSpec

simpleGetSpec :: Spec
simpleGetSpec = describe "simple GET requests" $

  it "should succeed" $ do
    person <- withServer getPerson
    person `shouldBe` methuselah


-- * API

data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


type API = "get" :> Get '[JSON] Person

api :: Proxy API
api = Proxy

methuselah :: Person
methuselah = Person "Methuselah" 939

-- * Server

withServer :: GenHaxl () a -> IO a
withServer action = testWithApplication (return $ serve api server) $ \port -> do
  ds <- initDataSource 10 (BaseUrl Http "localhost" port "")
  env <- initEnv (stateSet ds stateEmpty) ()
  runHaxl env action

server :: Server API
server = return methuselah

-- * Client

getPerson :: GenHaxl u Person
getPerson = client api
