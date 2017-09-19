{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Utils.StaticFilesSpec where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(Proxy))
import Snap.Core (route)
import Snap.Snaplet (Handler, SnapletInit, makeSnaplet)
import System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, around_, shouldBe)
import Test.Hspec.Snap (get, snap, TestResponse(..), RespCode(..))

import Servant.API (JSON)
import Servant.API.Alternative ((:<|>)((:<|>)))
import Servant.API.Capture (Capture)
import Servant.API.Verbs
import Servant.API.Raw (Raw)
import Servant.API.Sub ((:>))
import Servant.Server (Server, serveSnap)
import Servant.ServerSpec (Person(Person))
import Servant.Utils.StaticFiles (serveDirectory)

type Api =
       "dummy_api" :> Capture "person_name" String :> Get '[JSON] Person
  :<|> "static" :> Raw

type AppHandler = Handler () ()

api :: Proxy Api
api = Proxy

app :: SnapletInit () ()
app = makeSnaplet "servantsnap" "A test for servant-snap" Nothing (return ())

server :: Server Api '[] AppHandler
server =
       (\ name_ -> return (Person name_ 42))
  :<|> serveDirectory "static"

withStaticFiles :: IO () -> IO ()
withStaticFiles action = withSystemTempDirectory "servant-test" $ \ tmpDir ->
  bracket (setup tmpDir) teardown (const action)
 where
  setup tmpDir = do
    outer <- getCurrentDirectory
    setCurrentDirectory tmpDir
    createDirectory "static"
    writeFile "static/foo.txt" "bar"
    writeFile "static/index.html" "index"
    return outer

  teardown outer = do
    setCurrentDirectory outer

spec :: Spec
spec = do
  around_ withStaticFiles $ snap (route ([("", serveSnap api server)])) app $ do
    describe "serveDirectory" $ do
      it "successfully serves files" $ do
        response <- get "/static/foo.txt"
        liftIO $ response `shouldBe` Html (RespCode 200) "bar"

      it "serves the contents of index.html when requesting the root of a directory" $ do
        response <- get "/static/"
        liftIO $ response `shouldBe` Html (RespCode 200) "index"
