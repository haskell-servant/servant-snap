{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.Server.CORSSpec where

import           Control.Monad.IO.Class      (liftIO)
import           Data.List                   (foldl')
import           Data.Proxy
import           Servant.API                 ((:<|>) (..), (:>), BasicAuth,
                                              Capture, CaptureAll, Header (..),
                                              Headers, IsSecure (..), JSON,
                                              NoContent (..), NoFraming,
                                              OctetStream, PlainText, QueryFlag,
                                              QueryParam, QueryParams, Raw,
                                              RemoteHost, ReqBody, SourceIO,
                                              Stream, addHeader)
import           Servant.API.Verbs           (Delete, Get, Patch, Post, Put,
                                              Verb)
import           Servant.Server              hiding (route)
import           Servant.Server.Internal     (HasServer)
import           Snap
import qualified Snap.Core                   as SC
import qualified Snap.Test                   as ST
import qualified Snap.Util.CORS              as CORS
import           Test.Hspec

import           Servant.Utils.SnapTestUtils

-- TODO: Cleanup
spec :: Spec
spec = do
  corsSanitySpec

corsSanitySpec :: Spec
corsSanitySpec = do
  let handler = CORS.applyCORS CORS.defaultOptions bareEndpoint
      bareEndpoint = return ()

      servantEndpoint :: Snap ()
      servantEndpoint = do
        let printHeaders m r = liftIO $ putStrLn m >> print (headers r)
        respA <- SC.getResponse
        serveSnap (Proxy :: Proxy ("int" :> Get '[JSON] Int)) (return  5 :: Snap Int)

      servantSnapletInit :: SnapletInit () ()
      servantSnapletInit = makeSnaplet "servant-test" "Testing servant" Nothing $ do
        addRoutes [
                   ("other", SC.writeText "other route")
                  ,("", servantSnapletHandler)
                  ]
        wrapSite (CORS.applyCORS CORS.defaultOptions)
        return ()

      servantSnapletHandler :: Handler () () ()
      servantSnapletHandler = do
        let printHeaders m r = liftIO $ putStrLn m >> print (headers r)
        respA <- SC.getResponse
        serveSnap (Proxy :: Proxy ("int" :> Get '[JSON] Int)) (return  5 :: Handler () () Int)

      forwardHeaders :: Snap ()
      forwardHeaders = do
          req  <- SC.getRequest
          resp <- SC.getResponse
          putResponse $ foldl' (\r (hk, hv) -> SC.setHeader hk hv r) resp (listHeaders req)

      req :: ST.RequestBuilder IO ()
      req = do
        ST.setRequestType $ ST.RequestWithRawBody GET ""
        ST.setRequestPath "/"
        ST.setHeader "Origin" "http://origin.org"

      req' :: ST.RequestBuilder IO ()
      req' = do
        ST.setRequestType $ ST.RequestWithRawBody GET ""
        ST.setRequestPath "int"
        ST.setHeader "Origin" "http://origin.org"

      req'' :: ST.RequestBuilder IO ()
      req'' = do
        ST.setRequestType $ ST.RequestWithRawBody GET ""
        ST.setRequestPath "other"
        ST.setHeader "Origin" "http://origin.org"

  it "Sets CORS and forwards req headers for Snap non-servant handler" $ do
    resp <- ST.runHandler req (forwardHeaders >> handler)
    Right resp `shouldHaveStatus` 200
    Right resp `shouldHaveHeaders` [("Origin", "http://origin.org")
                                   ,("Access-Control-Allow-Origin", "http://origin.org")]
  it "Sets CORS and forwards req headers for Snap handler" $ do
    resp <- ST.runHandler req' (CORS.applyCORS CORS.defaultOptions $ forwardHeaders >> servantEndpoint)
    Right resp `shouldHaveStatus` 200
    Right resp `shouldDecodeTo`   (5 :: Int)
    Right resp `shouldHaveHeaders` [("Origin", "http://origin.org")
                                   ,("Access-Control-Allow-Origin", "http://origin.org")
                                   ]
  it "Sets CORS and forwards req headers for Snaplet servant handler with wrapSite" $ do
    resp <- testSnaplet servantSnapletInit req'
    resp `shouldHaveStatus` 200
    resp `shouldDecodeTo`   (5 :: Int)
    resp `shouldHaveHeaders` [("Access-Control-Allow-Origin", "http://origin.org")
                       ]

  it "Sets CORS and forwards req headers for Snaplet not-servant handler with wrapSite" $ do
    resp <- testSnaplet servantSnapletInit req''
    resp `shouldHaveStatus` 200
    resp `shouldHaveHeaders` [("Access-Control-Allow-Origin", "http://origin.org")
                             ]
