{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module tests whether streaming works from client to server
-- with a server implemented with servant-server.
module Servant.StreamingSpec where

import           Control.Concurrent
import           Control.Exception hiding (Handler)
import           Control.Monad.IO.Class
import qualified Data.ByteString as Strict
import Control.Monad.State (modify)
import qualified Data.ByteString.Lazy as Lazy
import           Network.HTTP.Types
-- import           Network.Wai
-- import           Network.Wai.Internal
import           Prelude
-- import           Prelude.Compat
import           Servant
import           Snap.Core
import           Snap.Internal.Http.Types
import           Snap.Test
import           Snap
import qualified System.Timeout
import qualified System.IO.Streams as IOS
import           Test.Hspec

type TestAPI =
  ReqBody '[OctetStream] Lazy.ByteString :> Post '[JSON] NoContent

testAPI :: Proxy TestAPI
testAPI = Proxy

spec :: Spec
spec = return ()
-- TODO: This test fails
{-
spec :: Spec
spec = do
  -- The idea of this test is this:
  --
  -- - The mock client will
  --   - send some data in the request body, but not all,
  --   - wait for the server to acknowledge (outside of http, through an MVar)
  --     that the server received some data,
  --   - send the rest of the request body.
  -- - The mock server will
  --   - receive some data,
  --   - notify the client that it received some data,
  --   - receive the rest of the data,
  --   - respond with an empty result.
  it "client to server can stream lazy ByteStrings" $ timeout $ do
    serverReceivedFirstChunk <- newWaiter

    -- - streams some test data
    -- - waits for serverReceivedFirstChunk
    -- - streams some more test data
    streamTestData <- do
      mvar :: MVar [IO Strict.ByteString] <- newMVar $
        map return (replicate 1000 "foo") ++
        (waitFor serverReceivedFirstChunk >> return "foo") :
        map return (replicate 1000 "foo")
      return $ modifyMVar mvar $ \ actions -> case actions of
        (a : r) -> (r, ) <$> a
        [] -> return ([], "")

    rqStream <- IOS.makeInputStream (Just <$> streamTestData)
    rqStream2 <- IOS.makeInputStream (return (Just "foo"))
    let makeRequest :: RequestBuilder IO ()
        makeRequest = do
          postRaw @IO "/" "application/octet-stream" "barbar"
          -- transformRequestBody $ \inStream -> return rqStream
          -- modify $ \rq -> rq { rqBody = rqStream }

    -- - receives the first chunk
    -- - notifies serverReceivedFirstChunk
    -- - receives the rest of the request
    let handler :: Lazy.ByteString -> Snap NoContent
        handler input = do
          liftIO $ putStrLn $ "handler Hello1"
          -- rq <- getRequest
          -- putRequest (rq { rqBody = rqStream })
          -- transformRequestBody $ \_ -> return rqStream2
          liftIO $ putStrLn $ "handler Hello2"
          liftIO $ do
            print "handler Hello3"
            let prefix = Lazy.take 3 input
            prefix `shouldBe` "foo"
            print "handler Hello4"
            notify serverReceivedFirstChunk ()
            print "handler Hello5"
            input `shouldBe` mconcat (replicate 2001 "foo")
            return NoContent

        app = serveSnap testAPI handler
    putStrLn "Hello1"
    response <- executeRequest rqStream makeRequest app
    rspStatus response `shouldBe` 200

executeRequest :: IOS.InputStream Strict.ByteString -> RequestBuilder IO () -> Snap () -> IO Response
executeRequest rqStream makeRequest act = do
  responseMVar <- newEmptyMVar
  putStrLn "Hello2"
  -- evalHandler (buildRequest get (Lazy.replicate "foo" 2001)) (act >> putMVar responseMVar response)
  -- evalHandler makeRequest (act >> get >>= \r -> liftIO (putMVar responseMVar r))
  evalHandler makeRequest $ do
    liftIO $ putStrLn "Hello3"
    -- rq <- getRequest
    liftIO $ putStrLn "Hello3.5"
    -- putRequest (rq { rqBody = rqStream })
    transformRequestBody $ \_ -> return rqStream
    liftIO $ putStrLn "Hello3.7"
    act
    liftIO $ putStrLn "Hello4"
    r <- Snap.getResponse
    liftIO (putMVar responseMVar r)

    -- (act >> get >>= \r -> liftIO (putMVar responseMVar r))
  -- let respond response = do
  --       putMVar responseMVar response
  --       return ResponseReceived
  -- ResponseReceived <- app request respond
  takeMVar responseMVar

-- executeRequest :: Application -> Request -> IO Response
-- executeRequest app request = do
--   responseMVar <- newEmptyMVar
--   let respond response = do
--         putMVar responseMVar response
--         return ResponseReceived
--   ResponseReceived <- app request respond
--   takeMVar responseMVar

timeout :: IO a -> IO a
timeout action = do
  result <- System.Timeout.timeout 1000000 action
  maybe (throwIO $ ErrorCall "timeout") return result

-- * waiter

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

newWaiter :: IO (Waiter a)
newWaiter = do
  mvar <- newEmptyMVar
  return $ Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }
-}
