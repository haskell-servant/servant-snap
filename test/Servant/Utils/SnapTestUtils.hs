{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Servant.Utils.SnapTestUtils where

import           Control.Lens                                (makeLenses)
import           Control.Monad                               (forM_, unless,
                                                              void, when)
import           Control.Monad.IO.Class                      (liftIO)
import           Data.Aeson
import qualified Data.Aeson                                  as A
import qualified Data.ByteString.Char8                       as B8
import qualified Data.ByteString.Lazy                        as BL
import           Data.CaseInsensitive                        (mk)
import           Data.List                                   (foldl')
import           Data.Maybe                                  (fromMaybe)
import           Data.Proxy
import qualified Data.Set                                    as Set
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Network.HTTP.Types                          (hAccept,
                                                              hContentType)
import qualified Network.HTTP.Types
import           Servant.API                                 ((:<|>) (..), (:>),
                                                              BasicAuth,
                                                              Capture,
                                                              CaptureAll,
                                                              Header (..),
                                                              Headers,
                                                              IsSecure (..),
                                                              JSON,
                                                              NoContent (..),
                                                              NoFraming,
                                                              OctetStream,
                                                              PlainText,
                                                              QueryFlag,
                                                              QueryParam,
                                                              QueryParams, Raw,
                                                              RemoteHost,
                                                              ReqBody, SourceIO,
                                                              Stream, addHeader)
import           Servant.API.Verbs                           (Delete, Get,
                                                              Patch, Post, Put,
                                                              Verb)
import           Servant.Server                              hiding (route)
import           Servant.Server.Internal                     (HasServer)
import           Snap
import qualified Snap.Core                                   as SC
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Snap.Test                                   as ST
import qualified Snap.Util.CORS                              as CORS
import           Test.Hspec
import qualified Test.HUnit                                  as HU


data App = App { _auth :: Snaplet (AuthManager App)
               , _sess :: Snaplet SessionManager}
makeLenses 'App

type AppHandler = Handler App App

app :: SnapletInit App App
app = app' []

app' :: [(B8.ByteString, AppHandler ())] -> SnapletInit App App
app' rs = makeSnaplet "servantsnap" "A test app for servant-snap" Nothing $ do
  s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
  a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes rs
  wrapSite (\h -> createTestUserIfMissing >> CORS.applyCORS CORS.defaultOptions h)
  return (App a s)

createTestUserIfMissing :: Handler App App ()
createTestUserIfMissing =
  with auth $ usernameExists testLogin >>= \case
    True  -> return ()
    False -> void $ createUser testLogin testPassword

testLogin    = "greg"
testPassword = "p@ssword"
------------------------------------------------------------------------------
-- * Assorted Snap helpers
------------------------------------------------------------------------------


mkInitAndServer :: (HasServer api context m, m ~ AppHandler)
                => Proxy (api :: *)
                -> Context context
                -> Server api context (AppHandler)
                -> (SnapletInit App App, AppHandler ())
mkInitAndServer api ctx serv =
  let sRoute = serveSnapWithContext api ctx serv
  in  (app' [("", sRoute)], sRoute)


mkRequest :: Method
          -> B8.ByteString
          -> B8.ByteString
          -> [Network.HTTP.Types.Header]
          -> B8.ByteString
          -> ST.RequestBuilder IO ()
mkRequest mth pth qs hds bdy = do
  let ct = fromMaybe "" (Prelude.lookup hContentType hds)
  ST.postRaw pth ct bdy
  ST.setQueryStringRaw qs
  unless (mth == SC.POST) $ ST.setRequestType (ST.RequestWithRawBody mth bdy)
  forM_ hds (\(k, v) -> unless (k == hContentType) $ ST.addHeader k v)
  -- req <- State.get -- Useful for debugging
  -- liftIO $ print req

runReqOnApi :: (HasServer api context m, m ~ AppHandler)
            => Proxy (api :: *)
            -> Context context
            -> Server api context AppHandler
            -> Method
            -> B8.ByteString
            -> B8.ByteString
            -> [Network.HTTP.Types.Header]
            -> B8.ByteString
            -> IO (Either T.Text Response)
runReqOnApi api ctx serv method route qs hds bod =
  let (sInit, serv') = mkInitAndServer api ctx serv
  -- in SST.runHandler Nothing (mkRequest method route qs hds bod) serv' sInit
  in testSnaplet sInit (mkRequest method route qs hds bod)

routes :: (HasServer api context m, m ~ AppHandler)
       => Proxy (api :: *)
       -> Context context
       -> Server api context (AppHandler)
       -> [(B8.ByteString, AppHandler ())]
routes p ctx s = [("", serveSnapWithContext p ctx s)]

testSnaplet :: SnapletInit b b -> ST.RequestBuilder IO () -> IO (Either T.Text Response)
testSnaplet snapletInit req = do
  (_, snapm, _) <- runSnaplet Nothing snapletInit
  fmap Right $ ST.runHandler req snapm

------------------------------------------------------------------------------
-- * hspec helpers
------------------------------------------------------------------------------

shouldHaveBody :: Either T.Text Response -> T.Text -> IO ()
shouldHaveBody (Left e) _ = HU.assertFailure $
                            "Failed to respond: " ++ T.unpack e
shouldHaveBody (Right r) a = do
  bod <- ST.getResponseBody r
  bod `shouldBe` T.encodeUtf8 a

shouldHaveStatus :: Either T.Text Response -> Int -> IO ()
shouldHaveStatus (Left e) _ = HU.assertFailure $
                              "Failed to respond: " ++ T.unpack e
shouldHaveStatus (Right r) a = do
  SC.rspStatus r `shouldBe` a


shouldDecodeTo :: (FromJSON a, Eq a, Show a)
               => Either T.Text Response
               -> a
               -> IO ()
shouldDecodeTo (Left e) _ = HU.assertFailure $
                            "Failed to respond: " ++ T.unpack e
shouldDecodeTo (Right resp) a = do
  bod <- ST.getResponseBody resp
  case A.decode' $ BL.fromStrict bod of
    Just x | x == a -> return ()
    Just _ -> HU.assertFailure $
              "Failed to decode response to " ++ show a ++
              " from body: " ++ B8.unpack bod
    Nothing -> HU.assertFailure $ "Failed to decode respone from body: " ++
               B8.unpack bod ++ "\nResponse: " ++ show resp

shouldHaveHeaders :: Either T.Text Response
                  -> [(B8.ByteString, B8.ByteString)]
                  -> Expectation
shouldHaveHeaders (Left e) _ = expectationFailure $ T.unpack e
shouldHaveHeaders (Right resp) hs = do
  let respHs  = Set.fromList $ SC.listHeaders resp
      hs'     = Set.fromList $  (\(k,v) -> (mk k,v)) <$> hs
      missing = Set.toList $ Set.difference hs' respHs
  case missing of
    [] -> return ()
    _  -> expectationFailure $
     "These expected headers and values were missing: " ++ show missing ++
     " from the response's: " ++ show (Set.toList respHs)

