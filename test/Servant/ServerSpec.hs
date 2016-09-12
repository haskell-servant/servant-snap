{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Servant.ServerSpec where


import           Control.Monad              (forM_, unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Base64     as Base64
import           Data.CaseInsensitive       (CI, mk, original)
import           Data.Char                  (toUpper)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set                   as Set
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.Encoding    as TL
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (Status(..), hAccept, methodGet, methodPost, methodPut, methodDelete, methodPatch, methodHead)
import qualified Network.HTTP.Types
import           Snap.Core                  hiding (Headers, addHeader)
import qualified Snap.Core                  as SC
import           Snap.Snaplet
import qualified Snap.Snaplet.Test          as SST
import qualified Snap.Test                  as ST

import           Test.Hspec
import           Test.Hspec.Core.Spec (Result(..))
import           Test.Hspec.Snap            hiding (NotFound)
import qualified Test.Hspec.Snap            as THS
import qualified Test.HUnit                 as HU
import           Servant.API                ((:<|>) (..), (:>),
                                             addHeader, BasicAuth(..), Capture, CaptureAll,
                                             Header (..), Headers, IsSecure(..),
                                             JSON, NoContent(..),
                                             PlainText,
                                             QueryFlag, QueryParam,
                                             QueryParams, Raw, RemoteHost, ReqBody)
import           Servant.API.Verbs          (Verb, Get, Post, Put, Delete, Patch)
import qualified Servant.API.Verbs          as V
import           Servant.Server             hiding (route)
import           Servant.Server.Internal    (HasServer)

import           Servant.Server.Internal.RoutingApplication hiding (Fail)

import Debug.Trace


-- * test data types

data App = App
type AppHandler = Handler App App

app :: SnapletInit App App
app = app'  []

app' :: [(B8.ByteString, AppHandler ())] -> SnapletInit App App
app' rs = makeSnaplet "servantsnap" "A test app for servant-snap" Nothing $ do
  addRoutes rs
  return App

routes :: HasServer api
       => Proxy (api :: *)
       -> Server api AppHandler
       -> [(B8.ByteString, AppHandler ())]
routes p s = [("", serveSnap p s)]


-- * Specs

spec :: Spec
spec = do
  verbSpec
  captureSpec
  -- queryParamSpec
  -- reqBodySpec
  -- headerSpec
  -- rawSpec
  alternativeSpec
  -- responseHeadersSpec
  -- miscCombinatorSpec
  -- basicAuthSpec
  -- genAuthSpec

------------------------------------------------------------------------------
-- * verbSpec {{{
------------------------------------------------------------------------------

type VerbApi method status
    =                Verb method status '[JSON] Person
 :<|> "noContent" :> Verb method status '[JSON] NoContent
 :<|> "header"    :> Verb method status '[JSON] (Headers '[Header "H" Int] Person)
 :<|> "headerNC"  :> Verb method status '[JSON] (Headers '[Header "H" Int] NoContent)
 :<|> "accept"    :> (    Verb method status '[JSON] Person
                     :<|> Verb method status '[PlainText] String
                     )

getStatus :: TestResponse -> Maybe Int
getStatus (Html (RespCode s) _) = Just s
getStatus (Json (RespCode s) _) = Just s
getStatus THS.NotFound = Nothing
getStatus (Redirect (RespCode s) _) = Just s
getStatus (Other (RespCode s)) = Just s
getStatus Empty = Nothing

statusIs :: TestResponse -> Int -> Bool
statusIs r s = getStatus r == Just s

decodesTo :: (FromJSON a, Eq a) => TestResponse -> a -> Bool
decodesTo (Json _ bs) a = A.decode' bs == Just a
decodesTo _ _ = False

bodyIs :: TestResponse -> TL.Text -> Bool
bodyIs (Html _ t) target = t == TL.toStrict target
bodyIs (Json _ b) target = b == TL.encodeUtf8 target
bodyIs _ _ = False

-- shouldDecodeTo :: (FromJSON a, Eq a) => TestResponse -> a -> Bool
shouldDecodeTo' (Json _ bs) t = case A.decode' bs of
  Just x | x == t -> setResult Success
  Just x | x /= t -> setResult $ Fail Nothing ("Expected to decode to " ++ show t ++ " but decoded to " ++ show x)
  Nothing -> setResult $ Fail Nothing "Failed to decode"
shouldDecodeTo' r _ = setResult $ Fail Nothing ("Expected JSON, got " ++ show r)

shouldDecodeTo :: (FromJSON a, Eq a, Show a) => Either T.Text Response -> a -> IO ()
shouldDecodeTo (Left e) _ =  HU.assertFailure "No response"
shouldDecodeTo (Right resp) a = do
  bod <- ST.getResponseBody resp
  case A.decode' $ BL.fromStrict bod of
    Just x | x == a -> return ()
    Just _ -> HU.assertFailure $ "Failed to decode response to " ++ show a
    Nothing -> HU.assertFailure $ "Failed to decode respone"
    

mkRequest :: Method -> B8.ByteString -> [Network.HTTP.Types.Header] -> B8.ByteString -> ST.RequestBuilder IO ()
mkRequest mth pth hds bdy = do
  ST.postRaw pth "" bdy
  ST.setRequestType (ST.RequestWithRawBody mth bdy)
  forM_ hds (\(hKey, hVal) -> ST.addHeader hKey  hVal)

verbSpec :: Spec
verbSpec = do
  let server :: Server (VerbApi method status) AppHandler
      server = return alice
          :<|> return NoContent
          :<|> return (addHeader 5 alice)
          :<|> return (addHeader 10 NoContent)
          :<|> (return alice :<|> return "B")
      get200     = Proxy :: Proxy (VerbApi 'V.GET 200)
      post210    = Proxy :: Proxy (VerbApi 'V.POST 210)
      put203     = Proxy :: Proxy (VerbApi 'V.PUT 203)
      delete280  = Proxy :: Proxy (VerbApi 'V.DELETE 280)
      patch214   = Proxy :: Proxy (VerbApi 'V.PATCH 214)
      wrongMethod m  = if m == SC.PUT then SC.POST else SC.PUT
      test desc api verbRoutes (method :: SC.Method) (status :: Int) = do
        snap (route verbRoutes) app $ describe ("Servant.API.Verb " ++ show method) $ do

          -- HEAD and 214/215 need not return bodies
          -- unless (status `elem` [214, 215] || method == SC.HEAD) $
          --   it "returns the person" $ do
          --     response <- runRequest (mkRequest method "/" [] "")
          --     liftIO $ statusIs response status `shouldBe` True
          --     response `shouldDecodeTo` alice -- decodesTo response alice `shouldBe` True

          it "returns no content on NoContent" $ do
              response <- runRequest (mkRequest method "/noContent" [] "")
              liftIO $ statusIs response status `shouldBe` True
              liftIO $ bodyIs response "" `shouldBe` True

          -- HEAD should not return body
          when (method == SC.HEAD) $
            it "HEAD returns no content body" $ do
              response <- runRequest $ mkRequest method  "/" [] ""
              liftIO $ bodyIs response "" `shouldBe` True

          it "throws 405 on wrong method " $ do
            response <- runRequest $ mkRequest (wrongMethod method) "/" [] ""
            liftIO $ statusIs response 405 `shouldBe` True

          -- -- TODO: get hspec-snap to return headers
          -- it "returns headers" $ do
          --   response1 <- runRequest $ mkRequest method "/header" [] ""
          --   liftIO $ isStatus response1 status `shouldBe` True
          --   liftIO $ simpleHeaders response1 `shouldContain` [("H", "5")]

          --   response2 <- handler "/header" [] ""
          --   liftIO $ statusCode (simpleStatus response2) `shouldBe` status
          --   liftIO $ simpleHeaders response2 `shouldContain` [("H", "5")]

          it "handles trailing '/' gracefully" $ do
            response <- runRequest $ mkRequest method "/headerNC/" [] ""
            liftIO $ statusIs response status `shouldBe` True

          it "returns 406 if the Accept header is not supported" $ do
            response <- runRequest $ mkRequest method "" [(hAccept, "crazy/mime")] ""
            liftIO $ statusIs response 406  `shouldBe` True

          it "responds if the Accept header is supported" $ do
            response <- runRequest $ mkRequest method ""
               [(hAccept, "application/json")] ""
            liftIO $ statusIs response status `shouldBe` True

          unless (status `elem` [214, 215] || method == SC.HEAD) $
            it "allows modular specification of supported content types" $ do
              response <- runRequest $ mkRequest method "/accept" [(hAccept, "text/plain")] ""
              liftIO $ statusIs response status `shouldBe` True
              liftIO $ bodyIs response "B" `shouldBe` True

          -- it "sets the Content-Type header" $ do
          --   response <- runRequest $ mkRequest method  "" [] ""
          --   liftIO $ simpleHeaders response `shouldContain`
          --     [("Content-Type", "application/json")]

        let sInit = app' verbRoutes
            runUrl p = SST.runHandler Nothing (mkRequest method p [] "") (serveSnap api server) sInit
        describe "Servant.API.Verb Headers" $ do

          -- HEAD and 214/215 need not return bodies
          unless (status `elem` [214, 215] || method == SC.HEAD) $
            it "returns the person" $ do
               resp <- runUrl "/" 
               resp `shouldDecodeTo` alice
               let (Right r') = resp in SC.rspStatus r' `shouldBe` status

          it "returs headers" $ do
            resp <- SST.runHandler Nothing (mkRequest method "/header" [] "") (serveSnap api server) sInit
            shouldHaveHeaders resp  [("H","5")]

          it "sets the content-type header" $ do
            resp <- SST.runHandler Nothing (mkRequest method "" [] "") (serveSnap api server) sInit
            resp `shouldHaveHeaders` [("Content-Type", "application/json")]

  -- test descr API routes method status
  test "GET 200" get200 (routes get200 server) SC.GET 200
  test "POST 210" post210 (routes post210 server) SC.POST 210
  test "PUT 203" put203 (routes put203 server) SC.PUT 203
  test "DELETE 280" delete280 (routes delete280 server) SC.DELETE  280
  test "PATCH 214" patch214 (routes patch214 server) SC.PATCH 214
  test "GET 200 with HEAD" get200 (routes get200 server) SC.HEAD 200

-- }}}

shouldHaveHeaders :: Either T.Text Response -> [(B8.ByteString, B8.ByteString)] -> Expectation
shouldHaveHeaders (Left e) hs = expectationFailure $ T.unpack e
shouldHaveHeaders (Right resp) hs = do
  let respHs  = Set.fromList $ SC.listHeaders resp :: Set.Set (CI B8.ByteString, B8.ByteString)
      hs'     = Set.fromList $  (\(k,v) -> (mk k,v)) <$> hs  :: Set.Set (CI B8.ByteString, B8.ByteString)
      missing = Set.toList $ Set.difference hs' respHs  :: [(CI B8.ByteString, B8.ByteString)]
  case missing of
    [] -> return ()
    _  -> expectationFailure $
     "These expected headers and values were missing: " ++ show missing ++
     " from the response's: " ++ show (Set.toList respHs)

------------------------------------------------------------------------------
-- * captureSpec {{{
------------------------------------------------------------------------------

type CaptureApi = Capture "legs" Integer :> Get '[JSON] Animal
captureApi :: Proxy CaptureApi
captureApi = Proxy
captureServer :: Integer -> AppHandler Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> throwError err404

type CaptureApi2 = CaptureApi :<|> Raw

captureApi2 :: Proxy CaptureApi2
captureApi2 = Proxy

captureServer2 :: Server CaptureApi2 AppHandler
captureServer2 = captureServer :<|> (do
  rq <- getRequest
  writeBS (SC.rqPathInfo rq))

captureSpec :: Spec
captureSpec = snap (route (routes captureApi captureServer)) app $  do
  describe "Servant.API.Capture" $ do

      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ decodesTo response tweety `shouldBe` True

      it "returns 400 if the decoding fails" $ do
        response <- get "/notAnInt"
        liftIO $ statusIs response 404 `shouldBe` True

{- TODO
    with (return (serve
        (Proxy :: Proxy (Capture "captured" String :> Raw))
        (\ "captured" request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))
-}

-- }}}
------------------------------------------------------------------------------
-- * captureAllSpec {{{
------------------------------------------------------------------------------

type CaptureAllApi = CaptureAll "legs" Integer :> Get '[JSON] Animal
captureAllApi :: Proxy CaptureAllApi
captureAllApi = Proxy
captureAllServer :: [Integer] -> AppHandler Animal
captureAllServer legs = case sum legs of
  4 -> return jerry
  2 -> return tweety
  0 -> return beholder
  _ -> throwError err404

captureAllSpec :: Spec
captureAllSpec = snap (route (routes captureAllApi captureAllServer)) app $ do
  describe "Servant.API.CaptureAll" $ do

      it "can capture a single element of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ decodesTo response tweety `shouldBe` True

      it "can capture multiple elements of the 'pathInfo'" $ do
        response <- get "/2/2"
        liftIO $ decodesTo response jerry `shouldBe` True

      it "can capture arbitrarily many elements of the 'pathInfo'" $ do
        response <- get "/1/1/0/1/0/1"
        liftIO $ decodesTo response jerry `shouldBe` True

      it "can capture when there are no elements in 'pathInfo'" $ do
        response <- get "/"
        liftIO $ decodesTo response jerry `shouldBe` True

      it "returns 400 if the decoding fails" $ do
        response <- get "/notAnInt"
        liftIO $ statusIs response 400 `shouldBe` True

      it "returns 400 if the decoding fails, regardless of which element" $ do
        response <- get "/1/0/0/notAnInt/3/"
        liftIO $ statusIs response 400 `shouldBe` True

      it "returns 400 if the decoding fails, even when it's multiple elements" $ do
        response <- get "/1/0/0/notAnInt/3/orange/"
        liftIO $ statusIs response 400 `shouldBe` True

{- TODO
    with (return (serve
        (Proxy :: Proxy (CaptureAll "segments" String :> Raw))
        (\ _captured request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "consumes everything from pathInfo" $ do
        get "/captured/foo/bar/baz" `shouldRespondWith` (fromString (show ([] :: [Int])))
-}

-- }}}
------------------------------------------------------------------------------
-- * queryParamSpec {{{
------------------------------------------------------------------------------

type QueryParamApi = QueryParam "name" String :> Get '[JSON] Person
                :<|> "a" :> QueryParams "names" String :> Get '[JSON] Person
                :<|> "b" :> QueryFlag "capitalize" :> Get '[JSON] Person

queryParamApi :: Proxy QueryParamApi
queryParamApi = Proxy

qpServer :: Server QueryParamApi AppHandler
qpServer = queryParamServer :<|> qpNames :<|> qpCapitalize

  where qpNames (_:name2:_) = return alice { name = name2 }
        qpNames _           = return alice

        qpCapitalize False = return alice
        qpCapitalize True  = return alice { name = map toUpper (name alice) }

        queryParamServer (Just name_) = return alice{name = name_}
        queryParamServer Nothing = return alice

{-
queryParamSpec :: Spec
queryParamSpec = do
  describe "Servant.API.QueryParam" $ do
      it "allows retrieving simple GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params1 = "?name=bob"
          response1 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params1,
            queryString = parseQuery params1
           }
          liftIO $ do
            decode' (simpleBody response1) `shouldBe` Just alice{
              name = "bob"
             }

      it "allows retrieving lists in GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params2 = "?names[]=bob&names[]=john"
          response2 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params2,
            queryString = parseQuery params2,
            pathInfo = ["a"]
           }
          liftIO $
            decode' (simpleBody response2) `shouldBe` Just alice{
              name = "john"
             }


      it "allows retrieving value-less GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params3 = "?capitalize"
          response3 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3,
            queryString = parseQuery params3,
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3) `shouldBe` Just alice{
              name = "ALICE"
             }

          let params3' = "?capitalize="
          response3' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3',
            queryString = parseQuery params3',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "ALICE"
             }

          let params3'' = "?unknown="
          response3'' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3'',
            queryString = parseQuery params3'',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3'') `shouldBe` Just alice{
              name = "Alice"
             }
-}

-- }}}
------------------------------------------------------------------------------
-- * reqBodySpec {{{
------------------------------------------------------------------------------
type ReqBodyApi = ReqBody '[JSON] Person :> Post '[JSON] Person
           :<|> "blah" :> ReqBody '[JSON] Person :> Put '[JSON] Integer

{-
reqBodyApi :: Proxy ReqBodyApi
reqBodyApi = Proxy

reqBodySpec :: Spec
reqBodySpec = snap (route (route reqBodyApi reqBodyServer)) app $ do


    it "passes the argument to the handler" $ do
      response <- mkReq post "" (encode alice)
      liftIO $ decodesTo response alice `shouldBe` True

    -- TODO
    -- it "rejects invalid request bodies with status 400" $ do
    --   mkReq methodPut "/blah" "some invalid body" `shouldRespondWith` 400

    -- it "responds with 415 if the request body media type is unsupported" $ do
    --   post "/"
    --     [(hContentType, "application/nonsense")] "" `shouldRespondWith` 415

  where server :: Server ReqBodyApi AppHandler
        server = return :<|> return . age
        -- mkReq handler method x = mkRequest x
        --    [(hContentType, "application/json;charset=utf-8")] ""

-}

-- }}}
------------------------------------------------------------------------------
-- * headerSpec {{{
------------------------------------------------------------------------------

{- TODO
type HeaderApi a = Header "MyHeader" a :> Delete '[JSON] NoContent
headerApi :: Proxy (HeaderApi a)
headerApi = Proxy

headerSpec :: Spec
headerSpec = describe "Servant.API.Header" $ do

    let expectsInt :: Maybe Int -> Handler NoContent
        expectsInt (Just x) = do
          when (x /= 5) $ error "Expected 5"
          return NoContent
        expectsInt Nothing  = error "Expected an int"

    let expectsString :: Maybe String -> Handler NoContent
        expectsString (Just x) = do
          when (x /= "more from you") $ error "Expected more from you"
          return NoContent
        expectsString Nothing  = error "Expected a string"

    with (return (serve headerApi expectsInt)) $ do
        let delete' x = delete x [("MyHeader", "5")]

        it "passes the header to the handler (Int)" $
            delete' "/" "" `shouldRespondWith` 200

    with (return (serve headerApi expectsString)) $ do
        let delete' x = delete x [("MyHeader", "more from you")]

        it "passes the header to the handler (String)" $
            delete' "/" "" `shouldRespondWith` 200

-}

-- }}}
------------------------------------------------------------------------------
-- * rawSpec {{{
------------------------------------------------------------------------------

{-
type RawApi = "foo" :> Raw

rawApi :: Proxy RawApi
rawApi = Proxy

rawApplication :: Show a => (Request -> a) -> Application
rawApplication f request_ respond = respond $ responseLBS ok200 []
    (cs $ show $ f request_)

rawSpec :: Spec
rawSpec = do
  describe "Servant.API.Raw" $ do
    it "runs applications" $ do
      (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo"]
         }
        liftIO $ do
          simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      (flip runSession) (serve rawApi (rawApplication pathInfo)) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo", "bar"]
         }
        liftIO $ do
          simpleBody response `shouldBe` cs (show ["bar" :: String])
-}

-- }}}
------------------------------------------------------------------------------
-- * alternativeSpec {{{
------------------------------------------------------------------------------
type AlternativeApi =
       "foo" :> Get '[JSON] Person
  :<|> "bar" :> Get '[JSON] Animal
  :<|> "foo" :> Get '[PlainText] T.Text
  :<|> "bar" :> Post '[JSON] Animal
  :<|> "bar" :> Put '[JSON] Animal
  :<|> "bar" :> Delete '[JSON] NoContent

alternativeApi :: Proxy AlternativeApi
alternativeApi = Proxy

alternativeServer :: Server AlternativeApi AppHandler
alternativeServer =
       return alice
  :<|> return jerry
  :<|> return "a string"
  :<|> return jerry
  :<|> return jerry
  :<|> return NoContent

alternativeSpec :: Spec
alternativeSpec = snap (route (routes alternativeApi alternativeServer)) app $ do
  describe "Servant.API.Alternative" $ do

      it "unions endpoints" $ do
        response <- get "/foo"
        liftIO $ do
          decodesTo response alice `shouldBe` True
        response_ <- get "/bar"
        liftIO $ do
          decodesTo response_ jerry `shouldBe`
            True

      it "checks all endpoints before returning 415" $ do
        response <- get "/foo"
        liftIO $ statusIs response 200 `shouldBe` True

      it "returns 404 if the path does not exist" $ do
        response <- get "/nonexistent"
        liftIO $ statusIs response 404 `shouldBe` True
-- }}}
------------------------------------------------------------------------------
-- * responseHeaderSpec {{{
------------------------------------------------------------------------------
type ResponseHeadersApi =
       Get   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Post  '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Put   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Patch '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)


responseHeadersServer :: Server ResponseHeadersApi AppHandler
responseHeadersServer = let h = return $ addHeader 5 $ addHeader "kilroy" "hi"
  in h :<|> h :<|> h :<|> h


{- TODO
responseHeadersSpec :: Spec
responseHeadersSpec = describe "ResponseHeaders" $ do
  with (return $ serve (Proxy :: Proxy ResponseHeadersApi) responseHeadersServer) $ do

    let methods = [get, post, put, patch]

    it "includes the headers in the response" $
      forM_ methods $ \method ->
        method "/" [] ""
          `shouldRespondWith` "\"hi\""{ matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
                                      , matchStatus  = 200
                                      }

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \method ->
        method "blahblah" [] ""
          `shouldRespondWith` 404

    it "returns 406 if the Accept header is not supported" $
      forM_ methods $ \method ->
        method "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406

-}

-- }}}
------------------------------------------------------------------------------
-- * miscCombinatorSpec {{{
------------------------------------------------------------------------------
{- TODO
type MiscCombinatorsAPI
  =    "version" :> HttpVersion :> Get '[JSON] String
  :<|> "secure"  :> IsSecure :> Get '[JSON] String
  :<|> "host"    :> RemoteHost :> Get '[JSON] String

miscApi :: Proxy MiscCombinatorsAPI
miscApi = Proxy

miscServ :: Server MiscCombinatorsAPI AppHandler
miscServ = versionHandler
      :<|> secureHandler
      :<|> hostHandler

  where versionHandler = return . show
        secureHandler Secure = return "secure"
        secureHandler NotSecure = return "not secure"
        hostHandler = return . show

miscCombinatorSpec :: Spec
miscCombinatorSpec = snap (route (routes miscApi miscServ)) app $ do
  describe "Misc. combinators for request inspection" $ do
    it "Successfully gets the HTTP version specified in the request" $
      go "/version" "\"HTTP/1.0\""

    it "Checks that hspec-wai uses HTTP, not HTTPS" $
      go "/secure" "\"not secure\""

    it "Checks that hspec-wai issues request from 0.0.0.0" $
      go "/host" "\"0.0.0.0:0\""

  where go path res = do
          response <- get path
          liftIO $ statusIs response res `shouldBe` True
-}
-- }}}
------------------------------------------------------------------------------
-- * Basic Authentication {{{
------------------------------------------------------------------------------

{- TODO
type BasicAuthAPI =
       BasicAuth "foo" () :> "basic" :> Get '[JSON] Animal
  :<|> Raw

basicAuthApi :: Proxy BasicAuthAPI
basicAuthApi = Proxy

basicAuthServer :: Server BasicAuthAPI
basicAuthServer =
  const (return jerry) :<|>
  (\ _ respond -> respond $ responseLBS imATeaPot418 [] "")

basicAuthContext :: Context '[ BasicAuthCheck () ]
basicAuthContext =
  let basicHandler = BasicAuthCheck $ \(BasicAuthData usr pass) ->
        if usr == "servant" && pass == "server"
          then return (Authorized ())
          else return Unauthorized
  in basicHandler :. EmptyContext

basicAuthSpec :: Spec
basicAuthSpec = do
  describe "Servant.API.BasicAuth" $ do
    with (return (serveWithContext basicAuthApi basicAuthContext basicAuthServer)) $ do

      context "Basic Authentication" $ do
        let basicAuthHeaders user password =
              [("Authorization", "Basic " <> Base64.encode (user <> ":" <> password))]
        it "returns 401 when no credentials given" $ do
          get "/basic" `shouldRespondWith` 401

        it "returns 403 when invalid credentials given" $ do
          THS.request methodGet "/basic" (basicAuthHeaders "servant" "wrong") ""
            `shouldRespondWith` 403

        it "returns 200 with the right password" $ do
          THS.request methodGet "/basic" (basicAuthHeaders "servant" "server") ""
            `shouldRespondWith` 200

        it "plays nice with subsequent Raw endpoints" $ do
          get "/foo" `shouldRespondWith` 418

-}

-- }}}
------------------------------------------------------------------------------
-- * General Authentication {{{
------------------------------------------------------------------------------

{-
type GenAuthAPI = AuthProtect "auth" :> "auth" :> Get '[JSON] Animal
             :<|> Raw

genAuthApi :: Proxy GenAuthAPI
genAuthApi = Proxy

genAuthServer :: Server GenAuthAPI
genAuthServer = const (return tweety)
           :<|> (\ _ respond -> respond $ responseLBS imATeaPot418 [] "")

type instance AuthServerData (AuthProtect "auth") = ()

genAuthContext :: Context '[AuthHandler Request ()]
genAuthContext =
  let authHandler = \req -> case lookup "Auth" (requestHeaders req) of
        Just "secret" -> return ()
        Just _ -> throwE err403
        Nothing -> throwE err401
  in mkAuthHandler authHandler :. EmptyContext

genAuthSpec :: Spec
genAuthSpec = do
  describe "Servant.API.Auth" $ do
    with (return (serveWithContext genAuthApi genAuthContext genAuthServer)) $ do

      context "Custom Auth Protection" $ do
        it "returns 401 when missing headers" $ do
          get "/auth" `shouldRespondWith` 401

        it "returns 403 on wrong passwords" $ do
          THS.request methodGet "/auth" [("Auth","wrong")] "" `shouldRespondWith` 403

        it "returns 200 with the right header" $ do
          THS.request methodGet "/auth" [("Auth","secret")] "" `shouldRespondWith` 200

        it "plays nice with subsequent Raw endpoints" $ do
          get "/foo" `shouldRespondWith` 418
-}

-- }}}
------------------------------------------------------------------------------
-- * Test data types {{{
------------------------------------------------------------------------------

data Person = Person {
  name :: String,
  age  :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 42

data Animal = Animal {
  species      :: String,
  numberOfLegs :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Animal
instance FromJSON Animal

jerry :: Animal
jerry = Animal "Mouse" 4

tweety :: Animal
tweety = Animal "Bird" 2

beholder :: Animal
beholder = Animal "Beholder" 0
-- }}}


{-  * The old test-suite (was half failing, half passing)

data Person = Person {
  name :: String,
  age  :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 42

data Animal = Animal {
  species      :: String,
  numberOfLegs :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Animal
instance FromJSON Animal

jerry :: Animal
jerry = Animal "Mouse" 4

tweety :: Animal
tweety = Animal "Bird" 2


-- * specs

spec :: Spec
spec = do
  captureSpec
--  getSpec
  postSpec
  putSpec
  -- patchSpec
  queryParamSpec
  headerSpec
  rawSpec
  unionSpec
  prioErrorsSpec
--   errorsSpec
  responseHeadersSpec

traceShow' a = traceShow a a

type CaptureApi = Capture "legs" Integer :> Get '[JSON] Animal

captureApi :: Proxy CaptureApi
captureApi = Proxy

captureServer :: Integer -> AppHandler Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> finishWith (setResponseCode 404 emptyResponse)

type CaptureApi2 = Capture "captured" String :> Raw
captureApi2 :: Proxy CaptureApi2
captureApi2 = Proxy

captureServer2 :: String  -> Server Raw AppHandler
captureServer2 _ = do
  r <- getRequest
  writeBS (rqPathInfo r)

captureSpec :: Spec
captureSpec = do
  snap (route (routes captureApi captureServer)) app $ do
   describe "Servant.API.Capture" $ do

      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        case response of
          Json _ bs -> do
            let d = decode' bs
            d `shouldEqual` Just tweety
          _       -> setResult (Fail Nothing "Should have been json body")

      it "returns 404 if the decoding fails" $ do
        get "/notAnInt" >>= should404

  snap (route (routes captureApi2 captureServer2)) app $ do
    describe "Servant.API.Capture" $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" >>= shouldHaveText "foo"


type GetApi = Get '[JSON] Person
        :<|> "empty" :> GetNoContent '[JSON] NoContent
getApi :: Proxy GetApi
getApi = Proxy

should405 :: TestResponse -> SnapHspecM b ()
should405 (Html _ _) = setResult (Fail Nothing "Should have failed, got HTML")
should405 (Other 405) = setResult Success
should405 _ = setResult (Fail Nothing "Should have 405'd")

getSpec :: Spec
-- getSpec = snap (route (routes getApi ((return alice :: AppHandler Person) :<|> (return NoContent :: AppHandler NoContent)))) app $ do
getSpec = snap (route (routes getApi (return alice :<|> return NoContent))) app $ do
  describe "Servant.API.Get" $ do

      it "allows to GET a Person" $ do
        response <- get "/"
        case response of
          Json _ bs -> do
            decode' bs `shouldEqual`  (Just alice)
          _ -> setResult (Fail Nothing "Should have been json body")

      it "throws 405 (wrong method) on POSTs" $ do
        postJson "/" ("" :: String) >>= should405
        postJson "/empty" ("" :: String) >>= should405

      it "returns 204 if the type is '()'" $ do
        get "empty" >>= shouldEqual (Other 204)

      it "returns 415 if the Accept header is not supported" $ do
        get' "" (Map.fromList [("Accept", ["crazy/mime"])]) >>= shouldEqual (Other 415)



type QueryParamApi = QueryParam "name" String :> Get '[JSON] Person
                :<|> "a" :> QueryParams "names" String :> Get '[JSON] Person
                :<|> "b" :> QueryFlag "capitalize" :> Get '[JSON] Person

queryParamApi :: Proxy QueryParamApi
queryParamApi = Proxy

qpServer :: Server QueryParamApi AppHandler
qpServer = queryParamServer :<|> qpNames :<|> qpCapitalize

  where qpNames (_:name2:_) = return alice { name = name2 }
        qpNames _           = return alice

        qpCapitalize False = return alice
        qpCapitalize True  = return alice { name = map toUpper (name alice) }

        queryParamServer (Just name_) = return alice{name = name_}
        queryParamServer Nothing = return alice

queryParamSpec :: Spec
queryParamSpec = snap (route (routes queryParamApi qpServer)) app $ do
  describe "Servant.API.QueryParam" $ do
      it "allows to retrieve simple GET parameters" $ do
          response1 <- get "?name=bob"
          case response1 of
            Json _ bs ->
              decode' bs `shouldEqual` (Just $ alice{name = "bob"})
            _ -> setResult (Fail Nothing "Should have been json body")

      it "allows to retrieve lists in GET parameters" $ do
          let params2 = "?names[]=bob&names[]=john"
          response2 <- get ("a" <> params2)
          case response2 of
            Json _ bs ->
              decode' bs `shouldEqual` Just alice{name="john"}
            _ -> setResult  (Fail Nothing "Should have been json body")

      it "allows to retrieve value-less GET parameters" $ do
          response3 <- get "b?capitalize"
          case response3 of
            Json _ bs ->
              decode' bs `shouldEqual` Just alice{name="ALICE"}
            _ -> setResult  (Fail Nothing "Should have been json body")

      it "allows to retrieve value-less GET parameters again" $ do -- TODO rename
          response3' <- get "b?capitalize"
          case response3' of
            Json _ bs ->
              decode' bs `shouldEqual` Just alice{name="ALICE"}
            _ -> setResult  (Fail Nothing "Should have been json body")

      it "allows to retrieve value-less GET parameters again" $ do -- TODO rename
          response3'' <- get "b?unknown="
          case response3'' of
            Json _ bs ->
              decode' bs `shouldEqual` Just alice{name="Alice"}
            _ -> setResult  (Fail Nothing "Should have been json body")



shouldDecodeTo (Json _ bs) v = decode' bs `shouldEqual` Just v
shouldDecodeTo _           _ = setResult (Fail Nothing "Should have been json body")


type PostApi =
       ReqBody '[JSON] Person :> Post '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Post '[JSON] Integer
  :<|> "empty" :> Post '[JSON] ()

postApi :: Proxy PostApi
postApi = Proxy

pServer = return . age :<|> return . age :<|> return ()

postSpec :: Spec
postSpec = snap (route (routes postApi pServer)) app $ do
  describe "Servant.API.Post and .ReqBody" $ do

      it "allows to POST a Person" $ do
        postJson "/" alice >>= shouldHaveText "42"

      it "allows alternative routes if all have request bodies" $ do
        postJson "/bla" alice >>= shouldHaveText "42"

      it "handles trailing '/' gracefully" $ do
        postJson "/bla/" alice >>= shouldHaveText "42"

      it "correctly rejects invalid request bodies with status 400" $ do
        postJson "/" ("some invalid body" :: String) >>= (`shouldEqual`  (Other 400))

      it "returns 204 if the type is '()'" $ do
        postJson "empty" ("" :: String) >>= (`shouldEqual` (Other 204))

      it "responds with 415 if the requested media type is unsupported" $ do
        post "/" (Map.fromList [("Content-Type",["application/nonsense"])])
          >>= (`shouldEqual` (Other 415))

type PutApi =
       ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "empty" :> Put '[JSON] ()

putApi :: Proxy PutApi
putApi = Proxy

putServer :: Server PutApi AppHandler
putServer = return . age :<|> return . age :<|> return ()

putSpec :: Spec
putSpec = snap (route (routes putApi pServer)) app $ do
  describe "Servant.API.Put and .ReqBody" $ do
      let putJson x v = put' x (T.decodeUtf8 . BL.toStrict $ encode v) (Map.fromList [("Content-Type" , ["application/json;charset=utf-8"])])
          putJson' x v = put' x (T.decodeUtf8 . BL.toStrict $ encode v) (Map.fromList [("Content-Type" , ["application/nonsense"])])

      it "allows to put a Person" $ do
        putJson "/" alice >>= shouldHaveText "42"
          --matchStatus = 200

      it "allows alternative routes if all have request bodies" $ do
        putJson "/bla" alice >>= shouldHaveText "42"

      it "handles trailing '/' gracefully" $ do
        putJson "/bla/" alice >>= shouldHaveText "42"

      it "correctly rejects invalid request bodies with status 400" $ do
        putJson "/" ("some invalid body" :: String) >>= (`shouldEqual` (Other 400))

      it "returns 204 if the type is '()'" $ do
        putJson "empty" ("" :: String) >>= (`shouldEqual` (Other 204))

      it "responds with 415 if the requested media type is unsupported" $ do
        putJson' "/" ("anything at all" :: String) >>= (`shouldEqual` (Other 415))



{-
type PatchApi =
       ReqBody '[JSON] Person :> Patch '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Patch '[JSON] Integer
  :<|> "empty" :> Patch '[] ()

patchApi :: Proxy PatchApi
patchApi = Proxy

patchServer :: Server PatchApi AppHandler
patchServer = return . age :<|> return . age :<|> return ()


patch :: (ToJSON v) => T.Text -> v -> SnapHspecM b TestResponse
patch r v = patch' r (T.decodeUtf8 . BL.fromStrict $ encode v) (Map.fromList ["Content-Type", ["application/json;charset=utf-8"]])

patch' :: T.Text -> T.Text -> Params -> SnapHspecM b TestResponse
patch' = -- TODO, need to build support for building patch requests into Snap.Core and Test.Hspec.Snap

patchSpec :: Spec
patchSpec = snap (route (routes patchApi patchServer)) app $ do
  describe "Servant.API.Patch and .ReqBody" $ do
      let patch' x = Test.Hspec.Wai.request methodPatch x [(hContentType
                                                        , "application/json;charset=utf-8")]

      it "allows to patch a Person" $ do
        patch' "/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "allows alternative routes if all have request bodies" $ do
        patch' "/bla" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "handles trailing '/' gracefully" $ do
        patch' "/bla/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "correctly rejects invalid request bodies with status 400" $ do
        patch' "/" "some invalid body" `shouldRespondWith` 400

      it "returns 204 if the type is '()'" $ do
        patch' "empty" "" `shouldRespondWith` ""{ matchStatus = 204 }

      it "responds with 415 if the requested media type is unsupported" $ do
        let patch'' x = Test.Hspec.Wai.request methodPatch x [(hContentType
                                                            , "application/nonsense")]
        patch'' "/" "anything at all" `shouldRespondWith` 415
-}

type HeaderApi a = Header "MyHeader" a :> Put '[JSON] ()
headerApi :: Proxy (HeaderApi a)
headerApi = Proxy


headerSpec :: Spec
headerSpec = do

  let expectsInt :: Maybe Int -> AppHandler ()
      --expectsInt :: Server HeaderApi
      expectsInt (Just x) = when (x /= 5) $ error "Expected 5"
      expectsInt Nothing  = error "Expected an int"

  let expectsString :: Maybe String -> AppHandler ()
      expectsString (Just x) = when (x /= "more from you") $ error "Expected more from you"
      expectsString Nothing  = error "Expected a string"

  snap (route (routes headerApi (expectsInt))) app $ do
    describe "Servant.API.Header" $ do
        let post' x = post x (params [("MyHeader" ,"5")])

        it "passes the header to the handler (Int)" $
            post' "/" >>= (`shouldEqual` (Other 204))

  snap (route (routes headerApi (expectsString))) app $ do
        let post' x = post x (params [("MyHeader" ,"more from you")])

        it "passes the header to the handler (String)" $
            post' "/"  >>= (`shouldEqual` (Other 204))


type RawApi = "foo" :> Raw
rawApi :: Proxy RawApi
rawApi = Proxy
--rawApplication :: Show a => (Request -> a) -> Application
--rawApplication f request_ respond = respond $ responseLBS ok200 [] (cs $ show $ f request_)
rawApplication :: Server Raw AppHandler
rawApplication = do
  b <- readRequestBody 1000
  writeBS (BL.toStrict b)

rawSpec :: Spec
rawSpec = snap (route (routes rawApi rawApplication)) app $ do
  describe "Servant.API.Raw" $ do
    it "runs applications" $ do
      get "foo/42" >>= shouldHaveText "42"
      -- (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
      --   response <- Network.Wai.Test.request defaultRequest{
      --     pathInfo = ["foo"]
      --    }
      --   liftIO $ do
      --     simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      get "foo/bar" >>= shouldHaveText "bar"
      -- (flip runSession) (serve rawApi (rawApplication pathInfo)) $ do
      --   response <- Network.Wai.Test.request defaultRequest{
      --     pathInfo = ["foo", "bar"]
      --    }
      --   liftIO $ do
      --     simpleBody response `shouldBe` cs (show ["bar" :: String])

type AlternativeApi =
       "foo" :> Get '[JSON] Person
  :<|> "bar" :> Get '[JSON] Animal
  :<|> "foo" :> Get '[PlainText] T.Text
  :<|> "bar" :> Post '[JSON] Animal
  :<|> "bar" :> Put '[JSON] Animal
  :<|> "bar" :> Delete '[JSON] ()
unionApi :: Proxy AlternativeApi
unionApi = Proxy

unionServer :: Server AlternativeApi AppHandler
unionServer =
       return alice
  :<|> return jerry
  :<|> return "a string"
  :<|> return jerry
  :<|> return jerry
  :<|> return ()

unionSpec :: Spec
unionSpec = snap (route (routes unionApi unionServer)) app $ do
  describe "Servant.API.Alternative" $ do

      it "unions endpoints" $ do
        get "/foo" >>= (`shouldDecodeTo` alice)
        -- response <- get "/foo"
        -- liftIO $ do
        --   decode' (simpleBody response) `shouldBe`
        --     Just alice
        get "/bar" >>= (`shouldDecodeTo` jerry)
        -- response_ <- get "/bar"
        -- liftIO $ do
        --   decode' (simpleBody response_) `shouldBe`
        --     Just jerry

      it "checks all endpoints before returning 415" $ do
        get "/foo" >>= should200 -- `shouldRespondWith` 200

      it "returns 404 if the path does not exist" $ do
        get "/nonexistent" >>= should404 -- `shouldRespondWith` 404

type ResponseHeadersApi =
       Get   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Post  '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Put   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Patch '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)

rhApi :: Proxy ResponseHeadersApi
rhApi = Proxy

rhServer :: Server ResponseHeadersApi AppHandler
rhServer = let h = return $ addHeader 5 $ addHeader "kilroy" "hi"
  in h :<|> h :<|> h :<|> h


responseHeadersSpec :: Spec
responseHeadersSpec = snap (route (routes rhApi rhServer)) app $ do
  describe "ResponseHeaders" $ do

    let methods = [(get,  should200)
                  ,(flip post Map.empty, (`shouldEqual` (Other 202)))
                  ,(flip put  Map.empty, (`shouldEqual` (Other 200)))
                  ]

    it "includes the headers in the response" $
      forM_ methods $ \(method, expected) ->
        method "/" >>= expected

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \(method,_) ->
        method "blahblah" >>= should404

    -- TODO: bring this test  back eventually
    -- (need to be able to add headers to requests in the list)
    -- it "returns 415 if the Accept header is not supported" $
    --   forM_ methods $ \(method,_) ->
    --     Test.Hspec.Wai.request method "" [(hAccept, "crazy/mime")] ""
    --       `shouldRespondWith` 415

type PrioErrorsApi = ReqBody '[JSON] Person :> "foo" :> Get '[JSON] Integer

prioErrorsApi :: Proxy PrioErrorsApi
prioErrorsApi = Proxy

peServer :: Server PrioErrorsApi AppHandler
peServer = return . age

-- | Test the relative priority of error responses from the server.
--
-- In particular, we check whether matching continues even if a 'ReqBody'
-- or similar construct is encountered early in a path. We don't want to
-- see a complaint about the request body unless the path actually matches.
--
prioErrorsSpec :: Spec
prioErrorsSpec = snap (route (routes prioErrorsApi peServer)) app $ do

  describe "PrioErrors" $ do

    let --check :: T.Text
        --      -> (String, [String], BL.ByteString)
        --      -> TestResponse
        --      -> SnapHspecM () TestResponse
        check path (cdescr, ctype, body) resp =
          it fulldescr $
            put' path body (params [("Content-Type", ctype)]) >>= (`shouldEqual` resp)
          where
            fulldescr = "returns " ++ show resp ++ " on PUT"
                     ++ " " ++ cs path ++ " (" ++ cdescr ++ ")"

        txt   = ("text"        , "text/plain;charset=utf8"      , "42"        )
        ijson = ("invalid json", "application/json;charset=utf8", "invalid"   )
        vjson = ("valid json"  , "application/json;charset=utf8",
                 T.decodeUtf8 . BL.toStrict $ encode alice)

    check "/"    txt   (Other 404)
    check "/bar" txt   (Other 404)
    check "/foo" txt   (Other 405)
    check "/"    ijson (Other 404)
    check "/bar" ijson (Other 404)
    check "/foo" ijson (Other 405)
    check "/"    vjson (Other 404)
    check "/bar" vjson (Other 404)
    check "/foo" vjson (Other 405)

-- | Test server error functionality.
errorsSpec :: Spec
errorsSpec = do
  let he = HttpError (Status 409 "Confict") (Just "A custom error")
  let ib = InvalidBody "The body is invalid"
  let wm = WrongMethod
  let nf = NotFound

  describe "Servant.Server.Internal.RouteMismatch" $ do
    it "HttpError > *" $ do
      ib <> he `shouldBe` he
      wm <> he `shouldBe` he
      nf <> he `shouldBe` he

      he <> ib `shouldBe` he
      he <> wm `shouldBe` he
      he <> nf `shouldBe` he

    it "HE > InvalidBody > (WM,NF)" $ do
      he <> ib `shouldBe` he
      wm <> ib `shouldBe` ib
      nf <> ib `shouldBe` ib

      ib <> he `shouldBe` he
      ib <> wm `shouldBe` ib
      ib <> nf `shouldBe` ib

    it "HE > IB > WrongMethod > NF" $ do
      he <> wm `shouldBe` he
      ib <> wm `shouldBe` ib
      nf <> wm `shouldBe` wm

      wm <> he `shouldBe` he
      wm <> ib `shouldBe` ib
      wm <> nf `shouldBe` wm

    it "* > NotFound" $ do
      he <> nf `shouldBe` he
      ib <> nf `shouldBe` ib
      wm <> nf `shouldBe` wm

      nf <> he `shouldBe` he
      nf <> ib `shouldBe` ib
      nf <> wm `shouldBe` wm
-}
