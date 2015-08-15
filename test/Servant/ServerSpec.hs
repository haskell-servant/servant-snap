{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}

module Servant.ServerSpec where


import           Control.Monad              (forM_, when)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT, left)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Conversion ()
import           Data.Char                  (toUpper)
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           GHC.Generics               (Generic)
--import           Network.HTTP.Types         (hAccept, hContentType,
--                                             methodDelete, methodGet,
--                                             methodPatch, methodPost, methodPut,
--                                             parseQuery, status409)
--import           Network.Wai                (Application, Request, pathInfo,
--                                             queryString, rawQueryString,
--                                             responseLBS)
import           Snap.Core                  hiding (Headers, addHeader)
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Test                  hiding (with, get, addHeader, put)
--import           Network.Wai.Test           (defaultRequest, request,
--                                             runSession, simpleBody)
import           Test.Hspec
import           Test.Hspec.Core.Spec (Result(..))
-- import           Test.Hspec.Wai             (get, liftIO, matchHeaders,
--                                              matchStatus, post, request,
--                                              shouldRespondWith, with, (<:>))
import           Test.Hspec.Snap            hiding (NotFound)
import           Servant.API                ((:<|>) (..), (:>),
                                             addHeader, Capture,
                                             Delete, Get, Header (..), Headers,
                                             JSON, MatrixFlag, MatrixParam,
                                             MatrixParams, Patch, PlainText,
                                             Post, Put, QueryFlag, QueryParam,
                                             QueryParams, Raw, ReqBody)
import           Servant.Server             (Server, serve, ServantErr(..), err404)
import           Servant.Server.Internal    (HasServer)
import           Servant.Server.Internal.SnapShims
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.PathInfo (pathInfo)

import Debug.Trace


-- * test data types

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

data App = App

app :: SnapletInit App App
app = makeSnaplet "servantsnap" "A test app for servant-snap" Nothing $ do
  --wrapSite (\site -> applicationToSnap (serve captureApi captureServer) )
  return App

routes :: HasServer api
       => Proxy api
       -> Server api (Handler App App)
       -> [(B8.ByteString, Handler App App ())]
routes p s = [("", applicationToSnap (serve p s))]

-- * specs

spec :: Spec
spec = do
  captureSpec
  getSpec
  postSpec
  putSpec
  -- patchSpec
  queryParamSpec
  matrixParamSpec
  headerSpec
  rawSpec
  unionSpec
  -- prioErrorsSpec
  errorsSpec
  -- responseHeadersSpec

traceShow' a = traceShow a a

type CaptureApi = Capture "legs" Integer :> Get '[JSON] Animal

captureApi :: Proxy CaptureApi
captureApi = Proxy

captureServer :: Integer -> EitherT ServantErr (Handler App App) Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> left err404

type CaptureApi2 = Capture "captured" String :> Raw (Handler App App) (Handler App App ())
captureApi2 :: Proxy CaptureApi2
captureApi2 = Proxy

captureServer2 :: String  -> Server (Raw a (Handler App App ())) (Handler App App)
captureServer2 _ = lift $ do
  r <- getRequest
  writeBS (rqPathInfo r)

captureSpec :: Spec
captureSpec = do
  snap (route (routes captureApi captureServer)) app $ do
   describe "Servant.API.Capture" $ do

      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        case response of
          Json bs -> do
            let d = decode' bs
            d `shouldEqual` Just tweety
          _       -> setResult (Fail "Should have been json body")

      it "returns 404 if the decoding fails" $ do
        get "/notAnInt" >>= should404

  snap (route (routes captureApi2 captureServer2)) app $ do
    describe "Servant.API.Capture" $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" >>= shouldEqual (Html "foo")


type GetApi = Get '[JSON] Person
        :<|> "empty" :> Get '[] ()
getApi :: Proxy GetApi
getApi = Proxy

should405 :: TestResponse -> SnapHspecM b ()
should405 (Html _) = setResult (Fail "Should have failed, got HTML")
should405 (Other 405) = setResult Success
should405 _ = setResult (Fail "Should have 405'd")

getSpec :: Spec
getSpec = snap (route (routes getApi (return alice :<|> return ()))) app $ do
  describe "Servant.API.Get" $ do

      it "allows to GET a Person" $ do
        response <- get "/"
        case response of
          Json bs -> do
            decode' bs `shouldEqual`  (Just alice)
          _ -> setResult (Fail "Should have been json body")

      it "throws 405 (wrong method) on POSTs" $ do
        postJson "/" ("" :: String) >>= should405
        postJson "/empty" ("" :: String) >>= should405

      it "returns 204 if the type is '()'" $ do
        get "empty" >>= shouldEqual (Other 204) --`shouldRespondWith` ""{ matchStatus = 204 }

      it "returns 415 if the Accept header is not supported" $ do
        get' "" (Map.fromList [("Accept", ["crazy/mime"])]) >>= shouldEqual (Other 415)



type QueryParamApi = QueryParam "name" String :> Get '[JSON] Person
                :<|> "a" :> QueryParams "names" String :> Get '[JSON] Person
                :<|> "b" :> QueryFlag "capitalize" :> Get '[JSON] Person

queryParamApi :: Proxy QueryParamApi
queryParamApi = Proxy

qpServer :: Server QueryParamApi (Handler App App)
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
            Json bs ->
              decode' bs `shouldEqual` (Just $ alice{name = "bob"})
            _ -> setResult (Fail "Should have been json body")

      it "allows to retrieve lists in GET parameters" $ do
          let params2 = "?names[]=bob&names[]=john"
          response2 <- get ("a" <> params2) --defaultRequest{
            --rawQueryString = params2,
            --queryString = parseQuery params2,
            --pathInfo = ["a"]
            --}
          case response2 of
            Json bs ->
              decode' bs `shouldEqual` Just alice{name="john"}
            _ -> setResult  (Fail "Should have been json body")

      it "allows to retrieve value-less GET parameters" $ do
          response3 <- get "b?capitalize"
           --  rawQueryString = params3,
           --  queryString = parseQuery params3,
           --  pathInfo = ["b"]
           -- }
          case response3 of
            Json bs ->
              decode' bs `shouldEqual` Just alice{name="ALICE"}
            _ -> setResult  (Fail "Should have been json body")

      it "allows to retrieve value-less GET parameters again" $ do -- TODO rename
          response3' <- get "b?capitalize"
           --  rawQueryString = params3,
           --  queryString = parseQuery params3,
           --  pathInfo = ["b"]
           -- }
          case response3' of
            Json bs ->
              decode' bs `shouldEqual` Just alice{name="ALICE"}
            _ -> setResult  (Fail "Should have been json body")

      it "allows to retrieve value-less GET parameters again" $ do -- TODO rename
          response3'' <- get "b?unknown="
           --  rawQueryString = params3,
           --  queryString = parseQuery params3,
           --  pathInfo = ["b"]
           -- }
          case response3'' of
            Json bs ->
              decode' bs `shouldEqual` Just alice{name="Alice"}
            _ -> setResult  (Fail "Should have been json body")



type MatrixParamApi = "a" :> MatrixParam "name" String :> Get '[JSON] Person
                :<|> "b" :> MatrixParams "names" String :> "bsub" :> MatrixParams "names" String :> Get '[JSON] Person
                :<|> "c" :> MatrixFlag "capitalize" :> Get '[JSON] Person
                :<|> "d" :> Capture "foo" Integer :> MatrixParam "name" String :> MatrixFlag "capitalize" :> "dsub" :> Get '[JSON] Person

matrixParamApi :: Proxy MatrixParamApi
matrixParamApi = Proxy

mpServer :: Server MatrixParamApi (Handler App App)
mpServer = matrixParamServer :<|> mpNames :<|> mpCapitalize alice :<|> mpComplex
  where mpNames (_:name2:_) _ = return alice { name = name2 }
        mpNames _           _ = return alice

        mpCapitalize p False = return p
        mpCapitalize p True  = return p { name = map toUpper (name p) }

        matrixParamServer (Just name) = return alice{name = name}
        matrixParamServer Nothing = return alice

        mpAge age p = return p { age = age }
        mpComplex capture name cap = matrixParamServer name >>= flip mpCapitalize cap >>= mpAge capture

shouldDecodeTo (Json bs) v = decode' bs `shouldEqual` Just v
shouldDecodeTo _         _ = setResult (Fail "Should have been json body")

matrixParamSpec :: Spec
matrixParamSpec = snap (route (routes matrixParamApi mpServer)) app $ do
  describe "Servant.API.MatrixParam" $ do
      it "allows to retrieve simple matrix parameters" $ do
          get "a;name=bob" >>= (`shouldDecodeTo` alice {name="bob"})

      it "allows to retrieve lists in matrix parameters" $
          get "b;names=bob;names=john/bsub;names=anna;names=sarah" >>= (`shouldDecodeTo` alice{name="john"})

      it "allows to retrieve value-less matrix parameters" $
          get "c;capitalize" >>= (`shouldDecodeTo` alice{name="ALICE"})

      it "allows to retrieve value-less matrix parameters" $
          get "c;capitalize=" >>= (`shouldDecodeTo` alice{name="ALICE"})

      it "allows to retrieve matrix parameters on captured segments" $
         get "d/12;name=stephen;capitalize/dsub" >>= (`shouldDecodeTo` alice{name="STEPHEN"})

      it "allows to retrieve matrix parameters on captured segments" $
         get "d;ignored=1/5/dsub" >>= (`shouldDecodeTo` alice{name="STEPHEN"})

type PostApi =
       ReqBody '[JSON] Person :> Post '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Post '[JSON] Integer
  :<|> "empty" :> Post '[] ()

postApi :: Proxy PostApi
postApi = Proxy

pServer = return . age :<|> return . age :<|> return ()

postSpec :: Spec
postSpec = snap (route (routes postApi pServer)) app $ do
  describe "Servant.API.Post and .ReqBody" $ do

      it "allows to POST a Person" $ do
        postJson "/" alice >>= (`shouldEqual` (Html "42"))
        --myPost "/" (encode alice) `shouldRespondWith` "42"{
        --  matchStatus = 201
        -- }

      it "allows alternative routes if all have request bodies" $ do
        postJson "/bla" alice >>= (`shouldEqual` (Html "42"))
        -- myPost "/bla" (encode alice) `shouldRespondWith` "42"{
        --   matchStatus = 201
        --  }

      it "handles trailing '/' gracefully" $ do
        postJson "/bla/" alice >>= (`shouldEqual` (Html "42"))
        -- myPost "/bla/" (encode alice) `shouldRespondWith` "42"{
        --   matchStatus = 201
        --  }

      it "correctly rejects invalid request bodies with status 400" $ do
        postJson "/" ("some invalid body" :: String) >>= (`shouldEqual`  (Other 400))

      it "returns 204 if the type is '()'" $ do
        postJson "empty" ("" :: String) >>= (`shouldEqual` (Other 204))

      it "responds with 415 if the requested media type is unsupported" $ do
        post "/" (Map.fromList [("Content-Type",["application/nonsense"])]) >>= (`shouldEqual` (Other 415))

type PutApi =
       ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "empty" :> Put '[] ()

putApi :: Proxy PutApi
putApi = Proxy

putServer :: Server PutApi (Handler App App)
putServer = return . age :<|> return . age :<|> return ()

putSpec :: Spec
putSpec = snap (route (routes putApi pServer)) app $ do
  describe "Servant.API.Put and .ReqBody" $ do
      let putJson x v = put' x (T.decodeUtf8 . BL.toStrict $ encode v) (Map.fromList [("Content-Type" , ["application/json;charset=utf-8"])])
          putJson' x v = put' x (T.decodeUtf8 . BL.toStrict $ encode v) (Map.fromList [("Content-Type" , ["application/nonsense"])])

      it "allows to put a Person" $ do
        putJson "/" alice >>= (`shouldEqual` (Html "42")) --`shouldRespondWith` "42"{
          --matchStatus = 200
         --}

      it "allows alternative routes if all have request bodies" $ do
        putJson "/bla" alice >>= (`shouldEqual` (Html "42"))

      it "handles trailing '/' gracefully" $ do
        putJson "/bla/" alice >>= (`shouldEqual` (Html "42"))

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

patchServer :: Server PatchApi (Handler App App)
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

  let expectsInt :: Maybe Int -> EitherT ServantErr (Handler App App) ()
      --expectsInt :: Server HeaderApi
      expectsInt (Just x) = when (x /= 5) $ error "Expected 5"
      expectsInt Nothing  = error "Expected an int"

  let expectsString :: Maybe String -> EitherT ServantErr (Handler App App) ()
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


type RawApi = "foo" :> Raw (Handler App App) (Handler App App ())
rawApi :: Proxy RawApi
rawApi = Proxy
--rawApplication :: Show a => (Request -> a) -> Application
--rawApplication f request_ respond = respond $ responseLBS ok200 [] (cs $ show $ f request_)
rawApplication :: Server (Raw (Handler App App) (Handler App App ())) (Handler App App)
rawApplication = lift $ do
  b <- readRequestBody 1000
  writeBS (BL.toStrict b)

rawSpec :: Spec
rawSpec = snap (route (routes rawApi rawApplication)) app $ do
  describe "Servant.API.Raw" $ do
    it "runs applications" $ do
      get "foo/42" >>= (`shouldEqual` (Html "42"))
      -- (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
      --   response <- Network.Wai.Test.request defaultRequest{
      --     pathInfo = ["foo"]
      --    }
      --   liftIO $ do
      --     simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      get "foo/bar" >>= (`shouldEqual` (Html "bar"))
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

unionServer :: Server AlternativeApi (Handler App App)
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

rhServer :: Server ResponseHeadersApi (Handler App App)
rhServer = let h = return $ addHeader 5 $ addHeader "kilroy" "hi"
  in h :<|> h :<|> h :<|> h


responseHeadersSpec :: Spec
responseHeadersSpec = snap (route (routes rhApi rhServer)) app $ do
  describe "ResponseHeaders" $ do

    let methods = [(get,  should200)
                  ,(flip post Map.empty, (`shouldEqual` (Other 202)))
                  ,(flip put  Map.empty, (`shouldEqual` (Other 200)))
                  --,(methodPatch, 200)
                  ]
        expectedHeaders = params [("H1","H2")]

    it "includes the headers in the response" $
      forM_ methods $ \(method, expected) ->
        method "/" >>= expected -- (`shouldEqual` expected)
          --`shouldRespondWith` "\"hi\""{ matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
          --                            , matchStatus  = expected
          --                            }

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \(method,_) ->
        method "blahblah" >>= should404
          --`shouldRespondWith` 404

    -- TODO: bring this test  back eventually
    -- (need to be able to add headers to requests in the list)
    -- it "returns 415 if the Accept header is not supported" $
    --   forM_ methods $ \(method,_) ->
    --     Test.Hspec.Wai.request method "" [(hAccept, "crazy/mime")] ""
    --       `shouldRespondWith` 415

{-
type PrioErrorsApi = ReqBody '[JSON] Person :> "foo" :> Get '[JSON] Integer

prioErrorsApi :: Proxy PrioErrorsApi
prioErrorsApi = Proxy

-- | Test the relative priority of error responses from the server.
--
-- In particular, we check whether matching continues even if a 'ReqBody'
-- or similar construct is encountered early in a path. We don't want to
-- see a complaint about the request body unless the path actually matches.
--
prioErrorsSpec :: Spec
prioErrorsSpec = describe "PrioErrors" $ do
  let server = return . age
  with (return $ serve prioErrorsApi server) $ do
    let check (mdescr, method) path (cdescr, ctype, body) resp =
          it fulldescr $
            Test.Hspec.Wai.request method path [(hContentType, ctype)] body
              `shouldRespondWith` resp
          where
            fulldescr = "returns " ++ show (matchStatus resp) ++ " on " ++ mdescr
                     ++ " " ++ cs path ++ " (" ++ cdescr ++ ")"

        get' = ("GET", methodGet)
        put' = ("PUT", methodPut)

        txt   = ("text"        , "text/plain;charset=utf8"      , "42"        )
        ijson = ("invalid json", "application/json;charset=utf8", "invalid"   )
        vjson = ("valid json"  , "application/json;charset=utf8", encode alice)

    check get' "/"    txt   404
    check get' "/bar" txt   404
    check get' "/foo" txt   415
    check put' "/"    txt   404
    check put' "/bar" txt   404
    check put' "/foo" txt   405
    check get' "/"    ijson 404
    check get' "/bar" ijson 404
    check get' "/foo" ijson 400
    check put' "/"    ijson 404
    check put' "/bar" ijson 404
    check put' "/foo" ijson 405
    check get' "/"    vjson 404
    check get' "/bar" vjson 404
    check get' "/foo" vjson 200
    check put' "/"    vjson 404
    check put' "/bar" vjson 404
    check put' "/foo" vjson 405

-}

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
