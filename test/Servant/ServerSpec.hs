{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Servant.ServerSpec where


import           Control.Monad              (forM_, when)
import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Conversion ()
import           Data.Char                  (toUpper)
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           GHC.Generics               (Generic)
import           Snap.Core                  hiding (Headers, addHeader)
import           Snap.Snaplet
import           Test.Hspec
import           Test.Hspec.Core.Spec (Result(..))
import           Test.Hspec.Snap            hiding (NotFound)
import           Servant.API                ((:<|>) (..), (:>),
                                             addHeader, Capture,
                                             Header (..), Headers,
                                             JSON, 
                                             PlainText,
                                             QueryFlag, QueryParam,
                                             QueryParams, Raw, ReqBody)
import           Servant.API.Verbs
import           Servant.Server             (Server, serve)
import           Servant.Server.Internal    (HasServer)
import           Servant.Server.Internal.SnapShims
import           Servant.Server.Internal.RoutingApplication

import Debug.Trace


-- * test data types

type AppHandler = Handler App App

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
  return App

routes :: HasServer api
       => Proxy api
       -> Server api AppHandler
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
  headerSpec
  rawSpec
  unionSpec
  prioErrorsSpec
  errorsSpec
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
        :<|> "empty" :> Get '[] ()
getApi :: Proxy GetApi
getApi = Proxy

should405 :: TestResponse -> SnapHspecM b ()
should405 (Html _ _) = setResult (Fail Nothing "Should have failed, got HTML")
should405 (Other 405) = setResult Success
should405 _ = setResult (Fail Nothing "Should have 405'd")

getSpec :: Spec
getSpec = snap (route (routes getApi (return alice :<|> return ()))) app $ do
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
  :<|> "empty" :> Post '[] ()

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
  :<|> "empty" :> Put '[] ()

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
         --}

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
