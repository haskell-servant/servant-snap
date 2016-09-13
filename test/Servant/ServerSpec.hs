{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Servant.ServerSpec where


-------------------------------------------------------------------------------
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.State.Class  as State
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import           Data.CaseInsensitive       (mk)
import           Data.Char                  (toUpper)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.Encoding    as TL
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (hAccept, hContentType)
import qualified Network.HTTP.Types
import           Snap.Core                  hiding (Headers, addHeader)
import qualified Snap.Core                  as SC
import           Snap.Snaplet
import qualified Snap.Snaplet.Test          as SST
import qualified Snap.Test                  as ST
-------------------------------------------------------------------------------
import           Test.Hspec
import           Test.Hspec.Snap            hiding (NotFound)
import qualified Test.Hspec.Snap            as THS
import qualified Test.HUnit                 as HU
import           Servant.API                ((:<|>) (..), (:>),
                                             addHeader, Capture,
                                             CaptureAll, Header (..), Headers,
                                             IsSecure(..), JSON, NoContent(..),
                                             PlainText, QueryFlag, QueryParam,
                                             QueryParams, Raw, RemoteHost,
                                             ReqBody)
import           Servant.API.Verbs          (Verb, Get, Post, Put, Delete,
                                             Patch)
import qualified Servant.API.Verbs          as V
import           Servant.Server             hiding (route)
import           Servant.Server.Internal    (HasServer)

-------------------------------------------------------------------------------
-- * test data types

data App = App
type AppHandler = Handler App App

app :: SnapletInit App App
app = app' []

app' :: [(B8.ByteString, AppHandler ())] -> SnapletInit App App
app' rs = makeSnaplet "servantsnap" "A test app for servant-snap" Nothing $ do
  addRoutes rs
  return App
-------------------------------------------------------------------------------


-- * Specs

spec :: Spec
spec = do
  verbSpec
  captureSpec
  captureAllSpec
  queryParamSpec
  reqBodySpec
  headerSpec
  rawSpec
  alternativeSpec
  responseHeadersSpec
  miscCombinatorSpec

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
      test (desc :: String) api verbRoutes (method :: SC.Method) (status :: Int) =
       describe ("Servant.API.Verb " ++ desc) $ do

        -- This group is run with hspec-snap
        snap (route verbRoutes) app $ do

          -- HEAD should not return body
          when (method == SC.HEAD) $
            it "HEAD returns no content body" $ do
              response <- runRequest $ mkRequest method  "/" "" [] ""
              liftIO $ bodyIs response "" `shouldBe` True

          it "throws 405 on wrong method " $ do
            response <- runRequest $ mkRequest (wrongMethod method) "/" "" [] ""
            liftIO $ statusIs response 405 `shouldBe` True

          it "handles trailing '/' gracefully" $ do
            response <- runRequest $ mkRequest method "/headerNC/" "" [] ""
            liftIO $ statusIs response status `shouldBe` True

          it "returns 406 if the Accept header is not supported" $ do
            response <- runRequest $ mkRequest method "" ""
                                     [(hAccept, "crazy/mime")] ""
            liftIO $ statusIs response 406  `shouldBe` True

          it "responds if the Accept header is supported" $ do
            response <- runRequest $ mkRequest method "" ""
               [(hAccept, "application/json")] ""
            liftIO $ statusIs response status `shouldBe` True

        let sInit = app' verbRoutes
            runUrl p = SST.runHandler Nothing
                       (mkRequest method p "" [] "")
                       (serveSnap api server) sInit

        -- This group is run with hspec directly

          -- HEAD and 214/215 need not return bodies
        unless (status `elem` [214, 215] || method == SC.HEAD) $
          it "returns the person" $ do
             resp <- runUrl "/" 
             resp `shouldDecodeTo` alice
             resp `shouldHaveStatus` status

        it "returns no content on NoContent" $ do
          resp <- runUrl "/noContent"
          resp `shouldHaveStatus` status
          resp `shouldHaveBody` ""

        it "returs headers" $ do
          resp <- SST.runHandler Nothing
                  (mkRequest method "/header" "" [] "")
                  (serveSnap api server) sInit
          shouldHaveHeaders resp  [("H","5")]

        it "sets the content-type header" $ do
          resp <- SST.runHandler Nothing (mkRequest method "" "" [] "")
                  (serveSnap api server) sInit
          resp `shouldHaveHeaders` [("Content-Type", "application/json")]

        unless (status `elem` [214, 215] || method == SC.HEAD) $
          it "allows modular specification of supported content types" $ do
            resp <- SST.runHandler Nothing
                    (mkRequest method "/accept" ""
                     [(hAccept, "text/plain")] "")
                    (serveSnap api server) sInit
            resp `shouldHaveStatus` status
            resp `shouldHaveBody` "B"

  test "GET 200" get200 (routes get200 server) SC.GET 200
  test "POST 210" post210 (routes post210 server) SC.POST 210
  test "PUT 203" put203 (routes put203 server) SC.PUT 203
  test "DELETE 280" delete280 (routes delete280 server) SC.DELETE  280
  test "PATCH 214" patch214 (routes patch214 server) SC.PATCH 214
  test "GET 200 with HEAD" get200 (routes get200 server) SC.HEAD 200

-- }}}

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

type CaptureApi2 = Capture "captured" String :> Raw

captureApi2 :: Proxy CaptureApi2
captureApi2 = Proxy

captureServer2 :: Server CaptureApi2 AppHandler
captureServer2 _ = do
  rq <- getRequest
  writeBS (SC.rqPathInfo rq) 

captureSpec :: Spec
captureSpec = do -- snap (route (routes captureApi captureServer)) app $  

  let ( sInit , _ ) = mkInitAndServer captureApi captureServer
      ( sInit2, _ ) = mkInitAndServer captureApi2 captureServer2
      runReq r method api serv i =
        SST.runHandler Nothing (mkRequest method r "" [] "")
        (serveSnap api serv) i

  describe "Servant.API.Capture" $ do

      it "can capture parts of the 'pathInfo'" $ do
        runReq "/2" SC.GET captureApi captureServer sInit >>=
          (`shouldDecodeTo` tweety)

      it "returns 400 if the decoding fails" $ do
        runReq "/notAnInt" SC.GET captureApi captureServer sInit >>=
          (`shouldHaveStatus` 400)

      it "strips the captured path snippet from pathInfo" $ do
        runReq "/captured/foo" SC.GET captureApi2 captureServer2 sInit2 >>=
          (`shouldHaveBody` "foo")

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
captureAllSpec = do
  describe "Servant.API.CaptureAll" $ do

      let runUrl u = runReqOnApi captureAllApi  captureAllServer SC.GET u "" [] ""
      it "can capture a single element of the 'pathInfo'" $ do
        runUrl "/2" >>= (`shouldDecodeTo` tweety)
        -- liftIO $ decodesTo response tweety `shouldBe` True

      it "can capture multiple elements of the 'pathInfo'" $ do
        runUrl "/2/2" >>= (`shouldDecodeTo` jerry)
        -- liftIO $ decodesTo response jerry `shouldBe` True

      it "can capture arbitrarily many elements of the 'pathInfo'" $ do
        runUrl "/1/1/0/1/0/1" >>= (`shouldDecodeTo` jerry)
        -- liftIO $ decodesTo response jerry `shouldBe` True

      it "can capture when there are no elements in 'pathInfo'" $ do
        runUrl "/" >>= (`shouldDecodeTo` beholder)
        -- liftIO $ decodesTo response jerry `shouldBe` True

      it "returns 400 if the decoding fails" $ do
        runUrl "/notAnInt" >>= (`shouldHaveStatus` 400)
        -- liftIO $ statusIs response 400 `shouldBe` True

      it "returns 400 if the decoding fails, regardless of which element" $ do
        runUrl "/1/0/0/notAnInt/3/" >>= (`shouldHaveStatus` 400)
        -- liftIO $ statusIs response 400 `shouldBe` True

      it "returns 400 if the decoding fails, even when it's multiple elements" $ do
        runUrl "/1/0/0/notAnInt/3/orange/" >>= (`shouldHaveStatus` 400)
        -- liftIO $ statusIs response 400 `shouldBe` True

      it "consumes everything from pathInfo" $ do
        let api' = (Proxy :: Proxy (CaptureAll "segments" String :> Raw))
            srv' = (\_ -> getRequest >>= writeBS . rqPathInfo)
        req <- runReqOnApi api' srv' SC.GET "/captured/foo/bar/baz" "" [] ""
        req `shouldHaveBody` ""

        -- (\ _captured request_ respond ->
        --     respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
        -- get "/captured/foo/bar/baz" `shouldRespondWith` (fromString (show ([] :: [Int])))


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

queryParamSpec :: Spec
queryParamSpec = do
  describe "Servant.API.QueryParam" $ do

      let runTest :: B8.ByteString -> B8.ByteString -> IO (Either T.Text Response)
          runTest p qs = runReqOnApi queryParamApi qpServer SC.GET p qs [(hContentType,"application/json")] ""

      it "allows retrieving simple GET parameters" $
        runTest "" "?name=bob" >>= (`shouldDecodeTo` alice {name="bob"})

{-
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
-}

      it "allows retrieving lists in GET parameters" $
        runTest "a" "?names[]=bob&names[]=john" >>= (`shouldDecodeTo` alice{name="john"})

{-
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
-}


      it "allows retrieving value-less GET parameters" $ do
        runTest "b" "?capitalize" >>= (`shouldDecodeTo` alice{name="ALICE"})

{-
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
-}

          -- let params3' = "?capitalize="
        runTest "b" "?capitalize=" >>= (`shouldDecodeTo` alice{name="ALICE"})
{-
          response3' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3',
            queryString = parseQuery params3',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "ALICE"
             }
-}

          -- let params3'' = "?unknown="
        runTest "b" "?unknown=" >>= (`shouldDecodeTo` alice{name="Alice"})
{-
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

reqBodyApi :: Proxy ReqBodyApi
reqBodyApi = Proxy

reqBodySpec :: Spec
reqBodySpec = do
  describe "Servant.API.ReqBody" $ do

    let runTest m p ct bod = runReqOnApi reqBodyApi server m p "" [(hContentType,ct)] bod
        goodCT = -- "application/json;charset=utf-8"
                 "application/json"
        badCT  = "application/nonsense"

    it "passes the argument to the handler" $ do
      -- response <- mkReq post "" (encode alice)
      runTest SC.POST "" goodCT (BL.toStrict $ A.encode alice) >>=
        (`shouldDecodeTo` alice)
      -- liftIO $ decodesTo response alice `shouldBe` True

    it "rejects invalid request bodies with status 400" $ do
      -- runReqOnApi reqBodyApi server SC.PUT "/blah"
      runTest SC.PUT "/blah" goodCT "some invalid body" >>= (`shouldHaveStatus` 400)
    --   mkReq methodPut "/blah" "some invalid body" `shouldRespondWith` 400

    it "responds with 415 if the request body media type is unsupported" $ do
      runTest SC.POST "/" badCT ""
        >>= (`shouldHaveStatus` 415)
    --   post "/"
    --     [(hContentType, "application/nonsense")] "" `shouldRespondWith` 415

  where server :: Server ReqBodyApi AppHandler
        server = return :<|> return . age
        -- mkReq handler method x = mkRequest x
        --    [(hContentType, "application/json;charset=utf-8")] ""


-- }}}
------------------------------------------------------------------------------
-- * headerSpec {{{
------------------------------------------------------------------------------

type HeaderApi a = Header "MyHeader" a :> Delete '[JSON] NoContent
headerApi :: Proxy (HeaderApi a)
headerApi = Proxy


headerSpec :: Spec
headerSpec = describe "Servant.API.Header" $ do

    let expectsInt :: Maybe Int -> AppHandler NoContent
        expectsInt (Just x) = do
          when (x /= 5) $ error "Expected 5"
          return NoContent
        expectsInt Nothing  = error "Expected an int"

    let expectsString :: Maybe String -> AppHandler NoContent
        expectsString (Just x) = do
          when (x /= "more from you") $ error "Expected more from you"
          return NoContent
        expectsString Nothing  = error "Expected a string"

    --with (return (serve headerApi expectsInt)) $ do
    -- let delete' x = delete x [("MyHeader", "5")]

    it "passes the header to the handler (Int)" $ do
      runReqOnApi headerApi expectsInt SC.DELETE "/" "" [("MyHeader","5")] "" >>= (`shouldHaveStatus` 200)
            --delete' "/" "" `shouldRespondWith` 200

    -- with (return (serve headerApi expectsString)) $ do
    --     let delete' x = delete x [("MyHeader", "more from you")]

    it "passes the header to the handler (String)" $ do
      runReqOnApi headerApi expectsString SC.DELETE "/" "" [("MyHeader","more from you")] "" >>= (`shouldHaveStatus` 200)
            -- delete' "/" "" `shouldRespondWith` 200


-- }}}
------------------------------------------------------------------------------
-- * rawSpec {{{
------------------------------------------------------------------------------

type RawApi = "foo" :> Raw

rawApi :: Proxy RawApi
rawApi = Proxy

rawServer :: Show a => (Request -> a) -> AppHandler ()
rawServer f  = do
  (writeBS . B8.pack . show . f) =<< getRequest

rawSpec :: Spec
rawSpec = do
  describe "Servant.API.Raw" $ do

    it "runs applications" $ do
      runReqOnApi rawApi (rawServer (const (42 :: Integer))) SC.GET "foo" "" [] "" >>= (`shouldHaveBody` "42")

{-
      (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo"]
         }
        liftIO $ do
          simpleBody response `shouldBe` "42"
-}

    it "gets the pathInfo modified" $ do
      runReqOnApi rawApi (rawServer rqPathInfo) SC.GET "foo/bar" "" [] "" >>=
        (`shouldHaveBody` (T.pack (show ("bar" :: B8.ByteString))))

{-
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
alternativeSpec = do
  describe "Servant.API.Alternative" $ do

      it "unions endpoints" $ do
        -- response <- get "/foo"
        response <- runReqOnApi alternativeApi alternativeServer SC.GET "/foo" "" [] ""
        response `shouldDecodeTo` alice
        -- liftIO $ do
        --   decodesTo response alice `shouldBe` True
        -- response_ <- get "/bar"
        response_ <- runReqOnApi alternativeApi alternativeServer SC.GET "/bar" "" [] ""
        response_ `shouldDecodeTo` jerry
        -- liftIO $ do
        --   decodesTo response_ jerry `shouldBe`
        --     True

      it "checks all endpoints before returning 415" $ do
        -- response <- get "/foo"
        response <- runReqOnApi alternativeApi alternativeServer SC.GET "/foo" "" [] ""
        response `shouldHaveStatus` 200
        -- liftIO $ statusIs response 200 `shouldBe` True

      it "returns 404 if the path does not exist" $ do
        -- response <- get "/nonexistent"
        response <- runReqOnApi alternativeApi alternativeServer SC.GET "/nonexistent" "" [] ""
        response `shouldHaveStatus` 404
        -- liftIO $ statusIs response 404 `shouldBe` True
-- }}}
------------------------------------------------------------------------------
-- * responseHeaderSpec {{{
------------------------------------------------------------------------------
type ResponseHeadersApi =
       Get   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Post  '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Put   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Patch '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)

responseHeadersApi :: Proxy ResponseHeadersApi
responseHeadersApi = Proxy

responseHeadersServer :: Server ResponseHeadersApi AppHandler
responseHeadersServer = let h = return $ addHeader 5 $ addHeader "kilroy" "hi"
  in h :<|> h :<|> h :<|> h


responseHeadersSpec :: Spec
responseHeadersSpec = describe "ResponseHeaders" $ do

    let methods = [SC.GET, SC.POST, SC.PUT, SC.PATCH]

    it "includes the headers in the response" $
      forM_ methods $ \method -> do
        req <- runReqOnApi responseHeadersApi responseHeadersServer method "/" "" [] ""
        req `shouldHaveStatus` 200
        req `shouldHaveHeaders` [("H1","5"),("H2","kilroy")]

{-
        method "/" [] ""
          `shouldRespondWith` "\"hi\""{ matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
                                      , matchStatus  = 200
                                      }
-}

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \method ->
        runReqOnApi responseHeadersApi responseHeadersServer method "blahblah" "" [] "" >>= (`shouldHaveStatus` 404)
        -- method "blahblah" [] ""
        --  `shouldRespondWith` 404

    it "returns 406 if the Accept header is not supported" $
      forM_ methods $ \method ->
        runReqOnApi responseHeadersApi responseHeadersServer method "" "" [(hAccept, "crazy/mime")] "" >>= (`shouldHaveStatus` 406)
        -- method "" [(hAccept, "crazy/mime")] ""
        --   `shouldRespondWith` 406


-- }}}
------------------------------------------------------------------------------
-- * miscCombinatorSpec {{{
------------------------------------------------------------------------------

type MiscCombinatorsAPI
  =    "version" :> HttpVersion :> Get '[JSON] HttpVersion
  :<|> "secure"  :> IsSecure :> Get '[JSON] String
  :<|> "host"    :> RemoteHost :> Get '[PlainText] String

miscApi :: Proxy MiscCombinatorsAPI
miscApi = Proxy

miscServ :: Server MiscCombinatorsAPI AppHandler
miscServ = versionHandler
      :<|> secureHandler
      :<|> hostHandler

  where versionHandler = return :: HttpVersion -> AppHandler HttpVersion
        secureHandler :: IsSecure -> AppHandler String
        secureHandler Secure = return "secure"
        secureHandler NotSecure = return "not secure"
        hostHandler = return . B8.unpack  :: B8.ByteString -> AppHandler String

miscCombinatorSpec :: Spec
miscCombinatorSpec = do
  describe "Misc. combinators for request inspection" $ do
    it "Successfully gets the HTTP version specified in the request" $
      go "/version" (T.decodeUtf8 . BL.toStrict $
      A.encode ((1,1) :: HttpVersion))

    it "Checks that hspec-wai uses HTTP, not HTTPS" $
      go "/secure" "\"not secure\""

    it "Checks that hspec-wai issues request from 0.0.0.0" $
      go "/host" "localhost"

  where go path res = do
          runReqOnApi miscApi miscServ SC.GET path "" [] "" >>=
            (`shouldHaveBody` res)

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


------------------------------------------------------------------------------
-- * hspec-snap helpers
------------------------------------------------------------------------------

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


------------------------------------------------------------------------------
-- * Assorted Snap helpers
------------------------------------------------------------------------------

mkInitAndServer :: HasServer api
                => Proxy (api :: *)
                -> Server api AppHandler
                -> (SnapletInit App App, AppHandler ())
mkInitAndServer api serv = let sRoute = serveSnap api serv
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
  req <- State.get -- Useful for debugging
  liftIO $ print req
  return ()

runReqOnApi :: HasServer api
            => Proxy (api :: *)
            -> Server api AppHandler
            -> Method
            -> B8.ByteString
            -> B8.ByteString
            -> [Network.HTTP.Types.Header]
            -> B8.ByteString
            -> IO (Either T.Text Response)
runReqOnApi api serv method route qs hds bod =
  let (sInit, serv') = mkInitAndServer api serv
  in SST.runHandler Nothing (mkRequest method route qs hds bod) serv' sInit

routes :: HasServer api
       => Proxy (api :: *)
       -> Server api AppHandler
       -> [(B8.ByteString, AppHandler ())]
routes p s = [("", serveSnap p s)]

