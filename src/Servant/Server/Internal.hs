{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.PathInfo
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServantErr
  ) where

-------------------------------------------------------------------------------
import           Control.Applicative         ((<$>))
import           Control.Monad.Trans.Class   (lift)
import           Data.Bool                   (bool)
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy        as BL
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Proxy
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>))
import           Data.Text                   (Text)
import           GHC.Generics
import           GHC.TypeLits                (KnownNat, KnownSymbol, natVal,
                                              symbolVal)
import           Network.HTTP.Types          (HeaderName, Method,
                                              Status(..), parseQueryText,
                                              methodGet, methodHead,
                                              hContentType, hAccept)
import           Web.HttpApiData             (FromHttpApiData,
                                              parseHeaderMaybe,
                                              parseQueryParamMaybe,
                                              parseUrlPieceMaybe,
                                              parseUrlPieces)
import           Snap.Core                   hiding (Headers, Method,
                                              getResponse, headers, route,
                                              method, withRequest)
import           Servant.API                 ((:<|>) (..), (:>), BasicAuth,
                                              Capture,
                                              CaptureAll, Header,
                                              IsSecure(..), QueryFlag,
                                              QueryParam, QueryParams, Raw,
                                              RemoteHost, ReqBody,
                                              ReflectMethod(..), Verb)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..), AllMime(..), canHandleAcceptH)
import           Servant.API.ResponseHeaders (Headers, getResponse, GetHeaders,
                                              getHeaders)
-- import           Servant.Common.Text         (FromText, fromText)

import           Servant.Server.Internal.BasicAuth
import           Servant.Server.Internal.Context
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr
import           Servant.Server.Internal.SnapShims

-- TODO: add MonadSnap m => m ?
class HasServer api context (m :: * -> *) where
  type ServerT api context m :: *

  route :: MonadSnap m
        => Proxy api
        -> Context context
        -> Delayed m env (Server api context m)
        -> Router m env

type Server api context m = ServerT api context m

-- * Instances

-- | A server for @a ':<|>' b@ first tries to match the request against the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasServer a ctx m, HasServer b ctx m) => HasServer (a :<|> b) ctx m where

  type ServerT (a :<|> b) ctx m = ServerT a ctx m :<|> ServerT b ctx m

  route Proxy ctx server = choice (route pa ctx ((\ (a :<|> _) -> a) <$> server))
                                  (route pb ctx ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromHttpApiData a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = parseUrlPieceMaybe

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> EitherT ServantErr IO Book
-- >         getBook isbn = ...
instance (FromHttpApiData a, HasServer sublayout context m)
      => HasServer (Capture capture a :> sublayout) context m where

  type ServerT (Capture capture a :> sublayout) context m =
     a -> ServerT sublayout context m

  route Proxy ctx d =
    CaptureRouter $
      route (Proxy :: Proxy sublayout) ctx
        (addCapture d $ \ txt -> case parseUrlPieceMaybe txt of
                                   Nothing -> delayedFail err400
                                   Just v  -> return v
        )


instance (FromHttpApiData a, HasServer sublayout context m)
      => HasServer (CaptureAll capture a :> sublayout) context m where

  type ServerT (CaptureAll capture a :> sublayout) context m =
    [a] -> ServerT sublayout context m

  route Proxy ctx d =
    CaptureAllRouter $
        route (Proxy :: Proxy sublayout) ctx
              (addCapture d $ \ txts -> case parseUrlPieces txts of
                 Left _  -> delayedFail err400
                 Right v -> return v
              )


allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request =
  method == methodGet && unSnapMethod (rqMethod request) == methodHead

allowedMethod :: Method -> Request -> Bool
allowedMethod method request =
  allowedMethodHead method request || unSnapMethod (rqMethod request) == method

processMethodRouter :: Maybe (BL.ByteString, BL.ByteString) -> Status -> Method
                    -> Maybe [(HeaderName, B.ByteString)]
                    -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
  Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
  Just (contentT, body) -> Route $ responseLBS status hdrs bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      hdrs = (hContentType, cs contentT) : fromMaybe [] headers

methodCheck :: MonadSnap m => Method -> Request -> DelayedM m ()
methodCheck method request
  | allowedMethod method request = return ()
  | otherwise                    = delayedFail err405

-- This has switched between using 'Fail' and 'FailFatal' a number of
-- times. If the 'acceptCheck' is run after the body check (which would
-- be morally right), then we have to set this to 'FailFatal', because
-- the body check is not reversible, and therefore backtracking after the
-- body check is no longer an option. However, we now run the accept
-- check before the body check and can therefore afford to make it
-- recoverable.
acceptCheck :: (AllMime list, MonadSnap m) => Proxy list -> B.ByteString -> DelayedM m ()
acceptCheck proxy accH
  | canHandleAcceptH proxy (AcceptHeader accH) = return ()
  | otherwise                                  = delayedFail err406



methodRouter :: (AllCTRender ctypes a, MonadSnap m)
             => Method -> Proxy ctypes -> Status
             -> Delayed m env (m a)
             -> Router m env -- Request (RoutingApplication m) m
methodRouter method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH = fromMaybe ct_wildcard $ getHeader hAccept request -- lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
               let handleA = handleAcceptH proxy (AcceptHeader accH) output
               processMethodRouter handleA status method Nothing request


methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v, MonadSnap m)
                    => Method -> Proxy ctypes -> Status
                    -> Delayed m env (m (Headers h v))
                    -> Router m env -- Request (RoutingApplication m) m
methodRouterHeaders method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH    = fromMaybe ct_wildcard $ getHeader hAccept request -- lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
                let headers = getHeaders output
                    handleA = handleAcceptH proxy (AcceptHeader accH) (getResponse output)
                processMethodRouter handleA status method (Just headers) request


instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a,
                               ReflectMethod method,
                               KnownNat status)
  => HasServer (Verb method status ctypes a) context m where
  type ServerT (Verb method status ctypes  a) context m = m a
  route Proxy _ = methodRouter method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a,
                               ReflectMethod method,
                               KnownNat status,
                               GetHeaders (Headers h a))
  => HasServer (Verb method status ctypes (Headers h a)) context m where

  type ServerT (Verb method status ctypes (Headers h a)) context m = m (Headers h a)
  route Proxy _ = methodRouterHeaders method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)



-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromText' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> EitherT ServantErr IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout context m)
      => HasServer (Header sym a :> sublayout) context m where

  type ServerT (Header sym a :> sublayout) context m =
    Maybe a -> ServerT sublayout context m

  route Proxy ctx subserver =
    let mheader req = parseHeaderMaybe =<< getHeader str req
    in  route (Proxy :: Proxy sublayout) ctx (passToServer subserver mheader)
    where str = fromString $ symbolVal (Proxy :: Proxy sym)


-- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout context m)
      => HasServer (QueryParam sym a :> sublayout) context m where

  type ServerT (QueryParam sym a :> sublayout) context m =
    Maybe a -> ServerT sublayout context m

  route Proxy ctx subserver =
    let querytext r = parseQueryText $ rqQueryString r
        param r =
          case lookup paramname (querytext r) of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> parseQueryParamMaybe v -- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy sublayout) ctx (passToServer subserver param)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)


-- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout context m)
      => HasServer (QueryParams sym a :> sublayout) context m where

  type ServerT (QueryParams sym a :> sublayout) context m =
    [a] -> ServerT sublayout context m

  route Proxy ctx subserver =
    let querytext r = parseQueryText $ rqQueryString r
        -- if sym is "foo", we look for query string parameters
        -- named "foo" or "foo[]" and call parseQueryParam on the
        -- corresponding values
        parameters r = filter looksLikeParam (querytext r)
        values r = mapMaybe (convert . snd) (parameters r)
    in  route (Proxy :: Proxy sublayout) ctx (passToServer subserver values)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = parseQueryParamMaybe v


-- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout context m)
      => HasServer (QueryFlag sym :> sublayout) context m where

  type ServerT (QueryFlag sym :> sublayout) context m =
    Bool -> ServerT sublayout context m

  route Proxy ctx subserver =
    let querytext r = parseQueryText $ rqQueryString r
        param r = case lookup paramname (querytext r) of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy sublayout) ctx (passToServer subserver param)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False


-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer Raw context m where

  type ServerT Raw context m = m ()

  route Proxy _ rawApplication = RawRouter $ \ env request respond -> do
    r <- runDelayed rawApplication env request
    case r of
      Route app   -> (snapToApplication' app) request (respond . Route)
      Fail a      -> respond $ Fail a
      FailFatal e -> respond $ FailFatal e


-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
--
-- All it asks is for a 'FromJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = postBook
-- >   where postBook :: Book -> EitherT ServantErr IO Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer sublayout context m
         ) => HasServer (ReqBody list a :> sublayout) context m where

  type ServerT (ReqBody list a :> sublayout) context m =
    a -> ServerT sublayout context m

  route Proxy ctx subserver =
    route (Proxy :: Proxy sublayout) ctx (addBodyCheck (subserver ) bodyCheck')
    where
      -- bodyCheck' :: DelayedM m a
      bodyCheck' = do
        req <- lift getRequest
        let contentTypeH = fromMaybe "application/octet-stream" $ getHeader hContentType req
        mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH) <$>
                                 lift (readRequestBody 2147483647) -- Maximum size: 2GB
        case mrqbody of
          Nothing        -> delayedFailFatal err415
          Just (Left e)  -> delayedFailFatal err400 { errBody = cs e }
          Just (Right v) -> return v


-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout context m) => HasServer (path :> sublayout) context m where

  type ServerT (path :> sublayout) context m = ServerT sublayout context m

  route Proxy ctx subserver =
    pathRouter
      (cs (symbolVal proxyPath))
      (route (Proxy :: Proxy sublayout) ctx subserver)
    where proxyPath = Proxy :: Proxy path


instance HasServer api context m => HasServer (HttpVersion :> api) context m where
  type ServerT (HttpVersion :> api) context m = HttpVersion -> ServerT api context m

  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx (passToServer subserver rqVersion)


instance HasServer api context m => HasServer (IsSecure :> api) context m where
  type ServerT (IsSecure :> api) context m = IsSecure -> ServerT api context m

  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx (passToServer subserver (bool NotSecure Secure . rqIsSecure))

instance HasServer api context m => HasServer (RemoteHost :> api) context m where
  type ServerT (RemoteHost :> api) context m = B.ByteString -> ServerT api context m

  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx (passToServer subserver rqHostName)

-- newtype BasicAuthCheck m usr =
--   BasicAuthCheck { unBasicAuthCheck :: BasicAuthData -> m (BasicAuthResult usr) }
--   deriving (Functor, Generic)

data BasicAuthResult usr = Unauthorized | BadPassword | NoSuchUser | Authorized usr
  deriving (Functor, Eq, Read, Show, Generic)

instance (HasServer api context m,
          KnownSymbol realm,
          HasContextEntry context (BasicAuthCheck m usr)
         ) => HasServer (BasicAuth realm usr :> api) context m where
  type ServerT (BasicAuth realm usr :> api) context m = usr -> ServerT api context m

  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx (subserver `addAuthCheck` authCheck)
    where
      realm = B.pack $ symbolVal (Proxy :: Proxy realm)
      basicAuthContext = getContextEntry ctx
      authCheck = withRequest $ \req -> runBasicAuth req realm basicAuthContext

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP
