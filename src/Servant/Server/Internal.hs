{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.PathInfo
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServantErr
  ) where

import           Control.Applicative         ((<$>))
import           Control.Monad.Trans.Either  (EitherT(..))
import qualified Data.ByteString.Char8       as B
import           Data.CaseInsensitive        (mk)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromMaybe, mapMaybe)
import           Data.Proxy
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           GHC.TypeLits                (KnownNat, KnownSymbol, natVal, symbolVal)
import           Network.HTTP.Types          (Method, QueryText, Status(..), parseQueryText)
import           Web.HttpApiData             (FromHttpApiData)
import           Web.HttpApiData.Internal    (parseHeaderMaybe,
                                              parseQueryParamMaybe,
                                              parseUrlPieceMaybe)
import           Snap.Core                   hiding (Headers, Method, getHeaders,
                                              getResponse, headers, route,
                                              method)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                               Delete, Get, Header,
                                              Patch, Post, Put,
                                              QueryFlag, QueryParam,
                                              QueryParams, Raw(..), ReqBody, ReflectMethod(..), Verb)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..))
import           Servant.API.ResponseHeaders (Headers, getResponse, GetHeaders,
                                              getHeaders)
-- import           Servant.Common.Text         (FromText, fromText)

import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr
import           Servant.Server.Internal.SnapShims

class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: MonadSnap m
        => Proxy layout
        -> m (RouteResult (Server layout m))
        -> Router Request (RoutingApplication m) m

type Server layout m = ServerT layout m

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
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
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
instance (KnownSymbol capture, FromHttpApiData a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type ServerT (Capture capture a :> sublayout) m =
     a -> ServerT sublayout m

  route Proxy subserver =
    DynamicRouter $ \ first ->
      route (Proxy :: Proxy sublayout)
            (case captured captureProxy first of
               Nothing  -> return $ failWith NotFound
               Just v   -> feedTo subserver v)
    where captureProxy = Proxy :: Proxy (Capture capture a)


methodRouter :: (AllCTRender ctypes a, MonadSnap m)
             => Method -> Proxy ctypes -> Status
             -> m (RouteResult (m a))
             -> Router Request (RoutingApplication m) m
methodRouter method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && unSnapMethod (rqMethod request) == method = do
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ getHeader (mk "Accept") request
            case handleAcceptH proxy (AcceptHeader accH) output of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS status [ ("Content-Type" , cs contentT)] body
      | pathIsEmpty request && unSnapMethod (rqMethod request) /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound

methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v, MonadSnap m)
                    => Method -> Proxy ctypes -> Status
                    -> m (RouteResult (m (Headers h v)))
                    -> Router Request (RoutingApplication m) m
methodRouterHeaders method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && unSnapMethod (rqMethod request) == method = do
        runAction action respond $ \ output -> do
          let accH = fromMaybe ct_wildcard $ getHeader (mk "Accept") request
              headers = getHeaders output
          case handleAcceptH proxy (AcceptHeader accH) (getResponse output) of
            Nothing -> failWith UnsupportedMediaType
            Just (contentT, body) -> succeedWith $
              responseLBS status ( ("Content-Type" , cs contentT) : headers) body
      | pathIsEmpty request && unSnapMethod (rqMethod request) /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound

methodRouterEmpty :: MonadSnap m => Method
                  -> m (RouteResult (m ()))
                  -> Router Request (RoutingApplication m) m
methodRouterEmpty method action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && unSnapMethod (rqMethod request) == method = do
          runAction action respond $ \ () ->
            succeedWith $ responseLBS noContent204 [] ""
      | pathIsEmpty request && unSnapMethod (rqMethod request) /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound


instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a,
                               ReflectMethod method,
                               KnownNat status)
  => HasServer (Verb method status ctypes a) where
  type ServerT (Verb method status ctypes  a) m = m a
  route Proxy = methodRouter method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a,
                               ReflectMethod method,
                               KnownNat status,
                               GetHeaders (Headers h a))
  => HasServer (Verb method status ctypes (Headers h a)) where

  type ServerT (Verb method status ctypes (Headers h a)) m = m (Headers h a)
  route Proxy = methodRouterHeaders method (Proxy :: Proxy ctypes) status
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
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout)
      => HasServer (Header sym a :> sublayout) where

  type ServerT (Header sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let mheader = parseHeaderMaybe =<< getHeader str request
    -- let mheader = parseHeaderMaybe =<< lookup str (requestHeaders request)
    in  route (Proxy :: Proxy sublayout) (feedTo subserver mheader)
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
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout)
      => HasServer (QueryParam sym a :> sublayout) where

  type ServerT (QueryParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let p =
          case rqQueryParam paramname request of
            Nothing       -> Nothing -- param absent from the query string
            Just []       -> Nothing -- param present with no value -> Nothing
            Just (v:_) -> parseQueryParamMaybe (decodeUtf8 v)-- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy sublayout) (feedTo subserver p)
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
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout)
      => HasServer (QueryParams sym a :> sublayout) where

  type ServerT (QueryParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) (feedTo subserver (values request))
    where
      paramName     = cs $ symbolVal (Proxy :: Proxy sym)
      paramsBare r = concat $ rqQueryParam paramName r
      paramsBrak r = concat $ rqQueryParam (paramName <> "[]") r
      values     r = mapMaybe (parseQueryParamMaybe . decodeUtf8) $ paramsBare r <> paramsBrak r


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
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (QueryFlag sym :> sublayout) where

  type ServerT (QueryFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let p = case rqQueryParam paramname request of
          Just []       -> True  -- param is there, with no value
          Just (v:_)    -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy sublayout) (feedTo subserver p)
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
--instance (ToRawApplication a, MonadSnap m) => HasServer (Raw m a) where
--instance ToRawApplication a => HasServer (Raw m a) where
--instance forall m a n.(MonadSnap m, m ~ n) => HasServer (Raw m (n a)) where
instance HasServer Raw where
--instance ToRawApplication a => HasServer (Raw m a) where

  --type ServerT (Raw n a) m = Raw n (Application m)
  type ServerT Raw m = m ()

  -- route :: Proxy layout -> IO (RouteResult (Server layout)) -> Router
  route Proxy rawApplication = LeafRouter $ \ request respond -> do
    r <- rawApplication
    case r of
      RR (Left err)      -> respond $ failWith err
      RR (Right rawApp) -> (snapToApplication rawApp )request (respond . succeedWith)


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
instance ( AllCTUnrender list a, HasServer sublayout
         ) => HasServer (ReqBody list a :> sublayout) where

  type ServerT (ReqBody list a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) $ do
      -- See HTTP RFC 2616, section 7.2.1
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
      -- See also "W3C Internet Media Type registration, consistency of use"
      -- http://www.w3.org/2001/tag/2002/0129-mime
      let contentTypeH = fromMaybe "application/octet-stream"
                       $ getHeader (mk "Content-Type") request
      rBody <- readRequestBody 10000000
      mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH)
                 <$> return rBody -- lazyRequestBody request
      case mrqbody of
        Nothing -> return $ failWith $ UnsupportedMediaType
        Just (Left e) -> return $ failWith $ InvalidBody e
        Just (Right v) -> feedTo subserver v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP
