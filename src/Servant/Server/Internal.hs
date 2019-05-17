{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

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
import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad               (join, when)
import           Data.Bool                   (bool)
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy        as BL
import           Data.Either                 (partitionEithers)
import           Data.List                   (foldl')
import           Data.Maybe                  (fromMaybe, isNothing, mapMaybe,
                                              maybeToList)
import           Data.Proxy
import           Data.String                 (IsString, fromString)
import           Data.String.Conversions     (cs, (<>))
import           Data.Tagged                 (Tagged(..), retag)
import qualified Data.Text                   as T
import           Data.Text                   (Text)
import           Data.Typeable               (Typeable)
import           GHC.Generics
import           GHC.TypeLits                (KnownNat, KnownSymbol, natVal,
                                              symbolVal)
import           Network.HTTP.Types          (HeaderName, Method,
                                              Status(..), parseQueryText,
                                              methodGet, methodHead,
                                              hContentType, hAccept)
import qualified Network.HTTP.Media          as NHM
import           Web.HttpApiData             (FromHttpApiData,
                                              parseHeader,
                                              parseQueryParam,
                                              parseUrlPieceMaybe,
                                              parseUrlPieces)
import           Snap.Core                   hiding (Headers, Method,
                                              getResponse, route,
                                              method, withRequest)
import           Servant.API                 ((:<|>) (..), (:>), BasicAuth,
                                              Capture',
                                              CaptureAll, Description, FramingRender (..), EmptyAPI
                                              Header', IsSecure(..), If, QueryFlag, IsSecure(..),
                                              QueryParam', QueryParams, Raw,
                                              RemoteHost, ReqBody',
                                              ReflectMethod(..), SBool(..), SBoolI, Stream, Summary,
                                              Verb, sbool
                                             )
import         Servant.API.Stream            (ToSourceIO(..))
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..), AllMime(..),
                                              MimeRender (..),
                                              canHandleAcceptH, contentType)
import           Servant.API.Modifiers       (FoldLenient, FoldRequired,
                                              RequestArgument, unfoldRequestArgument)
import           Servant.API.ResponseHeaders (Headers, getResponse, GetHeaders,
                                              getHeaders)
import qualified Servant.Types.SourceT       as S
import qualified System.IO.Streams           as IOS

import           Servant.Server.Internal.BasicAuth
import           Servant.Server.Internal.Context
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr
import           Servant.Server.Internal.SnapShims


class HasServer api context (m :: * -> *) where
  type ServerT api context m :: *

  route :: MonadSnap m
        => Proxy api
        -> Context context
        -> Delayed m env (Server api context m)
        -> Router m env

  hoistServerWithContext
      :: proxy api
      -> proxy' context
      -> (forall x. m x -> n x)
      -> ServerT api context m
      -> ServerT api context n

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

  hoistServerWithContext _ pc nt (a :<|> b) =
    hoistServerWithContext (Proxy :: Proxy a) pc nt a :<|>
    hoistServerWithContext (Proxy :: Proxy b) pc nt b

captured :: FromHttpApiData a => proxy (Capture' mods sym a) -> Text -> Maybe a
captured _ = parseUrlPieceMaybe

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> EitherT ServantErr IO Book
-- >         getBook isbn = ...
instance (FromHttpApiData a, HasServer api context m)
      => HasServer (Capture' mods capture a :> api) context m where

  type ServerT (Capture' mods capture a :> api) context m =
     a -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy ctx d =
    CaptureRouter $
      route (Proxy :: Proxy api) ctx
        (addCapture d $ \ txt -> case parseUrlPieceMaybe txt of
                                   Nothing -> delayedFail err400
                                   Just v  -> return v
        )

instance (FromHttpApiData a, HasServer api context m)
      => HasServer (CaptureAll capture a :> api) context m where

  type ServerT (CaptureAll capture a :> api) context m =
    [a] -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy ctx d =
    CaptureAllRouter $
        route (Proxy :: Proxy api) ctx
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
processMethodRouter handleA status method rHeaders request = case handleA of
  Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
  Just (contentT, body) -> Route $ responseLBS status hdrs bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      hdrs = (hContentType, cs contentT) : fromMaybe [] rHeaders

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
                let hdrs    = getHeaders output
                    handleA = handleAcceptH proxy (AcceptHeader accH) (getResponse output)
                processMethodRouter handleA status method (Just hdrs) request


instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a,
                               ReflectMethod method,
                               KnownNat status)
  => HasServer (Verb method status ctypes a) context m where
  type ServerT (Verb method status ctypes  a) context m = m a
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = methodRouter method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)


instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a,
                               ReflectMethod method,
                               KnownNat status,
                               GetHeaders (Headers h a))
  => HasServer (Verb method status ctypes (Headers h a)) context m where

  type ServerT (Verb method status ctypes (Headers h a)) context m = m (Headers h a)
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = methodRouterHeaders method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)



-- instance {-# OVERLAPPABLE #-} (MimeRender ctype a,
--                                ReflectMethod method,
--                                KnownNat status,
--                                FramingRender framing ctype,
--                                ToStreamGenerator b a)
--   => HasServer (Stream method status framing ctype b) context m where
--   type ServerT (Stream method status framing ctype b) context m = m b
--   hoistServerWithContext _ _ nt s = nt s

--   route Proxy _ = streamRouter ([],) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
--     where method = reflectMethod (Proxy :: Proxy method)
--           status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)


-- instance {-# OVERLAPPABLE #-} (MimeRender ctype a,
--                                ReflectMethod method,
--                                KnownNat status,
--                                FramingRender framing ctype,
--                                ToStreamGenerator b a,
--                                GetHeaders (Headers h b))
--   => HasServer (Stream method status framing ctype (Headers h b)) context m where

--   type ServerT (Stream method status framing ctype (Headers h b)) context m = m (Headers h b)
--   hoistServerWithContext _ _ nt s = nt s

--   route Proxy _ = streamRouter (\x -> (getHeaders x, getResponse x)) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
--     where method = reflectMethod (Proxy :: Proxy method)
--           status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

streamRouter :: (MimeRender ctype a,
                 FramingRender framing ctype,
                 ToStreamGenerator b a,
                 MonadSnap m
                ) =>
                (c -> ([(HeaderName, B.ByteString)], b))
             -> Method
             -> Status
             -> Proxy framing
             -> Proxy ctype
             -> Delayed m env (m c)
             -> Router m env
streamRouter splitHeaders method (Status code msg) framingproxy ctypeproxy action = leafRouter $ \env request respond ->
          let accH    = fromMaybe ct_wildcard $ getHeader hAccept request
              cmediatype = NHM.matchAccept [contentType ctypeproxy] accH
              accCheck = when (isNothing cmediatype) $ delayedFail err406
              contentHeader = (hContentType, NHM.renderHeader . maybeToList $ cmediatype)
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` accCheck
                       ) env request respond $ \ output ->
                let (headerz, fa) = splitHeaders output
                    k = getStreamGenerator . toStreamGenerator $ fa in
                Route $ setResponseStatus code msg
                      $ (\r -> foldl' (\r' (h,h') -> addHeader h h' r') r $ contentHeader : headerz)
                      $ flip setResponseBody emptyResponse $ \outStream -> do
                        let writeAndFlush bb = IOS.write (Just $ bb <> BB.flush) outStream
                        writeAndFlush (BB.lazyByteString $ header framingproxy ctypeproxy)
                        case boundary framingproxy ctypeproxy of
                             BoundaryStrategyBracket f ->
                                      let go x = let bs = mimeRender ctypeproxy x
                                                     (before, after) = f bs
                                                 in writeAndFlush (   BB.lazyByteString before
                                                                   <> BB.lazyByteString bs
                                                                   <> BB.lazyByteString after)
                                      in k go go
                             BoundaryStrategyIntersperse sep -> k
                               (\x -> do
                                  writeAndFlush . BB.lazyByteString $ mimeRender ctypeproxy x
                               )
                               (\x -> do
                                  writeAndFlush . (BB.lazyByteString sep <>) . BB.lazyByteString $ mimeRender ctypeproxy x
                               )
                             BoundaryStrategyGeneral f ->
                                      let go = writeAndFlush . BB.lazyByteString . f . mimeRender ctypeproxy
                                      in  k go go
                        IOS.write (Just . BB.lazyByteString $ trailer framingproxy ctypeproxy) outStream
                        return outStream


-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromHttpApiData' instance.
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
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context m
         , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
         )
      => HasServer (Header' mods sym a :> api) context m where

  type ServerT (Header' mods sym a :> api) context m =
    RequestArgument mods a -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy ctx subserver = route (Proxy :: Proxy api) ctx $
      subserver `addHeaderCheck` withRequest headerCheck
    where
      headerName :: IsString n => n
      headerName = fromString $ symbolVal (Proxy :: Proxy sym)

      headerCheck :: Request -> DelayedM m (RequestArgument mods a)
      headerCheck req =
          unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
        where
          mev :: Maybe (Either Text a)
          mev = fmap parseHeader $ getHeader headerName req

          errReq = delayedFailFatal err400
            { errBody = "Header " <> headerName <> " is required"
            }

          errSt e = delayedFailFatal err400
            { errBody = cs $ "Error parsing header "
                             <> headerName
                             <> " failed: " <> e
            }

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
-- by simply providing an instance of 'FromHttpApiData' for your type.
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
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context m
         , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
         )
      => HasServer (QueryParam' mods sym a :> api) context m where

  type ServerT (QueryParam' mods sym a :> api) context m =
    RequestArgument mods a -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy ctx subserver =
    let querytext r = parseQueryText $ rqQueryString r
        paramname = cs $ symbolVal (Proxy :: Proxy sym)

        parseParam :: Request -> DelayedM m (RequestArgument mods a)
        parseParam req =
            unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either T.Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext req

            errReq = delayedFailFatal err400
              { errBody = cs $ "Query parameter " <> paramname <> " is required"
              }

            errSt e = delayedFailFatal err400
              { errBody = cs $ "Error parsing query parameter "
                               <> paramname <> " failed: " <> e
              }

        delayed = addParameterCheck subserver . withRequest $ parseParam

    in route (Proxy :: Proxy api) ctx delayed

-- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context m)
      => HasServer (QueryParams sym a :> api) context m where

  type ServerT (QueryParams sym a :> api) context m =
    [a] -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy ctx subserver = route (Proxy :: Proxy api) ctx $
      subserver `addParameterCheck` withRequest paramsCheck
    where
      paramname = cs $ symbolVal (Proxy :: Proxy sym)
      paramsCheck req =
          case partitionEithers $ fmap parseQueryParam params of
              ([], parsed) -> return parsed
              (errs, _)   -> delayedFailFatal err400
                  { errBody = cs $ "Error parsing query parameter(s) "
                                   <> paramname <> " failed: "
                                   <> T.intercalate ", " errs
                  }
         where
           params :: [T.Text]
           params = mapMaybe snd
                  . filter (looksLikeParam . fst)
                  . parseQueryText
                  . rqQueryString
                  $ req

           looksLikeParam name = name == paramname || name == (paramname <> "[]")

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
instance (KnownSymbol sym, HasServer api context m)
      => HasServer (QueryFlag sym :> api) context m where

  type ServerT (QueryFlag sym :> api) context m =
    Bool -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    let querytext r = parseQueryText $ rqQueryString r
        param r = case lookup paramname (querytext r) of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy api) context (passToServer subserver param)
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

  hoistServerWithContext _ _ nt = nt

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
-- header, it is treated as @application/octet-stream@ (as specified in
-- <https://tools.ietf.org/html/rfc7231#section-3.1.1.4 RFC7231>.
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
instance ( AllCTUnrender list a, HasServer api context m, SBoolI (FoldLenient mods), MonadSnap m
         ) => HasServer (ReqBody' mods list a :> api) context m where

  type ServerT (ReqBody' mods list a :> api) context m =
    If (FoldLenient mods) (Either String a) a -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver
      = route (Proxy :: Proxy api) context $
          addBodyCheck subserver ctCheck bodyCheck
    where
      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \ request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ getHeader hContentType request
        case canHandleCTypeH (Proxy :: Proxy list) (cs contentTypeH) :: Maybe (BL.ByteString -> Either String a) of
          Nothing -> delayedFail err415
          Just f -> return f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = do
        mrqbody <- lift $ runRequestBody $ \ stream -> f <$> BL.fromChunks <$> IOS.toList {- IOS.toLazyByteString -} stream
        case sbool :: SBool (FoldLenient mods) of
          STrue -> return mrqbody
          SFalse -> case mrqbody of
            Left e  -> delayedFailFatal err400 { errBody = cs e }
            Right v -> return v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @api@.
instance (KnownSymbol path, HasServer api context m) => HasServer (path :> api) context m where

  type ServerT (path :> api) context m = ServerT api context m

  route Proxy context subserver =
    pathRouter
      (cs (symbolVal proxyPath))
      (route (Proxy :: Proxy api) context subserver)
    where proxyPath = Proxy :: Proxy path
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance HasServer api context m => HasServer (RemoteHost :> api) context m where
  type ServerT (RemoteHost :> api) context m = B.ByteString -> ServerT api context m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver rqHostName)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance HasServer api context m => HasServer (IsSecure :> api) context m where
  type ServerT (IsSecure :> api) context m = IsSecure -> ServerT api context m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver (bool NotSecure Secure . rqIsSecure))
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- This appears to be about 1000000% WAI-specific, so better hope you didn't need it

-- instance HasServer api context m => HasServer (Vault :> api) context m where
--   type ServerT (Vault :> api) context m = Vault -> ServerT api context m
-- 
--   route Proxy context subserver =
--     route (Proxy :: Proxy api) context (passToServer subserver vault)
--   hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance HasServer api context m => HasServer (HttpVersion :> api) context m where
  type ServerT (HttpVersion :> api) context m = HttpVersion -> ServerT api context m

  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx (passToServer subserver rqVersion)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- | Ignore @'Summary'@ in server handlers.
instance HasServer api ctx m => HasServer (Summary desc :> api) ctx m where
  type ServerT (Summary desc :> api) ctx m = ServerT api ctx m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- | Ignore @'Description'@ in server handlers.
instance HasServer api ctx m => HasServer (Description desc :> api) ctx m where
  type ServerT (Description desc :> api) ctx m = ServerT api ctx m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- | Singleton type representing a server that serves an empty API.
data EmptyServer = EmptyServer deriving (Typeable, Eq, Show, Bounded, Enum)

-- | Server for `EmptyAPI`
emptyServer :: ServerT EmptyAPI context m
emptyServer = Tagged EmptyServer

-- | The server for an `EmptyAPI` is `emptyAPIServer`.
--
-- > type MyApi = "nothing" :> EmptyApi
-- >
-- > server :: Server MyApi
-- > server = emptyAPIServer
instance HasServer EmptyAPI context m where
  type ServerT EmptyAPI context m = Tagged m EmptyServer

  route Proxy _ _ = StaticRouter mempty mempty

  hoistServerWithContext _ _ _ = retag

-- | Basic Authentication
instance ( KnownSymbol realm
         , HasServer api context m
         , HasContextEntry context (BasicAuthCheck m usr)
         )
    => HasServer (BasicAuth realm usr :> api) context m where

  type ServerT (BasicAuth realm usr :> api) context m = usr -> ServerT api context m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
      realm = B.pack $ symbolVal (Proxy :: Proxy realm)
      basicAuthContext = getContextEntry context
      authCheck = withRequest $ \ req -> runBasicAuth req realm basicAuthContext

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s


-- newtype BasicAuthCheck m usr =
--   BasicAuthCheck { unBasicAuthCheck :: BasicAuthData -> m (BasicAuthResult usr) }
--   deriving (Functor, Generic)

data BasicAuthResult usr = Unauthorized | BadPassword | NoSuchUser | Authorized usr
  deriving (Functor, Eq, Read, Show, Generic)

-- * helpers
--
ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP

instance forall ctype chunk method status framing m context a.
         ( MimeRender ctype chunk, ReflectMethod method, KnownNat status,
           FramingRender framing, MonadSnap m, ToSourceIO chunk a
         ) => HasServer (Stream method status framing ctype a) context m where
  type ServerT (Stream method status framing ctype a) context m = m a

  route Proxy _ action = streamRouter ([],) method status
                                      (Proxy :: Proxy framing)
                                      (Proxy :: Proxy ctype) action
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance ( MimeRender ctype chunk, ReflectMethod method, MimeRender ctype a, KnownNat status, MonadSnap m,
           FramingRender framing, ToSourceIO chunk a, GetHeaders (Headers h a)
         ) => HasServer (Stream method status framing ctype (Headers h a)) context m where
  type ServerT (Stream method status framing ctype (Headers h a)) context m = m (Headers h a)

  route Proxy _ action =
    streamRouter (\x -> (getHeaders x, getResponse x))
                 method
                 status
                 (Proxy :: Proxy framing)
                 (Proxy :: Proxy ctype)
                 action
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)



streamRouter :: forall ctype a c chunk env framing m
               .(MimeRender ctype chunk,
                 FramingRender framing,
                 -- ToSourceIO chunk (m a),
                 ToSourceIO chunk a,
                 MonadSnap m
                ) =>
                (c -> ([(HeaderName, B.ByteString)], a))
             -> Method
             -> Status
             -> Proxy framing
             -> Proxy ctype
             -> Delayed m env (m c)
             -> Router m env
streamRouter splitHeaders method status framingproxy ctypeproxy action =
  leafRouter $ \env request respond ->
    let accH    = fromMaybe ct_wildcard $ getHeader hAccept request
        cmediatype = NHM.matchAccept [contentType ctypeproxy] accH
        accCheck = when (isNothing cmediatype) $ delayedFail err406
        contentHeader = (hContentType, NHM.renderHeader . maybeToList $ cmediatype)
    in runAction (action `addMethodCheck` methodCheck method request
                         `addAcceptCheck` accCheck
                 ) env request respond $ \ output ->
          let (headers', fa) = splitHeaders output
              sourceT = toSourceIO fa
              S.SourceT kStepLBS =
                framingRender framingproxy
                              (mimeRender ctypeproxy :: chunk -> BL.ByteString)
                              sourceT
              response = setResponseStatus (statusCode status) (statusMessage status)
                         $ setHeaders (contentHeader:headers') emptyResponse
          in
          Route $ flip setResponseBody response $ \outStream -> do
            let
                writeAndFlush bb = IOS.write (Just $ bb <> BB.flush) outStream
                justFlush = IOS.write (Just BB.flush) outStream
                loop :: S.StepT IO BL.ByteString -> IO (IOS.OutputStream BB.Builder)
                loop S.Stop = justFlush >> return outStream
                loop (S.Error err) = fail err -- TODO: Provide a better error message
                loop (S.Skip s)    = loop s
                loop (S.Effect ms) = ms >>= loop
                loop (S.Yield lbs s) = do
                  writeAndFlush (BB.lazyByteString lbs)
                  loop s

            kStepLBS loop
