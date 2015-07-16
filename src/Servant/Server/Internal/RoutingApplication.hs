{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
module Servant.Server.Internal.RoutingApplication where

import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Control.Applicative                 (Applicative, (<$>))
import           Control.Monad.IO.Class              (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Either          (EitherT, runEitherT)
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as BL
import           Data.CaseInsensitive                (CI)
import           Data.IORef                          (IORef, newIORef,
                                                      readIORef, writeIORef)
import qualified Data.List                           as L
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         (Monoid, mappend, mempty,
                                                      (<>))
import           Data.String                         (fromString)
import           GHC.Int                             (Int64)
--import           Network.HTTP.Types                  hiding (Header,
--                                                      ResponseHeaders)
-- import qualified Network.Wai                         as Wai (Application,
--                                                              Request, Response,
--                                                              ResponseReceived,
--                                                              requestBody,
--                                                              responseLBS,
--                                                              strictRequestBody)
import           Servant.API                         ((:<|>) (..))
import           Servant.Server.Internal.ServantErr
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Internal.Http.Types
import           Snap.Internal.Iteratee.Debug        (iterateeDebugWrapper)
import qualified Snap.Iteratee                       as I

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO Response) -> IO Response


-- | A wrapper around @'Either' 'RouteMismatch' a@.
newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show, Functor, Applicative)

-- | If we get a `Right`, it has precedence over everything else.
--
-- This in particular means that if we could get several 'Right's,
-- only the first we encounter would be taken into account.
instance Monoid (RouteResult a) where
  mempty = RR $ Left mempty

  RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
  RR (Left _)  `mappend` RR (Right y) = RR $ Right y
  r            `mappend` _            = r

-- Note that the ordering of the constructors has great significance! It
-- determines the Ord instance and, consequently, the monoid instance.
data RouteMismatch =
    NotFound           -- ^ the usual "not found" error
  | WrongMethod        -- ^ a more informative "you just got the HTTP method wrong" error
  | UnsupportedMediaType -- ^ request body has unsupported media type
  | InvalidBody String -- ^ an even more informative "your json request body wasn't valid" error
  | HttpError Status (Maybe BL.ByteString)  -- ^ an even even more informative arbitrary HTTP response code error.
  deriving (Eq, Ord, Show)

instance Monoid RouteMismatch where
  mempty = NotFound
  -- The following isn't great, since it picks @InvalidBody@ based on
  -- alphabetical ordering, but any choice would be arbitrary.
  --
  -- "As one judge said to the other, 'Be just and if you can't be just, be
  -- arbitrary'" -- William Burroughs
  mappend = max


toApplication :: RoutingApplication
              -> Request
              -> (Response -> IO Response)
              -> IO Response
toApplication ra request respond = do
  liftIO $ putStrLn "TO  APPLICATION" -- TODO delete
  r <- liftIO $ ra request (routingRespond . traceShow' . routeResult)
  putStrLn $ "toApp response: " <> show r
  return r

   where
     routingRespond :: Either RouteMismatch Response -> IO Response
     routingRespond (Left NotFound) =
       respond $ responseLBS notFound404 [] "not found"
     routingRespond (Left WrongMethod) =
       respond $ responseLBS methodNotAllowed405 [] "method not allowed"
     routingRespond (Left (InvalidBody err)) =
       respond $ responseLBS badRequest400 [] $ fromString $ "invalid request body: " ++ err
     routingRespond (Left UnsupportedMediaType) =
       respond $ responseLBS unsupportedMediaType415 [] "unsupported media type"
     routingRespond (Left (HttpError status body)) =
       respond $ responseLBS status [] $ fromMaybe (BL.fromStrict $ statusMessage status) body
     routingRespond (Right response) =
       respond response


responseLBS :: Status -> [(CI B.ByteString, B.ByteString)] -> BL.ByteString -> Response
responseLBS (Status code msg) hs body =
    setResponseStatus code msg
    . (\r -> L.foldl' (\r' (h,h') -> addHeader h h' r') r hs)
    . setResponseBody (I.enumBuilder . fromLazyByteString $ body)
    $ emptyResponse

runAction :: IO (RouteResult (EitherT ServantErr IO a))
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action respond k = do
  r <- action
  go r
  where
    go (RR (Right a))  = do
      e <- runEitherT a
      respond $ case e of
        Right x  -> k x
        Left err -> succeedWith $ responseServantErr err
    go (RR (Left err)) = respond $ failWith err

feedTo :: IO (RouteResult (a -> b)) -> a -> IO (RouteResult b)
feedTo f x = (($ x) <$>) <$> f

extractL :: RouteResult (a :<|> b) -> RouteResult a
extractL (RR (Right (a :<|> _))) = RR (Right a)
extractL (RR (Left err))         = RR (Left err)

extractR :: RouteResult (a :<|> b) -> RouteResult b
extractR (RR (Right (_ :<|> b))) = RR (Right b)
extractR (RR (Left err))         = RR (Left err)

failWith :: RouteMismatch -> RouteResult a
failWith = RR . Left

succeedWith :: a -> RouteResult a
succeedWith = RR . Right

isMismatch :: RouteResult a -> Bool
isMismatch (RR (Left _)) = True
isMismatch _             = False



peekRequestBody :: forall m. MonadIO m => Int64 -> Request -> m B.ByteString
peekRequestBody nMax request =
  do
    (SomeEnumerator enum) <- liftIO $ readIORef (rqBody request)
    consumeStep <- liftIO $ I.runIteratee I.consume
    step <- liftIO $ I.runIteratee $ I.joinI $ I.takeNoMoreThan nMax consumeStep
    body <- liftIO $ fmap B.concat $ I.run_ $ enum step -- TODO run_ is unsafe
    let e = I.enumBS body I.>==> I.joinI . I.take 0
    let e' st =
          do
            let ii = iterateeDebugWrapper "regurgitate body" (I.returnI st)
            st' <- lift $ I.runIteratee ii
            e st'
    liftIO $ writeIORef (rqBody request) $ SomeEnumerator e'
    return body

peekRequestBodyIO :: Int64 -> Request -> IO B.ByteString
peekRequestBodyIO nMax request =
  do
    (SomeEnumerator enum) <- readIORef (rqBody request)
    consumeStep <- I.runIteratee I.consume
    step <- I.runIteratee $ I.joinI $ I.takeNoMoreThan nMax consumeStep
    body <- fmap B.concat $ I.run_ $ enum step -- TODO run_ is unsafe
    let e = I.enumBS body I.>==> I.joinI . I.take 0
    let e' st =
          do
            let ii = iterateeDebugWrapper "regurgitate body" (I.returnI st)
            st' <- lift $ I.runIteratee ii
            e st'
    writeIORef (rqBody request) $ SomeEnumerator e'
    return body
