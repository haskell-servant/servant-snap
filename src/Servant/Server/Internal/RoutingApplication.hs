{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
module Servant.Server.Internal.RoutingApplication where

import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Control.Applicative                 (Applicative, (<$>))
import Control.Monad (liftM)
import           Control.Monad.IO.Class              (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Either          (EitherT, runEitherT)
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as B8
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
import Snap.Internal.Iteratee.Debug as ID

import Debug.Trace

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> Snap Response) -> Snap Response


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


toApplication :: MonadSnap m => RoutingApplication -> Application m
toApplication ra request respond = do
  --liftIO $ putStrLn "TO  APPLICATION" -- TODO delete
  r <- (liftSnap <$> ra) request (routingRespond . routeResult)
  --liftIO $ putStrLn $ "toApp response: " <> show r
  return (undefined r)

   where
     --routingRespond :: MonadSnap m => Either RouteMismatch Response -> m Response
     routingRespond (Left NotFound) =
       respond . traceShow "RR NOTFOUND" $ responseLBS notFound404 [] "not found"
     routingRespond (Left WrongMethod) =
       respond . traceShow "RR NOTALLOWED" $ responseLBS methodNotAllowed405 [] "method not allowed"
     routingRespond (Left (InvalidBody err)) =
       respond . traceShow "RR INVALIDBODY" $ responseLBS badRequest400 [] $ fromString $ "invalid request body: " ++ err
     routingRespond (Left UnsupportedMediaType) =
       respond . traceShow "RR BADMIME" $ responseLBS unsupportedMediaType415 [] "unsupported media type"
     routingRespond (Left (HttpError status body)) =
       respond . traceShow "RR HTTP ERROR" $ responseLBS status [] $ fromMaybe (BL.fromStrict $ statusMessage status) body
     routingRespond (Right response) =
       respond $ traceShow "RR SUCCESS" response


responseLBS :: Status -> [(CI B.ByteString, B.ByteString)] -> BL.ByteString -> Response
responseLBS (Status code msg) hs body =
    setResponseStatus code msg
    . (\r -> L.foldl' (\r' (h,h') -> addHeader h h' r') r hs)
    . setResponseBody (I.enumBuilder . fromLazyByteString $ body)
    $ emptyResponse

runAction :: Snap (RouteResult (EitherT ServantErr Snap a))
          -> (RouteResult Response -> Snap r)
          -> (a -> RouteResult Response)
          -> Snap r
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

feedTo :: Snap (RouteResult (a -> b)) -> a -> Snap (RouteResult b)
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
    liftIO $ putStrLn "IN PEEKBODY"
    (SomeEnumerator enum) <- liftIO $ readIORef (rqBody request)

    consumeStep <- liftIO $ I.runIteratee I.consume
    liftIO $ print "about to get step"
    step <- liftIO $ I.runIteratee $ I.joinI $ I.takeNoMoreThan 15 consumeStep
    liftIO $ print "about to get body"

    eBody <- liftIO $ I.run $ enum step -- TODO run_ is unsafe
    case eBody of
      Left e -> error $ "ERROR: " ++  show e
      Right body' -> do
        let body = B.concat body'
        liftIO $ print "THE WHOLE BODY"
        liftIO $ print body
        let e = I.enumBS body I.>==> I.joinI . I.take 0
        let e' st =
                  do
                    let ii = iterateeDebugWrapper "regurgitate body" (I.returnI st)
                    st' <- lift $ I.runIteratee ii
                    e st'
        liftIO $ writeIORef (rqBody request) $ SomeEnumerator e'
        liftIO $ putStrLn $ "PEEKBODY: " ++ B8.unpack body
        return body

peekRequestBodyIO :: Int64 -> Request -> IO B.ByteString
peekRequestBodyIO nMax request =
  do
    print "IN ITERATEE PART"
    (SomeEnumerator enum) <- readIORef (rqBody request)
    consumeStep <- I.runIteratee I.consume
    step <- I.runIteratee $ I.joinI $ I.takeNoMoreThan nMax consumeStep
    print "ABOUT TO GRAB THE BODY"
    body <- fmap B.concat $ I.run_ $ ID.iterateeDebugWrapperWith show "DEBUG WRAPPER" (enum step) -- TODO run_ is unsafe
    print "BODY READ FINISHED"
    let e = I.enumBS body I.>==> I.joinI . I.take 0
    let e' st =
          do
            let ii = iterateeDebugWrapper "regurgitate body" (I.returnI st)
            st' <- lift $ I.runIteratee ii
            e st'
    writeIORef (rqBody request) $ SomeEnumerator e'
    putStrLn $ "PEEKBODY: " ++ B8.unpack body
    return body
