{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Servant.Server.Internal.RoutingApplication where

import           Control.Applicative                 (Applicative(..), Alternative(..), (<$>))
import           Control.Monad                       (ap, liftM)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Trans.Class
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Builder             as Builder
import qualified Data.ByteString.Lazy                as BL
import           Data.CaseInsensitive                (CI)
import qualified Data.List                           as L
import           Data.Proxy                          (Proxy(..))
import           Network.HTTP.Types                  (Status(..))
import qualified System.IO.Streams                   as Streams
import           Servant.Server.Internal.SnapShims
import           Servant.Server.Internal.ServantErr
import           Snap.Core
import           Snap.Internal.Http.Types            (setResponseBody)


type RoutingApplication m =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> m Response) -> m Response


-- | The result of matching against a path in the route tree.
data RouteResult a =
    Fail ServantErr           -- ^ Keep trying other paths. The @ServantErr@
                              -- should only be 404, 405 or 406.
  | FailFatal !ServantErr     -- ^ Don't try other paths.
  | Route !a
  deriving (Eq, Show, Read, Functor)


toApplication :: forall m. MonadSnap m => RoutingApplication m -> Application m
toApplication ra request respond = do

  snapReq  <- getRequest
  r        <- ra request routingRespond
  snapResp <- getResponse
  rspnd <- respond (r `addHeaders` headers snapResp)

  -- liftIO $ putStrLn $ unlines [
  --   "----------"
  --   , "SNAP REQ"
  --   , show snapReq
  --   , "----------"
  --   , "request"
  --   , show request
  --   , "----------"
  --   , "r"
  --   , show r
  --   , "----------"
  --   , "snapResp"
  --   , show snapResp
  --   , "----------"
  --   , "rspnd"
  --   , show rspnd
  --   ]
  
  return rspnd

  -- snapReq  <- getRequest
  -- r        <- ra (request `addHeaders` headers snapReq) routingRespond
  -- respond r

  -- r <- ra request routingRespond
  -- respond r

   where
     routingRespond (Fail err) = case errHTTPCode err of
       404 -> pass
       _   -> respond $ responseServantErr err
     routingRespond (FailFatal err) = respond $ responseServantErr err
     routingRespond (Route v) = respond v

responseLBS :: Status -> [(CI B.ByteString, B.ByteString)] -> BL.ByteString -> Response
responseLBS (Status code msg) hs body =
    setResponseStatus code msg
    . (\r -> L.foldl' (\r' (h,h') -> addHeader h h' r') r hs)
    . setResponseBody (\out -> do
       Streams.write (Just $ Builder.lazyByteString body) out
       return out)
    $ emptyResponse

runAction :: MonadSnap m
          => Delayed m env (m a)
          -> env
          -> Request
          -> (RouteResult Response -> m r)
          -> (a -> RouteResult Response)
          -> m r
runAction action env req respond k = do
  runDelayed action env req >>= go  >>= respond
  where
    go (Fail e) = return $ Fail e
    go (FailFatal e) = return $ FailFatal e
    go (Route a) = do
      e <- a
      return $ k e


data Delayed m env c where
  Delayed :: { capturesD :: env -> DelayedM m captures
             , methodD   :: DelayedM m ()
             , authD     :: DelayedM m auth
             , acceptD   :: DelayedM m ()
             , contentD  :: DelayedM m contentType
             , paramsD   :: DelayedM m params
             , headersD  :: DelayedM m headers
             , bodyD     :: contentType -> DelayedM m body
             , serverD   :: captures -> params -> headers -> auth -> body -> Request -> RouteResult c
             } -> Delayed m env c

instance Functor (Delayed m env) where
  fmap f Delayed{..} =
    Delayed
      { serverD = \ c p h a b req -> f <$> serverD c p h a b req
      , ..
      }

newtype DelayedM m a = DelayedM { runDelayedM :: Request -> m (RouteResult a) }

instance Monad m => Functor (DelayedM m) where
  fmap = liftM

instance Monad m => Applicative (DelayedM m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (DelayedM m) where
  return x = DelayedM (const $ return (Route x))
  DelayedM m >>= f =
    DelayedM $ \ req -> do
      r <- m req
      case r of
        Fail      e -> return $ Fail e
        FailFatal e -> return $ FailFatal e
        Route     a -> runDelayedM (f a) req

instance MonadIO m => MonadIO (DelayedM m) where
  liftIO m = DelayedM (const . liftIO $ Route <$> m)

instance (Monad m, MonadSnap m) => Alternative (DelayedM m) where
  empty   = DelayedM $ \_ -> return (Fail err404)
  a <|> b = DelayedM $ \req -> do
    respA <- runDelayedM a req
    case respA of
      Route a' -> return $ Route a'
      _ -> runDelayedM b req


instance MonadTrans DelayedM where
  lift f = DelayedM $ \_ -> do
    a <- f
    return $ Route a


-- | A 'Delayed' without any stored checks.
emptyDelayed :: Monad m => Proxy (m :: * -> *) -> RouteResult a -> Delayed m env a
emptyDelayed (Proxy :: Proxy m) result =
  Delayed (const r) r r r r r r (const r) (\ _ _ _ _ _ _ -> result)
  where
    r :: DelayedM m ()
    r = return ()

-- | Fail with the option to recover.
delayedFail :: Monad m => ServantErr -> DelayedM m a
delayedFail err = DelayedM (const $ return $ Fail err)

-- | Fail fatally, i.e., without any option to recover.
delayedFailFatal :: Monad m => ServantErr -> DelayedM m a
delayedFailFatal err = DelayedM (const $ return $ FailFatal err)

-- | Gain access to the incoming request.
withRequest :: (Request -> DelayedM m a) -> DelayedM m a
withRequest f = DelayedM (\ req -> runDelayedM (f req) req)

-- | Add a capture to the end of the capture block.
addCapture :: forall env a b captured m. Monad m => Delayed m env (a -> b)
           -> (captured -> DelayedM m a)
           -> Delayed m (captured, env) b
addCapture Delayed{..} new =
  Delayed
    { capturesD = \ (txt, env) -> (,) <$> capturesD env <*> new txt
    , serverD   = \ (x, v) p h a b req -> ($ v) <$> serverD x p h a b req
    , ..
    } -- Note [Existential Record Update]

-- | Add a parameter check to the end of the params block
addParameterCheck :: Monad m
               => Delayed m env (a -> b)
               -> DelayedM m a
               -> Delayed m env b
addParameterCheck Delayed {..} new =
  Delayed
    { paramsD = (,) <$> paramsD <*> new
    , serverD = \c (p, pNew) h a b req -> ($ pNew) <$> serverD c p h a b req
    , ..
    }

-- | Add a header check to the end of the headers block
addHeaderCheck :: Monad m
               => Delayed m env (a -> b)
               -> DelayedM m a
               -> Delayed m env b
addHeaderCheck Delayed {..} new =
  Delayed
    { headersD = (,) <$> headersD <*> new
    , serverD = \c p (h, hNew) a b req -> ($ hNew) <$> serverD c p h a b req
    , ..
    }

-- | Add a method check to the end of the method block.
addMethodCheck :: Monad m
               => Delayed m env a
               -> DelayedM m ()
               -> Delayed m env a
addMethodCheck Delayed{..} new =
  Delayed
    { methodD = methodD <* new
    , ..
    } -- Note [Existential Record Update]

-- | Add an auth check to the end of the auth block.
addAuthCheck :: Monad m
             => Delayed m env (a -> b)
             -> DelayedM m a
             -> Delayed m env b
addAuthCheck Delayed{..} new =
  Delayed
    { authD   = (,) <$> authD <*> new
    , serverD = \ c p h (y, v) b req -> ($ v) <$> serverD c p h y b req
    , ..
    } -- Note [Existential Record Update]

-- | Add a content type and body checks around parameter checks.
--
-- We'll report failed content type check (415), before trying to parse
-- query parameters (400). Which, in turn, happens before request body parsing.
addBodyCheck :: Monad m
             => Delayed m env (a -> b)
             -> DelayedM m c        -- ^ content type check
             -> (c -> DelayedM m a) -- ^ body check
             -> Delayed m env b
addBodyCheck Delayed{..} newContentD newBodyD =
  Delayed
    { contentD = (,) <$> contentD <*> newContentD
    , bodyD   = \(content, c) -> (,) <$> bodyD content <*> newBodyD c
    , serverD = \ c p h a (z, v) req -> ($ v) <$> serverD c p h a z req
    , ..
    } -- Note [Existential Record Update]


-- | Add an accept header check before handling parameters.
-- In principle, we'd like
-- to take a bad body (400) response take precedence over a
-- failed accept check (406). BUT to allow streaming the body,
-- we cannot run the body check and then still backtrack.
-- We therefore do the accept check before the body check,
-- when we can still backtrack. There are other solutions to
-- this, but they'd be more complicated (such as delaying the
-- body check further so that it can still be run in a situation
-- where we'd otherwise report 406).
addAcceptCheck :: Monad m
               => Delayed m env a
               -> DelayedM m ()
               -> Delayed m env a
addAcceptCheck Delayed{..} new =
  Delayed
    { acceptD = acceptD *> new
    , ..
    } -- Note [Existential Record Update]

-- | Many combinators extract information that is passed to
-- the handler without the possibility of failure. In such a
-- case, 'passToServer' can be used.
passToServer :: Delayed m env (a -> b) -> (Request -> a) -> Delayed m env b
passToServer Delayed{..} x =
  Delayed
    { serverD = \ c p h a b req -> ($ x req) <$> serverD c p h a b req
    , ..
    } -- Note [Existential Record Update]


-- | Run a delayed server. Performs all scheduled operations
-- in order, and passes the results from the capture and body
-- blocks on to the actual handler.
--
-- This should only be called once per request; otherwise the guarantees about
-- effect and HTTP error ordering break down.
runDelayed :: Monad m
           => Delayed m env a
           -> env
           -> Request
           -> m (RouteResult a)
runDelayed Delayed{..} env = runDelayedM $ do
  c <- capturesD env
  methodD
  a <- authD
  acceptD
  content <- contentD
  p <- paramsD  -- Has to be before body parsing, but after content-type checks
  h <- headersD
  b <- bodyD content
  DelayedM (\ req -> return $ serverD c p h a b req)
