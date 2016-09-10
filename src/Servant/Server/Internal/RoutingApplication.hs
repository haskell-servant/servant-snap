{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Servant.Server.Internal.RoutingApplication where

import           Control.Applicative                 (Applicative, Alternative(..), (<$>))
import           Control.Monad                       (ap, liftM, MonadPlus(..))
import           Control.Monad.Base
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Builder             as Builder
import qualified Data.ByteString.Lazy                as BL
import           Data.CaseInsensitive                (CI)
import qualified Data.List                           as L
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         (Monoid(..), (<>))
import           Data.Proxy                          (Proxy(..))
import           Data.String                         (fromString)
import           Network.HTTP.Types                  (Status(..))
import qualified System.IO.Streams                   as Streams
import           Servant.API                         ((:<|>) (..))
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

-- -- | A wrapper around @'Either' 'RouteMismatch' a@.
-- newtype RouteResult a =
--   RR { routeResult :: Either RouteMismatch a }
--   deriving (Eq, Show, Functor, Applicative)

-- -- | If we get a `Right`, it has precedence over everything else.
-- --
-- -- This in particular means that if we could get several 'Right's,
-- -- only the first we encounter would be taken into account.
-- instance Monoid (RouteResult a) where
--   mempty = RR $ Left mempty

--   RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
--   RR (Left _)  `mappend` RR (Right y) = RR $ Right y
--   r            `mappend` _            = r

-- -- Note that the ordering of the constructors has great significance! It
-- -- determines the Ord instance and, consequently, the monoid instance.
-- data RouteMismatch =
--     NotFound           -- ^ the usual "not found" error
--   | WrongMethod        -- ^ a more informative "you just got the HTTP method wrong" error
--   | UnsupportedMediaType -- ^ request body has unsupported media type
--   | InvalidBody String -- ^ an even more informative "your json request body wasn't valid" error
--   | HttpError Status (Maybe BL.ByteString)  -- ^ an even even more informative arbitrary HTTP response code error.
--   deriving (Eq, Ord, Show)

-- instance Monoid RouteMismatch where
--   mempty = NotFound
--   -- The following isn't great, since it picks @InvalidBody@ based on
--   -- alphabetical ordering, but any choice would be arbitrary.
--   --
--   -- "As one judge said to the other, 'Be just and if you can't be just, be
--   -- arbitrary'" -- William Burroughs
--   mappend = max


toApplication :: forall m. MonadSnap m => RoutingApplication m -> Application m
toApplication ra request respond = do
  -- r <- ra request (routingRespond . routeResult)
  -- respond r
  r <- ra request routingRespond
  respond r

   where
     routingRespond (Fail err) = respond $ responseServantErr err
     routingRespond (FailFatal err) = respond $ responseServantErr err
     routingRespond (Route v) = respond v
     --routingRespond :: MonadSnap m => Either RouteMismatch Response -> m Response
     -- routingRespond (Left NotFound) =
     --   respond $ responseLBS notFound404 [] "not found"
     -- routingRespond (Left WrongMethod) =
     --   respond $ responseLBS methodNotAllowed405 [] "method not allowed"
     -- routingRespond (Left (InvalidBody err)) =
     --   respond $ responseLBS badRequest400 [] $ fromString $ "invalid request body: " ++ err
     -- routingRespond (Left UnsupportedMediaType) =
     --   respond $ responseLBS unsupportedMediaType415 [] "unsupported media type"
     -- routingRespond (Left (HttpError status body)) =
     --   respond $ responseLBS status [] $ fromMaybe (BL.fromStrict $ statusMessage status) body
     -- routingRespond (Right response) =
     --   respond response


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
  -- r <- action
  -- go r
  -- where
  --   go (RR (Right a))  = do
  --     e <- a
  --     respond $ k e
  --     -- e <- runEitherT a
  --     -- respond $ case e of
  --     --   Right x  -> k x
  --     --   Left err -> succeedWith $ responseServantErr err
  --   go (RR (Left err)) = respond $ failWith err



data Delayed m env c where
  Delayed :: { capturesD :: env -> DelayedM m captures
             , methodD   :: DelayedM m ()
             , authD     :: DelayedM m auth
             , bodyD     :: DelayedM m body
             , serverD   :: captures -> auth -> body -> Request -> RouteResult c
             } -> Delayed m env c

instance Functor (Delayed m env) where
  fmap f Delayed{..} =
    Delayed
      { serverD = \ c a b req -> f <$> serverD c a b req
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

instance MonadBase b m  => MonadBase b (DelayedM m) where
  liftBase = liftBaseDefault

instance Monad m => Alternative (DelayedM m) where
  empty   = DelayedM $ \_ -> return (Fail err404)
  a <|> b = DelayedM $ \req -> do
    respA <- runDelayedM a req
    case respA of
      Route a' -> return $ Route a'
      _ -> runDelayedM b req

instance Monad m => MonadPlus (DelayedM m) where
  mzero = empty
  mplus = (<|>)

{-
instance MonadTransControl (DelayedM) where
  type StT DelayedM a = StT (RouteResult) a
  liftWith = defaultLiftWith DelayedM runDelayedM

instance MonadIO m => MonadBaseControl IO (DelayedM m) where
  type StM  (DelayedM m) a = ComposeSt (DelayedM) IO a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
-}
  


--  Could not deduce (MonadBaseControl IO (DelayedM m))

instance MonadTrans DelayedM where
  lift f = DelayedM $ \_ -> do
    a <- f
    return $ Route a

-- instance MonadSnap m => MonadSnap (DelayedM m) where
--   liftSnap m = DelayedM (const . liftSnap $ Route <$> m)

-- | A 'Delayed' without any stored checks.
emptyDelayed :: Monad m => Proxy (m :: * -> *) -> RouteResult a -> Delayed m env a
emptyDelayed (Proxy :: Proxy m) result =
  Delayed (const r) r r r (\ _ _ _ _ -> result)
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
    , serverD   = \ (x, v) a b req -> ($ v) <$> serverD x a b req
    , ..
    } -- Note [Existential Record Update]

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
    , serverD = \ c (y, v) b req -> ($ v) <$> serverD c y b req
    , ..
    } -- Note [Existential Record Update]

-- | Add a body check to the end of the body block.
addBodyCheck :: Monad m
             => Delayed m env (a -> b)
             -> DelayedM m a
             -> Delayed m env b
addBodyCheck Delayed{..} new =
  Delayed
    { bodyD   = (,) <$> bodyD <*> new
    , serverD = \ c a (z, v) req -> ($ v) <$> serverD c a z req
    , ..
    } -- Note [Existential Record Update]


-- | Add an accept header check to the beginning of the body
-- block. There is a tradeoff here. In principle, we'd like
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
    { bodyD = new *> bodyD
    , ..
    } -- Note [Existential Record Update]

-- | Many combinators extract information that is passed to
-- the handler without the possibility of failure. In such a
-- case, 'passToServer' can be used.
passToServer :: Delayed m env (a -> b) -> (Request -> a) -> Delayed m env b
passToServer Delayed{..} x =
  Delayed
    { serverD = \ c a b req -> ($ x req) <$> serverD c a b req
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
  b <- bodyD
  DelayedM (\ req -> return $ serverD c a b req)


-- feedTo :: MonadSnap m => m (RouteResult (a -> b)) -> a -> m (RouteResult b)
-- feedTo f x = (($ x) <$>) <$> f

-- extractL :: RouteResult (a :<|> b) -> RouteResult a
-- extractL (RR (Right (a :<|> _))) = RR (Right a)
-- extractL (RR (Left err))         = RR (Left err)

-- extractR :: RouteResult (a :<|> b) -> RouteResult b
-- extractR (RR (Right (_ :<|> b))) = RR (Right b)
-- extractR (RR (Left err))         = RR (Left err)

-- failWith :: RouteMismatch -> RouteResult a
-- failWith = RR . Left

-- succeedWith :: a -> RouteResult a
-- succeedWith = RR . Right

-- isMismatch :: RouteResult a -> Bool
-- isMismatch (RR (Left _)) = True
-- isMismatch _             = False
