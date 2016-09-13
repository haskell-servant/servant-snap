{-# language DeriveFunctor     #-}
{-# language KindSignatures    #-}
{-# language OverloadedStrings #-}

module Servant.Server.Internal.Router where

import           Data.Map                                   (Map)
import qualified Data.Map                                   as M
import           Data.Monoid                                ((<>))
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr
import           Snap.Core

type Router (m :: * -> *) env = Router' m env (RoutingApplication m)

-- | Internal representation of a router.
data Router' (m :: * -> *) env a = --  req app m =
    -- WithRequest   (req -> Router req app m)
    --   -- ^ current request is passed to the router
    StaticRouter  (Map Text (Router' m env a)) [env -> a]
  | CaptureRouter (Router' m (Text, env) a)
  | CaptureAllRouter (Router' m ([Text], env) a)
  | RawRouter (env -> a)
  | Choice (Router' m env a) (Router' m env a)
  deriving (Functor)

pathRouter :: Text -> Router' m env a -> Router' m env a
pathRouter t r = StaticRouter (M.singleton t r) []

leafRouter :: (env -> a) -> Router' m env a
leafRouter l = StaticRouter M.empty [l]


-- | Smart constructor for the choice between routers.
-- We currently optimize the following cases:
--
--   * Two static routers can be joined by joining their maps.
--   * Two dynamic routers can be joined by joining their codomains.
--   * Two 'WithRequest' routers can be joined by passing them
--     the same request and joining their codomains.
--   * A 'WithRequest' router can be joined with anything else by
--     passing the same request to both but ignoring it in the
--     component that does not need it.
--
choice :: MonadSnap m => Router' m env a -> Router' m env a -> Router' m env a
choice (StaticRouter table1 ls1) (StaticRouter table2 ls2) =
  StaticRouter (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice (CaptureRouter router1) (CaptureRouter router2) =
  CaptureRouter (choice router1 router2)
choice router1 (Choice router2 router3) = Choice (choice router1 router2) router3
choice router1 router2 = Choice router1 router2

data RouterStructure =
    StaticRouterStructure (Map Text RouterStructure) Int
  | CaptureRouterStructure RouterStructure
  | RawRouterStructure
  | ChoiceStructure RouterStructure RouterStructure
  deriving (Eq, Show)

routerStructure :: Router' m env a -> RouterStructure
routerStructure (StaticRouter m ls) =
  StaticRouterStructure (fmap routerStructure m) (length ls)
routerStructure (CaptureRouter router) =
  CaptureRouterStructure $
    routerStructure router
routerStructure (CaptureAllRouter router) =
  CaptureRouterStructure $
    routerStructure router
routerStructure (RawRouter _) =
  RawRouterStructure
routerStructure (Choice r1 r2) =
  ChoiceStructure
    (routerStructure r1)
    (routerStructure r2)

-- | Compare the structure of two routers.
--
sameStructure :: Router' m env a -> Router' m env b -> Bool
sameStructure r1 r2 =
  routerStructure r1 == routerStructure r2

-- | Provide a textual representation of the
-- structure of a router.
--
routerLayout :: Router' m env a -> Text
routerLayout router =
  T.unlines (["/"] ++ mkRouterLayout False (routerStructure router))
  where
    mkRouterLayout :: Bool -> RouterStructure -> [Text]
    mkRouterLayout c (StaticRouterStructure m n) = mkSubTrees c (M.toList m) n
    mkRouterLayout c (CaptureRouterStructure r)  = mkSubTree c "<capture>" (mkRouterLayout False r)
    mkRouterLayout c  RawRouterStructure         =
      if c then ["├─ <raw>"] else ["└─ <raw>"]
    mkRouterLayout c (ChoiceStructure r1 r2)     =
      mkRouterLayout True r1 ++ ["┆"] ++ mkRouterLayout c r2

    mkSubTrees :: Bool -> [(Text, RouterStructure)] -> Int -> [Text]
    mkSubTrees _ []             0 = []
    mkSubTrees c []             n =
      concat (replicate (n - 1) (mkLeaf True) ++ [mkLeaf c])
    mkSubTrees c [(t, r)]       0 =
      mkSubTree c    t (mkRouterLayout False r)
    mkSubTrees c ((t, r) : trs) n =
      mkSubTree True t (mkRouterLayout False r) ++ mkSubTrees c trs n

    mkLeaf :: Bool -> [Text]
    mkLeaf True  = ["├─•","┆"]
    mkLeaf False = ["└─•"]

    mkSubTree :: Bool -> Text -> [Text] -> [Text]
    mkSubTree True  pth children = ("├─ " <> pth <> "/") : map ("│  " <>) children
    mkSubTree False pth children = ("└─ " <> pth <> "/") : map ("   " <>) children

-- | Apply a transformation to the response of a `Router`.
tweakResponse :: (RouteResult Response -> RouteResult Response) -> Router m env -> Router m env
tweakResponse f = fmap (\a -> \req cont -> a req (cont . f))

runRouter :: MonadSnap m => Router m () -> RoutingApplication m
runRouter r = runRouterEnv r ()

-- | Interpret a router as an application.
runRouterEnv :: MonadSnap m
             => Router m env
             -> env
             -> RoutingApplication m
runRouterEnv router env request respond = case router of
  StaticRouter table ls -> case processedPathInfo request of
    [] -> runChoice ls env request respond
    [""] -> runChoice ls env request respond
    first : _ | Just router' <- M.lookup first table
              -> let request' = reqSafeTail request
                 in  runRouterEnv router' env request' respond
    _ -> respond $ Fail err404
  CaptureRouter router' ->
    case processedPathInfo request of
      []   -> respond $ Fail err404
      -- This case is to handle trailing slashes.
      [""] -> respond $ Fail err404
      first : _
        -> let request' = reqSafeTail request
           in  runRouterEnv router' (first,env)  request' respond
  CaptureAllRouter router' ->
    let segments = processedPathInfo request
        request'= reqNoPath request
    in  runRouterEnv router' (segments, env) request' respond
  RawRouter app ->
    app env request respond
  Choice r1 r2 ->
    runChoice [runRouterEnv r1, runRouterEnv r2] env request respond


runChoice :: [env -> RoutingApplication m] -> env -> RoutingApplication m
runChoice ls =
  case ls of
    []       -> \ _ _ respond -> respond (Fail err404)
    [r]      -> r
    (r : rs) ->
      \ env request respond ->
      r env request $ \ response1 ->
      case response1 of
        Fail _ -> runChoice rs env request $ \ response2 ->
          respond $ highestPri response1 response2
        _      -> respond response1
  where
    highestPri (Fail e1) (Fail e2) =
      if worseHTTPCode (errHTTPCode e1) (errHTTPCode e2)
        then Fail e2
        else Fail e1
    highestPri (Fail _) y = y
    highestPri x _ = x

-- Priority on HTTP codes.
--
-- It just so happens that 404 < 405 < 406 as far as
-- we are concerned here, so we can use (<).
worseHTTPCode :: Int -> Int -> Bool
worseHTTPCode = (<)
