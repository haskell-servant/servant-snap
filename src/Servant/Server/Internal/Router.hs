module Servant.Server.Internal.Router where

import           Data.Map                                   (Map)
import qualified Data.Map                                   as M
import           Data.Monoid                                ((<>))
import           Data.Text                                  (Text)
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.RoutingApplication
import           Snap.Core


-- | Internal representation of a router.
data Router req app m =
    WithRequest   (req -> Router req app m)
      -- ^ current request is passed to the router
  | StaticRouter  (Map Text (Router req app m))
      -- ^ first path component used for lookup and removed afterwards
  | DynamicRouter (Text -> Router req app m)
      -- ^ first path component used for lookup and removed afterwards
  | LeafRouter    (RoutingApplication m)
      -- ^ to be used for routes that match an empty path
  | Choice        (Router req app m) (Router req app m)
      -- ^ left-biased choice between two routers

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
choice :: MonadSnap m => Router req app m -> Router req app m -> Router req app m
choice (StaticRouter table1) (StaticRouter table2) =
  StaticRouter (M.unionWith choice table1 table2)
choice (DynamicRouter fun1)  (DynamicRouter fun2)  =
  DynamicRouter (\ first -> choice (fun1 first) (fun2 first))
choice (WithRequest router1) (WithRequest router2) =
  WithRequest (\ request -> choice (router1 request) (router2 request))
choice (WithRequest router1) router2 =
  WithRequest (\ request -> choice (router1 request) router2)
choice router1 (WithRequest router2) =
  WithRequest (\ request -> choice router1 (router2 request))
choice router1 router2 = Choice router1 router2

-- | Interpret a router as an application.
runRouter :: MonadSnap m
          => Router Request (RoutingApplication m) m
          -> RoutingApplication m
runRouter (WithRequest router) request respond =
  runRouter (router request) request respond
runRouter (StaticRouter table) request respond =
  case processedPathInfo request of
    first : _
      | Just router <- M.lookup first table
      -> let request' = reqSafeTail request
         in  runRouter router request' respond
    _ -> respond $ failWith NotFound
runRouter (DynamicRouter fun)  request respond =
  case processedPathInfo request of
    first : _
      -> let request' = reqSafeTail request
         in  runRouter (fun first) request' respond
    _ -> respond $ failWith NotFound
runRouter (LeafRouter app)     request respond = app request respond
runRouter (Choice r1 r2)       request respond =
  runRouter r1 request $ \ mResponse1 ->
    if isMismatch mResponse1
      then runRouter r2 request $ \ mResponse2 ->
             respond (mResponse1 <> mResponse2)
      else respond mResponse1
