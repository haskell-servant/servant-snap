# Several modules only needed to have their Wai `Request` replaced by snap-core's `Request`

# Where Wai `Application` shows up, replace by MonadSnap m => m ()

# `pathInfo` doesn't exist in Snap world, but it's easy to write

# I dropped a lot of CPP flags... to appease ghc-mod. May have to bring them back!

# First major hang-up is in Servant.Server.Internal.RoutingApplication

 - Memoization trick works now over Wai's kind of implicit ByteString fragment generator thing.
 - snap-core uses a bona-fide `Enumerator` pre-1.0, and io-streams `InputStream` post-1.0
 - snap 1.0 isn't yet released, but mostly works and is on github
 - snap-core doesn't expose the Request constructor, for safety. No easy way to muck wit the internal InputSource
 - Right now, I'm simply going to patch snap-core to export (rqBody :: Request -> InputStream ByteString), so we can play w/ the stream.
 - Waiting to hear back from gcollins on alternatives
 - May be even better to consider ways of elimininating the multiple-body-read. Is it possible? Even when we end in servant's `Raw` route?

 - UPDATE:
 - You can mess with the Enumerator/InputStream simply by importing a .Internal from snap-core
 - [Example](https://github.com/snapframework/snap-server/blob/master/src/Snap/Internal/Http/Server/Session.hs#L423): snap-server plays with the stream when processing forms that ar x-www-form-encoded
 - We can just read the first N bytes into a bytestring from the iostream, pass it through the routing machinery, and build a new iostream to put back into the request for use by the handler

# Wai and Snap differ in how they store request path. Wai just has a list of path parts. Snap-core has a 'rqPathContext' and a 'rqPathInfo', each is a bytestring with '/'s inside delimiting components. Patched over this in `Servant.Server.Internal.PathInfo` with the functions `pathInfo` and `pathSafeTail`

# Hmm. Snap doesn't seem to parse matrix params.. troublesome for Internal.hs. Will continue to depend on Wai for `parseQueryText` until we can replace it in snap-core or servant-server

# Look at uri-bytestring. Once this gets matrix param supporte, we can use it instead of the Network.URI parser

# Application <-> Snap ()
I believe I need a function `snapServeApplication :: (Request -> (Respsonse -> IO Response) -> IO Response) -> Snap Response`
