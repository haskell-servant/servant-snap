# servant-snap

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

A library for running [servant](https://haskell-servant.github.io) APIs under any [MonadSnap](http://snapframework.com) monad. For example, we can build a `Servant` server from `Snap` handlers:


```
> :set -XTypeOperators -XDataKinds -XOverloadedStrings
> import Data.Proxy (Proxy(..))
> import Data.Text (Text, append)
> import Servant
> import Servant.Server (serveSnap)
> import Snap.Http.Server (quickHttpServe)
> type Api = "number" :> Get '[JSON] Int  :<|>  "hello" :> Capture "name" String :> Get '[JSON] String)
> let api = Proxy :: Proxy Api
> quickHttpServe $ serveSnap api (return 5 :<|> \name -> return (append "Hello, " name))
```

Or you can integrate a `Servant` API into more complex application with [`Snaplets`](https://hackage.haskell.org/package/snap-1.0.0.0/docs/Snap-Snaplet.html) and your own `Handler b v` monad, using lenses to move between subsnaplets as you would in any other Snap handler.

There is a longer example demonstrating this below, and a compileable demo at [example/greet.hs](https://github.com/haskell-servant/servant-snap/blob/master/example/greet.hs).

## Comparison with `servant-server`

`servant-snap` is very similar to `servant-server`, providing a number of `HasServer` instances for various `servant` API combinators.

`servant-snap` hosts `MonadSnap m` handlers (for example `Snap`, `Handler b v`). `servant-server` hosts `EitherT ServantErr IO` handlers and other monads that can be naturally transformed into that monad through [`enter`](http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers).
This makes `servant-snap` a bit more flexible and less type-safe, because `servant-snap` handlers can access data from the HTTP request. `servant-server` hides this information from the handlers so that all routing decisions are determined by the API type alone.

`MonadSnap m` handlers use lenses and snaplets to control which effects and state the handler can access.
In `servant-server`, you use `vault` to hold needed bits of state (for example, database connections).

`servant-server` provides combinators for controlling access to sub-api's. `servant-snap` still does authentication through the [`auth snaplet`](https://hackage.haskell.org/package/snap-1.0.0.0/docs/Snap-Snaplet-Auth.html).
We are still thinking about how to support type-level signalling of auth requirements for `servant-snap`.
It would be possible to provide `snap`-specific auth combinators, but using them would force a `snap` dependency on the API itself, which we want to avoid (it is common to compile a `servant` API with `ghcjs`, so we want the API to be packaged without any system dependencies).

## FAQ


### If this package is called `servant-snap`, shouldn't `servant-server` be named `servant-wai`? The current naming is confusing.

At some point, we hope to split `servant-server` into a generic base part and an `servant-wai` backend. This would make writing other backends easier and drastically reduce code duplication.


### Can `servant-snap` serve Heist templates?

This would be a nice feature. Although it may be better as a separate package (because we don't want a `servant` API to be forced to depend on `servant-snap` by using `servant-snap` combinators). Right now, `servant-snap` applications either put heist template rendering behind one of `servant`'s `Raw` endpoints, or simple route to them outside of the `servant` API.


### What does Snap offer beyond a regular `servant-server` application?

Snap has a very simple core, a nice monad for request/response handling, and a large number of stable units of added functionality ("Snaplets") for handling auth, persistence, templating, etc. One of the biggest pain-points of Snap use is manually digging data out of requests, validating it, and building responses. Servant fills this niche for snap perfectly. Read more about the [Snap Framework](http://www.snapframework.com)!


### Is anyone using `servant-snap` in production?

For now, only its author :) You can find examples of 


```
-- Top-level Application State
data App = App
  { _heist :: Snaplet (Heist App)
  , _sess  :: Snaplet SessionManager
  , _db    :: Snaplet Postgres
  , _auth  :: Snaplet (AuthManager App)
  }

makeLenses'' App

instance HasHeist App where
  heistLens = subSnaplet heist


-- Our app's base monad
type AppHandler = Handler App App


-- Our Servant API
type Api = "users" :> Get '[JSON] [User]
           :<|>
           "user" :> Capture "name" Text :> Get '[JSON] User
           :<|>
           "reset_db" :> Post '[JSON] NoContent


-- Servant API implementation using DB and Auth Snaplets
apiServer :: ServerT Api AppHandler
apiServer = getAllUsers :<|> getUserByName :<|> resetDB
  where
    getAllUsers =
      with db $ query_ "SELECT * FROM users"

    getUserByName name =
      with db $ query  "SELECT * FROM users WHERE name=(?)" [(Only name)]

    resetDB = do
      isAdmin <- with auth $ do
        u <- getCurrentUser
        return ((userLogin <$> u) == Just "admin")
      if isAdmin
      then with db $ query_ "DELETE FROM users"
      else throwError err403
          

-- Snap initializer action
-- (Notice that we mix servant and non-servant routes)
app :: SnapletInit App App
app =  makeSnaplet "app" "standard example app" Nothing $ do
  h <- nestSnaplet "" heist $  heistInit "templates"
  s <- nestSnaplet "sess" sess $
    initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
  d <- nestSnaplet "db" db pgsInit
  a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
  addRoutes [ ("login",     with auth handleLoginSubmit)    -- Regular snap handler
            , ("logout",    with auth handleLogout)         -- for non-API routes
            , ("new_user",  with auth handleNewUser)
            , ("api",       serveSnap api apiServer         -- servant-snap handler
            , ("",          serveDirectory "static")
            ]
  addAuthSplices h auth
  return $ App h s d a

main :: IO ()
main = httpServe defaultConfig app
```

/

Snaplet-capable [snap](http://www.snapframework.com) server for [servant](http://github.com/haskell-servant) APIs.

This library lets you *implement* an HTTP server with handlers for each endpoint of a servant API, handling most of the boilerplate for you.


```
cabal install --only-dep
cabal build
dist/build/greet/greet
curl localhost:8001/api/hello/DearUser
```

