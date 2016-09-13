{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens
import           Data.Aeson
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import qualified Heist.Interpreted as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Http.Server (defaultConfig)
import           Snap.Http.Server.Config (setPort)

import           Servant

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi m =

  -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet


  :<|> "hellosnap" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

  -- POST /greet with a Greet as JSON in the request body,
  --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

  -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()

  :<|> "files" :> Raw
  :<|> "doraw" :> Raw



data App = App {
    _heist :: Snaplet (Heist App)
  , _sess  :: Snaplet SessionManager
  , _auth  :: Snaplet (AuthManager App)
  }
makeLenses ''App

type AppHandler = Handler App App

testApi :: Proxy (TestApi AppHandler)
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT ServantErr IO' monad.


server :: Server (TestApi AppHandler) AppHandler
server = helloH
    :<|> helloH'
    :<|> postGreetH
    :<|> deleteGreetH
    :<|> serveDirectory "static"
    :<|> doRaw

  where helloH :: MonadSnap m => Text -> Maybe Bool -> m Greet
        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        helloH' :: Text -> Maybe Bool -> (Handler App App) Greet
        helloH' name _ = with auth $ do
          cu <- currentUser
          return (Greet $ "Hi from snaplet, " <> name
                  <> ". Login is " <> maybe "No login" (pack . show) cu)

        postGreetH :: Greet -> (Handler App App) Greet
        postGreetH greet = return greet

        deleteGreetH _ = return ()

        -- doRaw :: Server Raw (Handler App App)
        doRaw = with auth $ do
          u <- currentUser
          let spl = "tName" ## I.textSplice (maybe "NoLogin" (pack . show) u)
          renderWithSplices "test" spl


-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
--test :: MonadSnap m => Application m
test :: AppHandler ()
test = serveSnap testApi server


instance HasHeist App where
  heistLens = subSnaplet heist

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "An example app in servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
  a <- nestSnaplet "" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes [("api", test)]
  return $ App h s a


-- Run the server.
--
runTestServer :: Int -> IO ()
runTestServer port = serveSnaplet (setPort port defaultConfig)
                      initApp

-- Put this all to work!
main :: IO ()
main = runTestServer 8001
