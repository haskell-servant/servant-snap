{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens
import           Data.Aeson hiding (defaultOptions)
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import qualified Heist.Interpreted as I
import           Snap.Core hiding (GET)
import           Snap.CORS
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Http.Server (defaultConfig)

import           Servant.API
import           Servant (serveSnap, Server, serveDirectory)

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi m f =

  -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet


  :<|> "hellosnap" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

  -- POST /greet with a Greet as JSON in the request body,
  --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

  -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()

  :<|> "stream" :> StreamGet NetstringFraming JSON (f Greet)

  :<|> "files" :> Raw
  :<|> "doraw" :> Raw


-- Our application has some of the usual Snaplets
data App = App {
    _heist :: Snaplet (Heist App)
  , _sess  :: Snaplet SessionManager
  , _auth  :: Snaplet (AuthManager App)
  }
makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

type AppHandler = Handler App App

testApi :: Proxy (TestApi AppHandler StreamGenerator)
testApi = Proxy


-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'AppHandler' monad.

server :: Server (TestApi AppHandler StreamGenerator) '[] AppHandler
server = helloH
    :<|> helloH'
    :<|> postGreetH
    :<|> deleteGreetH
    :<|> doStream
    :<|> serveDirectory "static"
    :<|> doRaw

  where helloH :: Text -> Maybe Bool -> AppHandler Greet
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

        doStream = return $ StreamGenerator $ \sendFirst sendRest -> do
          sendFirst (Greet "hi")
          sendRest  (Greet "tao")
          sendRest  (Greet "Howareya")

        doRaw = with auth $ do
          u <- currentUser
          let spl = "tName" ## I.textSplice (maybe "NoLogin" (pack . show) u)
          renderWithSplices "test" spl


initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "An example app in servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
  a <- nestSnaplet "" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes [("api", applyCORS defaultOptions $ serveSnap testApi server)
            ,("",    writeText "Hello")]
  return $ App h s a


-- Run the server.
main :: IO ()
main = serveSnaplet defaultConfig initApp

