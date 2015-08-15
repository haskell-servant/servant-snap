{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class (lift)
import           Control.Lens
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T
import Heist
import qualified Heist.Interpreted as I
import           Servant.Server.Internal.SnapShims
import           Servant.Utils.StaticFiles (serveDirectory)
import           Servant.HTML.Blaze
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Http.Server

import           Servant

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =

       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()

  :<|> "files" :> Raw (Handler App App) (Handler App App ())
  :<|> "doraw" :> Raw (Handler App App) (Handler App App ())

  -- :<|> "template" :> Capture "name" Text :> Get '[HTML] (BS.ByteString)

  -- :<|> Get '[JSON] Greet

  -- :<|> "testraw" :> Raw

  -- :<|> "dir" :> Raw

--doTemplate = undefined
-- doTemplate :: Text -> EitherT ServantErr (Handler App App) Text
-- doTemplate name = EitherT $ fmap Right $ withHeistState (\(hs) -> (undefined :: Handler App App Text))-- (\s -> I.renderTemplate s "test" >>= runTempl)
--   where runTempl Nothing = error "Problem"
--         runTempl (Just (m,b)) = return (T.decodeUtf8 $ toByteString b)
-- doTemplate :: Text -> EitherT ServantErr (Handler App App) BS.ByteString
-- doTemplate name = lift $ do
--   s <- getHeistState
--   m <- I.renderTemplate s "test"
--   case m of
--     Nothing -> error "template error"
--     Just (b,m) -> return $ toByteString $ b


data App = App {
    _heist :: Snaplet (Heist App)
  , _sess  :: Snaplet SessionManager
  , _auth  :: Snaplet (AuthManager App)
  }
makeLenses ''App

helloH' :: Text -> Maybe Bool -> EitherT ServantErr (Handler App App) Greet
helloH' name _ = lift $ with auth $ do
  cu <- currentUser
  return (Greet $ "Hi from snaplet, " <> name <> ". Login is " <> maybe "No login" (pack . show) cu)

--doRaw :: EitherT ServantErr (Handler App App) ()
--doRaw = lift $ writeBS "Hello frow raw!"


testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT ServantErr IO' monad.


--server :: MonadSnap m => Server TestApi m
server :: Server TestApi (Handler App App)
server = helloH' :<|> postGreetH :<|> deleteGreetH :<|> serveDirectory "static" :<|> doRaw -- :<|> (serveDirectory "static")

  where helloH :: MonadSnap m => Text -> Maybe Bool -> EitherT ServantErr m Greet
        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        --helloH name (Just False) = writeBS ("Hello, " <> name)
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH :: Greet -> EitherT ServantErr (Handler App App) Greet
        postGreetH greet = return greet

        deleteGreetH _ = return ()
        doRaw :: Server (Raw (Handler App App) (Handler App App ())) (Handler App App)
        doRaw = lift $ with auth $ do
          u <- currentUser
          let spl = "tName" ## I.textSplice (maybe "NoLogin" (pack . show) u)
          renderWithSplices "test" spl
        -- nodeal = return $ Greet "NoDeal"
        --justReq = writeBS "Hello"
        --testRaw :: Application m
        --testRaw = snapToApplication $ getRequest >>= writeBS . BS.pack . show
        --justReq = cs $ mconcat (pathInfo req)

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
--test :: MonadSnap m => Application m
test :: Application (Handler App App)
test = serve testApi server


instance HasHeist App where
  heistLens = subSnaplet heist

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "An example app in servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  a <- nestSnaplet "" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes [("t", render "test" >> return ())]
  addRoutes [("api", --modifyRequest (\r -> r {rqContextPath = BS.drop 4 (rqContextPath r)}) >>
                     applicationToSnap test)]
  --wrapSite (\site -> applicationToSnap test)
  return $ App h s a

initApp' :: SnapletInit App App
initApp' = makeSnaplet "myapp2" "an example without servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  a <- nestSnaplet "" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes [("t", render "test" >> return ())]
  return $ App h s a

snapCfg = setPort 8001 mempty
-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
--runTestServer :: Int -> IO ()
--runTestServer port = simpleHttpServe
--                     (setPort port mempty :: Config Snap ())
--                     (applicationToSnap test :: Snap ())

runTestServer' :: Int -> IO ()
runTestServer' port = serveSnaplet (setPort port mempty )
                      initApp

-- Put this all to work!
main :: IO ()
main = runTestServer' 8001
