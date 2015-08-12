{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Error
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class (lift)
import           Control.Lens
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
--import Network.Wai
--import Network.Wai.Handler.Warp
import qualified Data.ByteString.Char8 as BS
import           Servant.Server.Internal.SnapShims
import           Servant.HTML.Blaze
import           Snap.Core
import           Snap.Snaplet
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
  :<|> "template" :> Capture "name" :> Get '[HTML] ()

  -- :<|> Get '[JSON] Greet

  -- :<|> "testraw" :> Raw

  -- :<|> "dir" :> Raw

helloH' :: Text -> Maybe Bool -> EitherT ServantErr (Handler App App) Greet
helloH' name _ = return (Greet $ "Hi from snaplet, " <> name)

doTemplate = undefined
--doTemplate :: Text -> EitherT ServantErr (Handler App App) ()
--doTemplate name = undefined --lift $ render "test" >> return ()

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
server = helloH' :<|> postGreetH :<|> deleteGreetH :<|> doTemplate

  where helloH :: MonadSnap m => Text -> Maybe Bool -> EitherT ServantErr m Greet
        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        --helloH name (Just False) = writeBS ("Hello, " <> name)
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        --postGreetH :: Greet -> EitherT ServantErr (Handler App App) Greet
        postGreetH greet = return greet

        deleteGreetH _ = return ()
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

data App = App {
  _heist :: Snaplet (Heist App)
  }
makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "An example app in servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  wrapSite (\site -> applicationToSnap test)
  return $ App h

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
--runTestServer :: Int -> IO ()
--runTestServer port = simpleHttpServe
--                     (setPort port mempty :: Config Snap ())
--                     (applicationToSnap test :: Snap ())

runTestServer' port = serveSnaplet (setPort port mempty )
                      initApp

-- Put this all to work!
main :: IO ()
main = runTestServer' 8001
