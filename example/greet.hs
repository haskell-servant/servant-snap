{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
--import Network.Wai
--import Network.Wai.Handler.Warp
import qualified Data.ByteString.Char8 as BS
import           Servant.Server.Internal.SnapShims
import           Snap.Core hiding (serveDirectory)
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
  -- :<|> Get '[JSON] Greet

  :<|> "testraw" :> Raw

  :<|> "dir" :> Raw

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT ServantErr IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH  :<|> testRaw :<|> (snapToApplication $ writeBS "Ok")

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        --helloH name (Just False) = writeBS ("Hello, " <> name)
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        deleteGreetH _ = return ()
        -- nodeal = return $ Greet "NoDeal"
        --justReq = writeBS "Hello"
        testRaw :: Application
        testRaw = snapToApplication $ getRequest >>= writeBS . BS.pack . show
        --justReq = cs $ mconcat (pathInfo req)

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Int -> IO ()
runTestServer port = simpleHttpServe
                     (setPort port mempty :: Config Snap ())
                     (applicationToSnap test :: Snap ())

-- Put this all to work!
main :: IO ()
main = runTestServer 8001
