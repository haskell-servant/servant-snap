{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Server.Internal.EnterSpec where

import qualified Control.Category              as C
import           Control.Monad.Reader
import           Data.Aeson                    (decode')
import qualified Data.ByteString.Char8         as B8
import           Data.Proxy
import           Servant.API
import           Servant.Server                hiding (route)
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Snaplet
import           Test.Hspec                    (Spec, describe, it)
import           Test.Hspec.Core.Spec          (Result(Fail))
import           Test.Hspec.Snap

type AppHandler = Handler App App

data App = App

app :: SnapletInit App App
app = makeSnaplet "servantsnap" "A test app for servant-snap" Nothing $
      return App

app' :: HasServer api => Proxy api -> Server api AppHandler -> SnapletInit App App
app' p s = makeSnaplet "servantsnap'" "A test app for servant-snap" Nothing $ do
  addRoutes [("", serveSnap p s)]
  return App

routes :: HasServer api
       => Proxy api
       -> Server api AppHandler
       -> [(B8.ByteString, AppHandler ())]
routes p s = [("", serveSnap p s)]


spec :: Spec
spec = return ()

{-
type ReaderAPI = "int"    :> Get '[JSON] Int
            :<|> "string" :> Post '[JSON] String

type IdentityAPI = "bool" :> Get '[JSON] Bool

type CombinedAPI = ReaderAPI :<|> IdentityAPI

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

readerServer' :: ServerT ReaderAPI (Reader String)
readerServer' = return 1797 :<|> ask

fReader :: Reader String :~> AppHandler
fReader = generalizeNat C.. runReaderTNat "hi"

readerServer :: Server ReaderAPI AppHandler
readerServer = enter fReader readerServer'

combinedReaderServer' :: ServerT CombinedAPI (Reader String)
combinedReaderServer' = readerServer' :<|> enter generalizeNat (return True)

combinedReaderServer :: Server CombinedAPI AppHandler
combinedReaderServer = enter fReader combinedReaderServer'

shouldDecodeTo (Json _ bs) v = decode' bs `shouldEqual` Just v
shouldDecodeTo _           _ = setResult (Fail Nothing "Should have been json body")

enterSpec :: Spec
enterSpec =
  --snap (route (routes readerAPI readerServer)) app $
  snap (route (routes combinedAPI combinedReaderServer)) (app' combinedAPI combinedReaderServer) $
    describe "Enter" $ do

      it "allows getting in arbitrary monads" $
        get "int" >>= flip shouldDecodeTo (1797 :: Int)

      it "allows posting in arbitrary monads" $
        postJson "string" ("3" :: String) >>= shouldEqual (Other 201)

      --before (return (serve combinedAPI combinedReaderServer)) $ do
      it "allows combnation of enters" $
        flip shouldDecodeTo True =<< get "bool"
-}
