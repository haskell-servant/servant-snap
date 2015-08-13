{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Server.Internal.EnterSpec where

import qualified Control.Category              as C
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Proxy
import           Servant.API
import           Servant.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Snaplet
import           Test.Hspec                    (Spec, describe, it, before)
--import           Test.Hspec.Wai                (get, matchStatus, post,
--                                                shouldRespondWith, with)
import           Test.Hspec.Snap

-- data App = App

-- app :: SnapletInit App App
-- app = makeSnaplet "Enterapp" "A servant-snap test example app" Nothing $ do
--   return App

spec :: Spec
spec = describe "module Servant.Server.Enter" $ do
  return ()
--     enterSpec

-- type ReaderAPI = "int" :> Get '[JSON] Int
--             :<|> "string" :> Post '[JSON] String

-- type IdentityAPI = "bool" :> Get '[JSON] Bool

-- type CombinedAPI = ReaderAPI :<|> IdentityAPI

-- readerAPI :: Proxy ReaderAPI
-- readerAPI = Proxy

-- combinedAPI :: Proxy CombinedAPI
-- combinedAPI = Proxy

-- readerServer' :: ServerT ReaderAPI (Reader String)
-- readerServer' = return 1797 :<|> ask

-- fReader :: Reader String :~> EitherT ServantErr (Handler App App)
-- fReader = generalizeNat C.. (runReaderTNat "hi")

-- readerServer :: Server ReaderAPI (Handler App App)
-- readerServer = enter fReader readerServer'

-- combinedReaderServer' :: ServerT CombinedAPI (Reader String)
-- combinedReaderServer' = readerServer' :<|> enter generalizeNat (return True)

-- combinedReaderServer :: Server CombinedAPI Snap
-- combinedReaderServer = enter fReader combinedReaderServer'

-- enterSpec :: Spec
-- enterSpec = describe "Enter" $ do
--   beforeEval (return (serve readerAPI readerServer)) $ do

--     it "allows running arbitrary monads" $ do
--       get "int" `shouldEqual` Html "1797"
--       postJson "string" "3" `shouldEqual` Other 201

--   --before (return (serve combinedAPI combinedReaderServer)) $ do
--     it "allows combnation of enters" $ do
--       shouldBeTrue $ get "bool"
