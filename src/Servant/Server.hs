{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module lets you implement 'Server's for defined APIs. You'll
-- most likely just need 'serve'.
module Servant.Server
  ( -- * Run a snap handler from an API
    serveSnap
  , serveSnapWithContext

  , -- * Handlers for all standard combinators
    HasServer(..)
  , Server

  , -- * Reexports
    module Servant.Server.Internal.BasicAuth
  , module Servant.Server.Internal.Context

    -- ** Basic functions and datatypes

    -- * Default error type
  , ServantErr(..)
  , throwError
    -- ** 3XX
  , err300
  , err301
  , err302
  , err303
  , err304
  , err305
  , err307
    -- ** 4XX
  , err400
  , err401
  , err402
  , err403
  , err404
  , err405
  , err406
  , err407
  , err409
  , err410
  , err411
  , err412
  , err413
  , err414
  , err415
  , err416
  , err417
   -- * 5XX
  , err500
  , err501
  , err502
  , err503
  , err504
  , err505

  ) where

import           Data.Proxy                        (Proxy(..))
import           Servant.Server.Internal
import           Servant.Server.Internal.BasicAuth
import           Servant.Server.Internal.Context
import           Servant.Server.Internal.SnapShims
import           Snap.Core                         hiding (route)


-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
--
-- Example:
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > app :: Application
-- > app = serve myApi server
-- >
-- > main :: IO ()
-- > main = Network.Wai.Handler.Warp.run 8080 app
--

serveApplication
  :: forall layout context m.(HasServer layout context m, MonadSnap m)
  => Proxy layout
  -> Context context
  -> Server layout context m
  -> Application m
serveApplication p ctx server = toApplication (runRouter (route p ctx (emptyDelayed (Proxy :: Proxy (m :: * -> *)) ((Route server)))))

serveSnapWithContext
  :: forall layout context m.(HasServer layout context m, MonadSnap m)
  => Proxy layout
  -> Context context
  -> Server layout context m
  -> m ()
serveSnapWithContext p ctx server = applicationToSnap $ serveApplication p ctx server

serveSnap
  :: forall layout m.(HasServer layout '[] m, MonadSnap m)
  => Proxy layout
  -> Server layout '[] m
  -> m ()
serveSnap p server = serveSnapWithContext p EmptyContext server