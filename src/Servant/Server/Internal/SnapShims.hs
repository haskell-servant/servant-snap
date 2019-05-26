{-# LANGUAGE OverloadedStrings #-}

module Servant.Server.Internal.SnapShims where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import           Data.List             (foldl')
import           Data.IORef
import           Network.HTTP.Types    (Status(..))
import           Snap.Core


type Application m = Request -> (Response -> m Response) -> m Response

snapToApplication :: MonadSnap m => m () -> Application m
snapToApplication snapAction req respond = do
  putRequest req
  snapAction >> getResponse >>= respond

snapToApplication' :: MonadSnap m => m a -> Application m
snapToApplication' snapAction req respond = do
  putRequest req
  snapAction >> getResponse >>= respond

applicationToSnap :: MonadSnap m => Application m -> m ()
applicationToSnap app = do
  req      <- getRequest
  snapResp <- getResponse
  r <- fmap (`addHeaders` headers snapResp) $ app req return
  putResponse r

addHeaders :: HasHeaders a => a -> Headers -> a
addHeaders a hs = foldl' (\xs (k,v) -> addHeader k v xs) a (listHeaders hs)

unSnapMethod :: Method -> B.ByteString
unSnapMethod = B.pack . show

-- data Status = Status {
--     statusCode    :: Int
--   , statusMessage :: B.ByteString
-- } deriving (Eq, Show, Ord)

ok200 :: Status
ok200 = Status 200 "OK"

noContent204 :: Status
noContent204 = Status 204 "No Content"

created201 :: Status
created201 = Status 201 "Created"

badRequest400 :: Status
badRequest400 = Status 400 "Bad Request"

notFound404 :: Status
notFound404 = Status 404 "Not Found"

methodNotAllowed405 :: Status
methodNotAllowed405 = Status 405 "Method Not Allowed"

unsupportedMediaType415 :: Status
unsupportedMediaType415 = Status 415 "Not Found"



-- Emulate a thunk for memoizing the result of an IO action
-- may be useful for dealing with the multiple body reads [issue](https://github.com/haskell-servant/servant/issues/3)
initThunk :: IO a -> IO (IO a)
initThunk act = do
  r <- newIORef Nothing
  return (fromThunk act r)

  where
    fromThunk :: IO a -> IORef (Maybe a) -> IO a
    fromThunk action r = do
      ma <- readIORef r
      maybe forceIt return ma
        where forceIt = do {a  <- action; writeIORef r (Just a); return a}
