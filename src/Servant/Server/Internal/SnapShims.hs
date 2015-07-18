{-# LANGUAGE OverloadedStrings #-}

module Servant.Server.Internal.SnapShims where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.IORef
import           Snap.Core
import qualified Snap.Iteratee          as I

import           Debug.Trace

traceShow' a = traceShow a a

type Application m = Request -> (Response -> m Response) -> m Response


-- snapToApplication :: MonadSnap m => m () -> Request -> (Response -> m Response) -> m Response
-- snapToApplication snapAction req handler = do
--   traceShow ("SNAPTOAPP REQ: " ++ show req) (return ())
--   Right (_,resp') <- liftIO $ I.run $
--     liftSnap $ runSnap snapHelper
--     (\l -> putStrLn ("LOG: " ++ B8.unpack l))
--     (const $ putStrLn "TIMEOUT") req -- TODO use real logging and timeout functions
--   return resp'
--   where
--     snapHelper = do
--       putRequest (traceShow' req)
--       snapAction
--       res <- getResponse
--       res' <- handler res
--       return res'


--runSnap :: Snap () -> Iteratee IO (Req,Resp)
--Application = Request -> IO Callback -> IO Response

-- serveApplication :: (Request -> (Response -> IO Response) -> IO Response) -> Snap ()
-- serveApplication app = do
--   req <- getRequest
--   respIter <- liftIO snapPart req
--   return undefined
--   where snapPart :: Request -> (Request -> (Response -> IO Response) -> IO Response) -> IO Response
--         snapPart req app = do
--           app req (runSnap (liftIO $ app req))

-- applicationToSnap :: MonadSnap m
--                   => Application m
--                   -> Snap ()
-- applicationToSnap app = do
--   req <- getRequest
--   r <- liftIO $ putStrLn "***RUNNING APP***" >> app req return
--   putResponse r

data Status = Status {
    statusCode    :: Int
  , statusMessage :: B.ByteString
} deriving (Eq, Show, Ord)

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
