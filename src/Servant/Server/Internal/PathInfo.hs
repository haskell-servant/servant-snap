{-# LANGUAGE OverloadedStrings #-}
module Servant.Server.Internal.PathInfo where

import qualified Data.ByteString.Char8 as B
import           Data.List             (unfoldr)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Snap.Core


rqPath :: Request -> B.ByteString
rqPath r = B.append (rqContextPath r) (rqPathInfo r)

pathInfo :: Request -> [Text]
pathInfo = T.splitOn "/" . T.decodeUtf8 . rqPathInfo


pathSafeTail :: Request -> ([B.ByteString], [B.ByteString])
pathSafeTail r =
  let contextParts = B.split '/' (rqContextPath r)
      restParts    = B.split '/' (rqPathInfo r)
  in (contextParts, drop 1 restParts)


-- TODO: Is this right? Does it drop leading/trailing slashes?
reqSafeTail :: Request -> Request
reqSafeTail r = let (ctx,inf) = pathSafeTail r
                in  r { rqContextPath = B.intercalate "/" ctx
                      , rqPathInfo    = B.intercalate "/" inf
                      }

reqNoPath :: Request -> Request
reqNoPath r = r {rqPathInfo = ""}

-- | Like `null . pathInfo`, but works with redundant trailing slashes.
pathIsEmpty :: Request -> Bool
pathIsEmpty = f . processedPathInfo
  where
    f []   = True
    f [""] = True
    f _    = False


splitMatrixParameters :: Text -> (Text, Text)
splitMatrixParameters = T.break (== ';')

parsePathInfo :: Request -> [Text]
parsePathInfo = filter (/= "") . mergePairs . map splitMatrixParameters . pathInfo
  where mergePairs = concat . unfoldr pairToList
        pairToList []          = Nothing
        pairToList ((a, b):xs) = Just ([a, b], xs)

-- | Returns a processed pathInfo from the request.
--
-- In order to handle matrix parameters in the request correctly, the raw pathInfo needs to be
-- processed, so routing works as intended. Therefor this function should be used to access
-- the pathInfo for routing purposes.
processedPathInfo :: Request -> [Text]
processedPathInfo r =
  case pinfo of
    (x:xs) | T.head x == ';' -> xs
    _                        -> pinfo
  where pinfo = parsePathInfo r
