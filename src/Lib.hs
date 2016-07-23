{-# LANGUAGE OverloadedStrings #-}

-- | A library to do stuff.
module Lib
    (
      me
    , defaultOptions
    , setToken
    ) where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type URL = String

-- | get info about the authenticated user
me :: Options -> IO T.Text
me options = do
    print options
    r <- getWith' options "https://www.pivotaltracker.com/services/v5/me"
    print (r ^. responseStatus)
    case (r ^. responseStatus . statusCode) of
        401 -> return $ handle401 r
        _ -> return (r ^. responseBody . _String)

getWith' :: Options -> URL -> IO (Response L.ByteString)
getWith' options url = getWith (setCheckStatus defaults) url

defaultOptions = setCheckStatus defaults

setCheckStatus :: Options -> Options
setCheckStatus options =
    options & checkStatus .~ (Just $ \_ _ _ -> Nothing)

setToken :: B.ByteString -> Options -> Options
setToken token options = options & header "X-TrackerToken" .~ [token]

handle401 response = T.intercalate " " [ errorMsg response, possibleFix response ]
  where
    extract r keyName = r ^. responseBody . key keyName . _String
    possibleFix r = extract r "possible_fix"
    errorMsg r = extract r "error"
