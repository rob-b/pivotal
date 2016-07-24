{-# LANGUAGE OverloadedStrings #-}

-- | A library to do stuff.
module Lib
    ( me
    , myProjects
    , defaultOptions
    , setToken
    ) where

import           Network.Wreq            ( Options, Response, checkStatus
                                         , defaults, getWith, header
                                         , responseBody, responseStatus
                                         , statusCode )
import           Control.Lens
import           Data.Aeson.Lens         ( _Array, _Integer, _String, key )

import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text               as T
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L

-- | get info about the authenticated user
me :: Options -> IO T.Text
me options = do
    r <- getWith options "https://www.pivotaltracker.com/services/v5/me"
    case (r ^. responseStatus . statusCode) of
      401 -> return $ handle401 (r ^. responseBody)
      _ -> return $ decode (r ^. responseBody)

-- | get info about the authenticated user's projects
myProjects :: Options -> IO T.Text
myProjects options = do
    r <- getWith options "https://www.pivotaltracker.com/services/v5/me"
    case (r ^. responseStatus . statusCode) of
        401 -> return $ handle401 (r ^. responseBody)
        200 -> return $ handle200 (r ^. responseBody)
        _ -> return $ decode (r ^. responseBody)
  where
    formatLine :: (Integer, T.Text) -> T.Text
    formatLine (i, t) = T.intercalate " / " [ t, (T.pack . show) i ]

    projectNames :: L.ByteString -> [(Integer, T.Text)]
    projectNames r = r ^.. key "projects" . _Array . traverse . to (\o -> ( o ^?! key "project_id" . _Integer , o ^?! key "project_name" . _String))

    handle200 :: L.ByteString -> T.Text
    handle200 body = T.append "The names and ids of your projects:\n" $
        T.intercalate "\n" (map formatLine (projectNames body))


simpleApiRequest :: Response L.ByteString -> IO T.Text
simpleApiRequest r = case (r ^. responseStatus . statusCode) of
    401 -> return $ handle401 (r ^. responseBody)
    _ -> return $ decode (r ^. responseBody)

decode :: L.ByteString -> T.Text
decode = TL.toStrict . TL.decodeUtf8

defaultOptions :: Options
defaultOptions = setCheckStatus defaults

setCheckStatus :: Options -> Options
setCheckStatus options =
    options & checkStatus .~ (Just $ \_ _ _ -> Nothing)

setToken :: B.ByteString -> Options -> Options
setToken token options = options & header "X-TrackerToken" .~ [token]

handle401 :: L.ByteString -> T.Text
handle401 response = T.intercalate " " [ errorMsg response, possibleFix response ]
  where
    extract r keyName = r ^. key keyName . _String
    possibleFix r = extract r "possible_fix"
    errorMsg r = extract r "error"
