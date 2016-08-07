{-# LANGUAGE OverloadedStrings #-}

-- | A library to do stuff.
module Pivotal.Lib
    ( me
    , myProjects
    , stories
    , story
    , defaultOptions
    , setToken
    , loadSample
    ) where

import           Network.Wreq            ( checkStatus, defaults, getWith
                                         , header, responseBody, responseStatus
                                         , statusCode )
import           Control.Lens
import           Data.Aeson.Lens         ( _Array, _Integer, _String, key )
import           Data.Maybe              ( fromMaybe )

import qualified Network.Wreq            as Wreq
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text               as T
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Formatting

-- | gest info about the authenticated user
me :: Wreq.Options -> IO T.Text
me options = do
    r <- getWith options "https://www.pivotaltracker.com/services/v5/me"
    case (r ^. responseStatus . statusCode) of
      401 -> return $ handle401 (r ^. responseBody)
      _ -> return $ decode (r ^. responseBody)

-- | get info about the authenticated user's projects
myProjects :: Wreq.Options -> IO T.Text
myProjects options = do
    r <- getWith options "https://www.pivotaltracker.com/services/v5/me"
    case (r ^. responseStatus . statusCode) of
        401 -> return $ handle401 (r ^. responseBody)
        200 -> return $ handle200 (r ^. responseBody)
        _ -> return $ decode (r ^. responseBody)
  where
    formatSingleProject :: (Integer, T.Text) -> T.Text
    formatSingleProject (project_id, project_name) = sformat("#" % int % " " % stext) project_id project_name

    projectNames :: L.ByteString -> [(Integer, T.Text)]
    projectNames r = r ^.. key "projects" . _Array . traverse .
        to (\o -> ( o ^?! key "project_id" . _Integer
                  , o ^?! key "project_name" . _String
                  ))

    handle200 :: L.ByteString -> T.Text
    handle200 body = T.intercalate "\n" (map formatSingleProject (projectNames body))

stories :: Wreq.Options -> String -> IO T.Text
stories options url = do
  r <- getWith options url
  case (r ^. responseStatus . statusCode) of
      401 -> return $ handle401 (r ^. responseBody)
      200 -> return $ handle200 (r ^. responseBody)
      _ -> return $ decode (r ^. responseBody)
  where
    handle200 :: L.ByteString -> T.Text
    handle200 body = (T.intercalate "\n" . formatStoryDetails . storyDetails) body

    storyDetails :: L.ByteString -> [(T.Text, T.Text, T.Text, Integer)]
    storyDetails r = r ^.. _Array . traverse .
        to (\o -> ( o ^?! key "name" . _String
                  , o ^?! key "current_state" . _String
                  , o ^?! key "story_type" . _String
                  , o ^?! key "id" . _Integer
                  ))

    formatStoryDetails :: [(T.Text, T.Text, T.Text, Integer)] -> [T.Text]
    formatStoryDetails = map formatSingleStory

formatSingleStory :: (T.Text, T.Text, T.Text, Integer) -> T.Text
formatSingleStory (name, current_state, story_type, story_id) =
    sformat ("#" % int % " " % (right 13 ' ') % (right 9 ' ') % stext)
            story_id
            current_state
            story_type
            name

story :: Wreq.Options -> String -> IO T.Text
story options url = do
  r <- getWith options url
  case (r ^. responseStatus . statusCode) of
      401 -> return $ handle401 (r ^. responseBody)
      200 -> return $ handle200 (r ^. responseBody)
      _ -> return $ decode (r ^. responseBody)

  where
    storyDetails :: L.ByteString -> (T.Text, T.Text, T.Text, T.Text, T.Text)
    storyDetails r = ( r ^?! key "name" . _String
                     , r ^?! key "current_state" . _String
                     , r ^?! key "story_type" . _String
                     , fromMaybe "" (r ^?  key "description" . _String)
                     , r ^?! key "url" . _String
                     )

    fmt :: (T.Text, T.Text, T.Text, T.Text, T.Text) -> T.Text
    fmt (name, state, type', desc, url') =
      sformat("" % stext % "\n\nType: " % stext % "\nState: " % stext % "\n" % stext % "\n\n" % stext) (mkTitle name) type' state url' desc

    mkTitle :: T.Text -> T.Text
    mkTitle s = T.intercalate "\n" [s, T.replicate (T.length s) "*"]

    handle200 :: L.ByteString -> T.Text
    handle200 body = (fmt . storyDetails) body

decode :: L.ByteString -> T.Text
decode = TL.toStrict . TL.decodeUtf8

handle401 :: L.ByteString -> T.Text
handle401 response = T.intercalate " " [ errorMsg response, possibleFix response ]
  where
    extract r keyName = r ^. key keyName . _String
    possibleFix r = extract r "possible_fix"
    errorMsg r = extract r "error"

loadSample :: IO L.ByteString
loadSample = L.readFile "story.json"

defaultOptions :: Wreq.Options
defaultOptions = setCheckStatus defaults

setCheckStatus :: Wreq.Options -> Wreq.Options
setCheckStatus options =
    options & checkStatus .~ (Just $ \_ _ _ -> Nothing)

setToken :: B.ByteString -> Wreq.Options -> Wreq.Options
setToken token options = options & header "X-TrackerToken" .~ [token]

-- simpleApiRequest :: Response L.ByteString -> IO T.Text
-- simpleApiRequest r = case (r ^. responseStatus . statusCode) of
--     401 -> return $ handle401 (r ^. responseBody)
--     _ -> return $ decode (r ^. responseBody)
