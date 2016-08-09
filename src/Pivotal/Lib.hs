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
    , projectNames
    ) where
import           Formatting
import           Pivotal.Extract         ( errorMsg401, projectNames
                                         , storyDetail, storyDetailList )
import           Network.Wreq            ( checkStatus, defaults, getWith
                                         , header, responseBody, responseStatus
                                         , statusCode )
import           Control.Lens
import qualified Pivotal.Format          as F
import qualified Network.Wreq            as Wreq
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text               as T
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L

-- | get info about the authenticated user
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
    handle200 body = F.format (storyDetailList body)

    formatStoryDetails :: [(T.Text, T.Text, T.Text, T.Text, T.Text)] -> [T.Text]
    formatStoryDetails = map formatSingleStory

formatSingleStory :: (T.Text, T.Text, T.Text, T.Text, T.Text) -> T.Text
formatSingleStory (name, current_state, story_type, _, story_id) =
    sformat ("#" % stext % " " % (right 13 ' ') % (right 9 ' ') % stext)
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

    fmt :: (T.Text, T.Text, T.Text, T.Text, T.Text) -> T.Text
    fmt (name, state, type', desc, url') =
      sformat("" % stext % "\n\nType: " % stext % "\nState: " % stext % "\n" % stext % "\n\n" % stext) (mkTitle name) type' state url' desc

    mkTitle :: T.Text -> T.Text
    mkTitle s = T.intercalate "\n" [s, T.replicate (T.length s) "*"]

    handle200 :: L.ByteString -> T.Text
    handle200 = F.format . storyDetail

decode :: L.ByteString -> T.Text
decode = TL.toStrict . TL.decodeUtf8

handle401 :: L.ByteString -> T.Text
handle401 response = T.intercalate " " (errorMsg401 response)

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
