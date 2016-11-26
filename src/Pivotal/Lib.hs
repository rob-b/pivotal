{-# LANGUAGE OverloadedStrings #-}

-- | A library to do stuff.
module Pivotal.Lib
  (
   -- Transform responses from wreq to formatted output
    myProjectsHandler
  , storiesHandler
  , storyHandler
  , projectMembersHandler
  , genericHandler
   -- convert a config to a Text of the response
  , mkConfig
  , processEndpoint)
  where

import Formatting
import Pivotal.Extract
       (errorMsg401, personList, projectNames, storyDetail, flatPerson,
        storyDetailList)
import Pivotal.Types
import Network.Wreq
       (getWith, responseBody, responseStatus, statusCode)
import Data.Aeson (encode)
import Control.Lens
import qualified Pivotal.Format as F
import qualified Network.Wreq as Wreq
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Control.Monad.Reader

personFile :: FilePath
personFile = ".people.json"

-- FIXME: Handle case where this file does not exist
loadPersonFile = flatPerson <$> L.readFile personFile

processEndpoint :: ReaderT Config IO T.Text
processEndpoint = do
  config <- ask
  liftIO $ doRequest (cOptions config) (cURL config) (handle200 config)

doRequest :: Wreq.Options -> String -> Handler -> IO T.Text
doRequest options url handler = do
  r <- getWith options url
  case r ^. responseStatus . statusCode of
    x
      | x `elem` [400 .. 499] -> handle4xx (r ^. responseBody)
    200 -> unHandler handler (r ^. responseBody)
    _ -> genericHandler (r ^. responseBody)

formatSingleProject :: (Integer, T.Text) -> T.Text
formatSingleProject (project_id,project_name) =
  sformat ("#" % int % " " % stext) project_id project_name

myProjectsHandler :: L.ByteString -> IO T.Text
myProjectsHandler body =
  return $ T.intercalate "\n" (map formatSingleProject (projectNames body))

storiesHandler :: L.ByteString -> IO T.Text
storiesHandler = return . F.format . storyDetailList

storyHandler :: L.ByteString -> IO T.Text
storyHandler bs = F.format <$> fmap (`storyDetail` bs) loadPersonFile

projectMembersHandler :: L.ByteString -> IO T.Text
projectMembersHandler response = do
  L.writeFile personFile $ encode (personList response)
  return $ T.intercalate "\n" (map F.format (personList response))

genericHandler :: L.ByteString -> IO T.Text
genericHandler = return . TL.toStrict . TL.decodeUtf8

handle4xx :: L.ByteString -> IO T.Text
handle4xx response = return $ T.intercalate " " (errorMsg401 response)
