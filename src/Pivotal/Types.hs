{-# LANGUAGE OverloadedStrings #-}

module Pivotal.Types
  where

import Pivotal.Person (Person)
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as L
import qualified Network.Wreq         as Wreq
import           Control.Lens
import           Text.Show.Functions  ()
import           Network.Wreq         ( checkStatus, defaults, header )

newtype ProjectId = ProjectId { unProjectId :: T.Text }
    deriving (Show)

newtype Token = Token { unToken :: B.ByteString }
    deriving (Show)

data ListParams = ListParams { storyType  :: Maybe B.ByteString
                             , storyState :: Maybe B.ByteString
                             }
    deriving (Show)

data DetailParams = DetailParams Integer
    deriving (Show)

data StoriesParams = StoryListParams ListParams
                   | StoryDetailParams DetailParams
    deriving (Show)

newtype Handler = Handler { unHandler :: L.ByteString -> IO T.Text }
    deriving (Show)

data Config = Config
  { cURL :: String            -- ^  url of a pivotal endpoint
  , cOptions :: Wreq.Options  -- ^  Connection options to pass to Wreq
  , handle200 :: Handler      -- ^  A hander that converts the response from wreq to Text
  , cPeople :: Maybe [Person] -- ^  People affiliated with this project
  } deriving (Show)

mkConfig :: Token -> String -> (L.ByteString -> IO T.Text) -> Config
mkConfig token url f = Config { cURL = url
                              , cOptions = setToken token defaultOptions
                              , handle200 = Handler f
                              , cPeople = Nothing
                              }

defaultOptions :: Wreq.Options
defaultOptions = setCheckStatus defaults

setCheckStatus :: Wreq.Options -> Wreq.Options
setCheckStatus options =
    options & checkStatus .~ (Just $ \_ _ _ -> Nothing)

setToken :: Token -> Wreq.Options -> Wreq.Options
setToken token options = options & header "X-TrackerToken" .~ [unToken token]
