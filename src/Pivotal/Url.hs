{-# LANGUAGE OverloadedStrings #-}

module Pivotal.Url
     where

import Network.HTTP.Types.URI
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString            as B
import           Data.ByteString.Builder
import           Data.Text.Encoding         ( decodeUtf8 )
import Data.Maybe (isJust)

data StoriesParams = StoriesParams { sType  :: Maybe B.ByteString
                                   , sState :: Maybe B.ByteString
                                   }

schemeAndLocation :: T.Text
schemeAndLocation = "https://www.pivotaltracker.com"

params :: StoriesParams -> Query
params (StoriesParams storyType storyState) =
  filter f [ ("with_state", storyState), ("with_story_type", storyType) ]
    where f pair = isJust (snd pair)

joinPath :: [T.Text] -> [T.Text]
joinPath xs = ["services", "v5"] ++ xs

mkStoriesURL :: T.Text -> StoriesParams -> T.Text
mkStoriesURL projectId term =
    schemeAndLocation `T.append` path
  where
    path = decodeUtf8 $ BLC.toStrict (toLazyByteString pathRaw)
    pathRaw = encodePath (joinPath [ "projects", projectId, "stories" ]) (params term)

mkStoriesURL' :: T.Text -> StoriesParams -> String
mkStoriesURL' projectId term = T.unpack $ mkStoriesURL projectId term
