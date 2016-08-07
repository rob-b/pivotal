{-# LANGUAGE OverloadedStrings #-}

module Pivotal.Url
  ( StoriesParams(..), mkStoriesURL')
     where

import Network.HTTP.Types.URI
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString            as B
import           Data.ByteString.Builder
import           Data.Text.Encoding         ( decodeUtf8 )
import Data.Maybe (isJust)

data StoriesParams = StoriesParams { storyType  :: Maybe B.ByteString
                                   , storyState :: Maybe B.ByteString
                                   }

schemeAndLocation :: T.Text
schemeAndLocation = "https://www.pivotaltracker.com"

params :: StoriesParams -> Query
params (StoriesParams t s) =
  filter f [ ("with_state", s), ("with_story_type", t) ]
    where f pair = isJust (snd pair)

joinPath :: [T.Text] -> [T.Text]
joinPath xs = ["services", "v5"] ++ xs

mkStoriesURL :: T.Text -> StoriesParams -> T.Text
mkStoriesURL projectId term =
    schemeAndLocation `T.append` path
  where
    path :: T.Text
    path = decodeUtf8 $ BLC.toStrict (toLazyByteString pathRaw)
    pathRaw :: Builder
    pathRaw = encodePath (joinPath [ "projects", projectId, "stories" ]) (params term)

mkStoriesURL' :: T.Text -> StoriesParams -> String
mkStoriesURL' projectId term = T.unpack $ mkStoriesURL projectId term
