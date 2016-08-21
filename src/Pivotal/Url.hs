{-# LANGUAGE OverloadedStrings #-}

module Pivotal.Url
    ( StoriesParams(..)
    , mkStoriesURL'
    , mkListParams
    , mkDetailParams
    , mkMembershipsURL
    ) where

import           Pivotal.Types
import           Network.HTTP.Types.URI
import           Data.ByteString.Builder
import           Data.Text.Encoding         ( decodeUtf8 )
import           Data.Maybe                 ( isJust )
import           Formatting
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString            as B

mkListParams :: Maybe B.ByteString -> Maybe B.ByteString -> StoriesParams
mkListParams t s = StoryListParams ListParams { storyType = t, storyState = s }

mkDetailParams :: Integer -> StoriesParams
mkDetailParams storyId = StoryDetailParams (DetailParams storyId)

schemeAndLocation :: T.Text
schemeAndLocation = "https://www.pivotaltracker.com"

params :: StoriesParams -> Query
params (StoryListParams (ListParams t s)) =
  filter f [ ("with_state", s), ("with_story_type", t) ]
    where f pair = isJust (snd pair)
params (StoryDetailParams _) = []

joinPath :: ProjectId -> [T.Text] -> [T.Text]
joinPath projectId segments = ["services", "v5", "projects", unProjectId projectId] ++ segments

mkStoriesURL :: ProjectId -> StoriesParams -> T.Text
mkStoriesURL projectId parameters
  | StoryDetailParams (DetailParams pid) <- parameters =
    combine $ encodePathSegments (storyPath projectId (Just pid))
  | otherwise = combine $ pathRaw parameters
  where
    pathRaw :: StoriesParams -> Builder
    pathRaw term = encodePath (joinPath projectId ["stories"]) (params term)

combine :: Builder -> T.Text
combine b = T.append schemeAndLocation . decodeUtf8 . BLC.toStrict $ toLazyByteString b

mkStoriesURL' :: ProjectId -> StoriesParams -> String
mkStoriesURL' projectId term = T.unpack $ mkStoriesURL projectId term

storyPath :: Integral a => ProjectId -> Maybe a -> [T.Text]
storyPath projectId Nothing = joinPath projectId ["stories"]
storyPath projectId (Just s) = joinPath projectId ["stories", sformat("" % int) s]

mkMembershipsURL :: ProjectId -> String
mkMembershipsURL projectId = T.unpack .
  combine . encodePathSegments $ joinPath projectId ["memberships"]
