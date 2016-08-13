{-# LANGUAGE OverloadedStrings #-}

module Pivotal.Url
    ( StoriesParams(..)
    , mkStoriesURL'
    , mkListParams
    , mkDetailParams
    , mkMembershipsURL
    ) where

import           Network.HTTP.Types.URI
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString            as B
import           Data.ByteString.Builder
import           Data.Text.Encoding         ( decodeUtf8 )
import           Data.Maybe                 ( isJust )
import           Formatting

data ListParams = ListParams { storyType  :: Maybe B.ByteString
                             , storyState :: Maybe B.ByteString
                             }
    deriving (Show)

data DetailParams = DetailParams Integer
    deriving (Show)

data StoriesParams = StoryListParams ListParams | StoryDetailParams DetailParams
    deriving (Show)

mkListParams :: Maybe B.ByteString -> Maybe B.ByteString -> StoriesParams
mkListParams t s = StoryListParams $ ListParams { storyType = t, storyState = s }

mkDetailParams :: Integer -> StoriesParams
mkDetailParams storyId = StoryDetailParams (DetailParams storyId)

schemeAndLocation :: T.Text
schemeAndLocation = "https://www.pivotaltracker.com"

params :: StoriesParams -> Query
params (StoryListParams (ListParams t s)) =
  filter f [ ("with_state", s), ("with_story_type", t) ]
    where f pair = isJust (snd pair)
params (StoryDetailParams _) = []

joinPath :: [T.Text] -> [T.Text]
joinPath xs = ["services", "v5"] ++ xs

mkStoriesURL :: T.Text -> StoriesParams -> T.Text
mkStoriesURL projectId parameters
    | StoryDetailParams (DetailParams pid) <- parameters =
          combine $ encodePathSegments (storyPath projectId (Just pid))
    | otherwise = combine $ pathRaw parameters
  where
    pathRaw :: StoriesParams -> Builder
    pathRaw term = encodePath (joinPath [ "projects", projectId, "stories" ]) (params term)

combine :: Builder -> T.Text
combine b = T.append schemeAndLocation . decodeUtf8 . BLC.toStrict $ toLazyByteString b

mkStoriesURL' :: T.Text -> StoriesParams -> String
mkStoriesURL' projectId term = T.unpack $ mkStoriesURL projectId term

storyPath :: Integral a => T.Text -> Maybe a -> [T.Text]
storyPath projectId Nothing = joinPath [ "projects", projectId, "stories" ]
storyPath projectId (Just s) = joinPath [ "projects", projectId, "stories", sformat("" % int) s ]

mkMembershipsURL :: T.Text -> String
mkMembershipsURL projectId = T.unpack .
  combine . encodePathSegments $ joinPath [ "projects", projectId, "memberships" ]
