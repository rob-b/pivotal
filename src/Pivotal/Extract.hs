{-# LANGUAGE OverloadedStrings #-}

module Pivotal.Extract
    where

import           Pivotal.Format
import           Control.Lens
import           Data.Aeson.Lens ( AsValue, _Array, _Integer, _String, key )
import qualified Data.Text       as T
import           Data.Maybe      ( fromMaybe )

storyDetailList :: Data.Aeson.Lens.AsValue s => s -> StoryList
storyDetailList r = StoryList (r ^.. _Array . traverse . to storyDetail)

storyDetail :: Data.Aeson.Lens.AsValue s => s -> Story
storyDetail r = Story (r ^?! key "name" . _String)
                      (r ^?! key "current_state" . _String)
                      (r ^?! key "story_type" . _String)
                      (r ^?! key "id" . _Integer)
                      (fromMaybe "" (r ^?  key "description" . _String))
                      (r ^?! key "url" . _String)

projectNames :: Data.Aeson.Lens.AsValue s => s -> [(Integer, T.Text)]
projectNames r = r ^.. key "projects" . _Array . traverse .
    to (\o -> ( o ^?! key "project_id" . _Integer
              , o ^?! key "project_name" . _String
              ))


errorMsg401 :: AsValue s => s -> [T.Text]
errorMsg401 s = [errorMsg s, possibleFix s]
  where
    extract r keyName = r ^. key keyName . _String
    possibleFix r = extract r "possible_fix"
    errorMsg r = extract r "error"
