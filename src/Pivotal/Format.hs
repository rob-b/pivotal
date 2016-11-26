{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pivotal.Format ( StoryList(..), Story(..), storyOwners, format ) where

import Pivotal.Person (Person(..))
import qualified Data.Text    as T
import           Control.Lens
import           Formatting hiding( format)

data Story = Story
  { _storyName :: T.Text
  , _storyState :: T.Text
  , _storyType :: T.Text
  , _storyId :: Integer
  , _storyDescription :: T.Text
  , _storyURL :: T.Text
  , _storyOwners :: [Person]
  } deriving (Show)

data StoryList =
  StoryList [Story]
   deriving (Show)

makeLenses ''Story

class Formattable a where
  format :: a -> T.Text

instance Formattable Story where
  format story =
    sformat
      ("" % stext % "\n\nType: " % stext % "\nState: " % stext % "\nOwners: " %
       stext %
       "\n" %
       stext %
       "\n\n" %
       stext)
      (mkTitle $ story ^. storyName)
      (story ^. storyType)
      (story ^. storyState)
      (mkOwners $ story ^. storyOwners)
      (story ^. storyURL)
      (story ^. storyDescription)

instance Formattable StoryList where
  format (StoryList xs) = T.intercalate "\n" (map fmtListItem xs)

instance Formattable Person where
  format (Person name initials id') =
    sformat ("#" % int % " " % stext % " (" % stext % ")") id' name initials

fmtListItem :: Story -> T.Text
fmtListItem s =
  sformat
    ("#" % int % " " % right 13 ' ' % right 9 ' ' % stext % " " % stext)
    (s ^. storyId)
    (s ^. storyState)
    (s ^. storyType)
    (s ^. storyName)
    (joinTexts . map format $ s ^. storyOwners)

joinTexts :: [T.Text] -> T.Text
joinTexts = T.intercalate ", "

mkTitle :: T.Text -> T.Text
mkTitle s = T.intercalate "\n" [s, T.replicate (T.length s) "*"]

mkOwners :: [Person] -> T.Text
mkOwners xs = T.intercalate ", " $ map userName xs
