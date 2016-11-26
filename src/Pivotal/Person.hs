{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Pivotal.Person ( Person(..) ) where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char (toLower)
import qualified Data.Text    as T

data Person = Person
  { userName :: T.Text
  , userInitials :: T.Text
  , userID :: Integer
  } deriving (Generic,Show)

instance FromJSON Person
instance ToJSON Person where
  toJSON =
    genericToJSON
      defaultOptions
      { fieldLabelModifier = drop 4 . map toLower
      }
