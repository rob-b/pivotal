{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Pivotal.Person ( Person(..) ) where

import           GHC.Generics
import           Data.Aeson
import qualified Data.Text    as T

data Person = Person { userName     :: T.Text
                     , userInitials :: T.Text
                     , userID       :: Integer
                     }
    deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person
