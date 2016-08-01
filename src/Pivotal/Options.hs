{-# LANGUAGE OverloadedStrings #-}
module Pivotal.Options
    ( execParser
    , optionsWithInfo
    , Token
    , Options(..)
    , Command(..)
    , StoriesOption(..)
    ) where

import Options.Applicative
import qualified Data.ByteString.Char8 as BC

data Options = Options Command
    deriving (Show)

data StoriesOption = StoriesDetail Integer | StoriesList (Maybe String)
    deriving (Show)

data Command = Stories StoriesOption
             | Me
             | Projects
    deriving (Show)

type Token = BC.ByteString

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

optionsWithInfo :: ParserInfo Options
optionsWithInfo = info (helper <*> parseCommand)
        (fullDesc
        <> progDesc "Interact with pivotal tracker"
        <> header "At the top")

storiesParser :: Parser Command
storiesParser = Stories <$> (storiesDetailParser <|> storiesListParser)

storiesDetailParser :: Parser StoriesOption
storiesDetailParser = StoriesDetail <$> argument auto (metavar "story id")

storiesListParser :: Parser StoriesOption
storiesListParser = StoriesList <$> optional (option str (long "status" <> help "Filter by status" <> metavar "status"))

commandParser :: Parser Command
commandParser = subparser $
           command "stories" (withInfo storiesParser "View story")
        <> command "profile" (withInfo (pure Me) "View user's profile")
        <> command "projects" (withInfo (pure Projects) "View user's projects")

parseCommand :: Parser Options
parseCommand = Options <$> commandParser
