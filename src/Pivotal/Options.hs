{-# LANGUAGE OverloadedStrings #-}
module Pivotal.Options (execParser, optionsWithInfo, Token, Options(..), Command(..)) where

import Options.Applicative
import qualified Data.ByteString.Char8 as BC

data Options = Options Command deriving (Show)
data Command = Status String
             | Stories (Maybe Integer)
             | Me
             | Projects
    deriving (Show)

type Token = BC.ByteString

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

optionsWithInfo :: ParserInfo Options
optionsWithInfo = info (helper <*> parseCommand)
        (fullDesc
        <> progDesc "Do the thing..."
        <> header "At the top")

storiesParser :: Parser Command
storiesParser = Stories <$> optional (argument auto (metavar "story-id"))

statusParser :: Parser Command
statusParser = Status <$> argument str (metavar "status-kind")

commandParser :: Parser Command
commandParser = subparser $
           command "stories" (withInfo storiesParser "View story")
        <> command "profile" (withInfo (pure Me) "View user's profile")
        <> command "projects" (withInfo (pure Projects) "View user's projects")
        <> command "status" (withInfo statusParser "View stories with given status")

parseCommand :: Parser Options
parseCommand = Options <$> commandParser
