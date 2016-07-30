{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                   ( defaultOptions, myProjects
                                       , stories
                                       , me
                                       , myProjects
                                       , setToken )
import qualified Data.Text.IO          as TIO
import           System.Environment    ( lookupEnv, getArgs )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Options.Applicative

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

main' :: IO ()
main' = do
    args <- fmap headMaybe getArgs
    let cmd = case fromMaybe "me" args of
            "stories" -> stories
            "projects" -> myProjects
            "me" -> me
            _ -> me

    token <- lookupEnv "PIVOTAL_TOKEN"
    case token of
        Nothing -> do
            TIO.putStrLn "Must set PIVOTAL_TOKEN"
            exitWith $ ExitFailure 1
        Just token' -> do
            result <- cmd $ setToken (BC.pack token') defaultOptions
            TIO.putStrLn result


data Args = Args { aProfile :: Maybe String
                 , aStories :: Command
                 } deriving (Show)

data Options = Options Command deriving (Show)
data Command = Status String
             | Stories Integer
             | Me
    deriving (Show)

-- pivotal project xxx stories yxy
-- pivotal stories all
-- pivotal stories yxy
-- pivotal profile


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- profileParser :: Parser Command
-- profileParser = Me <$> argument auto (metavar "IHATETHIS")

storiesParser :: Parser Command
storiesParser = Stories <$> argument auto (metavar "story-id")

sub :: Parser Command
sub = subparser $
    command "stories" (withInfo storiesParser "View story")
        <> command "profile" (withInfo (pure Me) "somthing...")

options :: Parser Args
options = Args
    <$> optional (argument str (metavar "profile"))
    <*> sub

parseCommand = Options <$> sub

optionsWithInfo :: ParserInfo Options
optionsWithInfo = info (helper <*> parseCommand)
        (fullDesc
        <> progDesc "Do the thing..."
        <> header "At the top")

main :: IO ()
main = do
  cmd <- execParser optionsWithInfo
  print cmd
