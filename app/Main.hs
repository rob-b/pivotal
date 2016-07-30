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

data Command = Status String
             | Stories Integer
    deriving (Show)

parseInteger :: Parser Integer
parseInteger = argument auto (metavar "INTEGER")

parseString :: Parser String
parseString = argument str (metavar "STRING")

profileParser :: ParserInfo Command
profileParser = info (Status <$> parseString) (progDesc "Display profile info")

storyParser :: ParserInfo Command
storyParser = info (Stories <$> parseInteger) (progDesc "Display story info")

commandParser :: ParserInfo Command
commandParser = info commands $ progDesc "My program"
  where commands = subparser $ mconcat [command "status" profileParser, command "stories" storyParser]

-- options :: Parser Args
-- options = Args
--     <$> optional (argument str (metavar "profile"))
--     <*> commandParser
main = do
  cmd <- execParser commandParser
  print cmd
