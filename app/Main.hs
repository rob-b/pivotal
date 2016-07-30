{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                   ( defaultOptions, myProjects
                                       , stories
                                       , me
                                       , myProjects
                                       , setToken )
import qualified Data.Text              as T
import qualified Data.Text.IO          as TIO
import           System.Environment    ( lookupEnv )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BC
import Options.Applicative

data Options = Options Command deriving (Show)
data Command = Status String
             | Stories (Maybe Integer)
             | Me
             | Projects
    deriving (Show)

type Token = BC.ByteString

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

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

optionsWithInfo :: ParserInfo Options
optionsWithInfo = info (helper <*> parseCommand)
        (fullDesc
        <> progDesc "Do the thing..."
        <> header "At the top")

run :: Token -> Options -> IO T.Text
run token (Options cmd) =
    case cmd of
        Me -> run' me
        Status a -> run' stories
        Stories x -> run' stories
        Projects -> run' myProjects
  where
    run' f = f $ setToken token defaultOptions

main :: IO ()
main = do
  token <- lookupEnv "PIVOTAL_TOKEN"
  valid' <- case token of
    Nothing -> do
      TIO.putStrLn "Must set PIVOTAL_TOKEN"
      exitWith $ ExitFailure 1
    Just t -> return (BC.pack t)
  result <- (run valid') =<< execParser optionsWithInfo
  TIO.putStrLn result
