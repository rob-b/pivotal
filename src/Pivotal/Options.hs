{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pivotal.Options
  ( execParser
  , run
  , optionsWithInfo
  , Command(..)
  , mkApp
  , optionsCommand
  , optionsToken
  , optionsProjectId
  , (^.))
  where

import Pivotal.Lib
import Pivotal.Url
       (mkDetailParams, mkListParams, mkMembershipsURL, mkStoriesURL')
import Pivotal.Types
import Control.Monad.Reader
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Control.Lens hiding (argument)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

data StoriesOption
  = StoriesDetail Integer
  | StoriesList (Maybe B.ByteString)
                (Maybe B.ByteString)
   deriving (Show)

data Command
  = Stories StoriesOption
  | Me
  | Projects
  | Setup
   deriving (Show)

data Options = Options
  { _optionsProjectId :: Maybe ProjectId
  , _optionsToken :: Maybe Token
  , _optionsCommand :: Command
  } deriving (Show)

data App = App
  { _appToken :: Token
  , _appProjectId :: ProjectId
  , _appCommand :: Command
  } deriving (Show)

makeLenses ''Options

mkStoriesList :: B.ByteString -> Maybe B.ByteString -> StoriesOption
mkStoriesList a = StoriesList (Just a)

mkApp :: Token -> ProjectId -> Command -> App
mkApp token projectId cmd =
  App
  { _appToken = token
  , _appProjectId = projectId
  , _appCommand = cmd
  }

storyStatuses :: [B.ByteString]
storyStatuses =
  [ "accepted"
  , "delivered"
  , "finished"
  , "started"
  , "rejected"
  , "planned"
  , "unstarted"
  , "unscheduled"]

storyKinds :: [B.ByteString]
storyKinds = ["feature", "bug", "chore", "release"]

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

optionsWithInfo :: ParserInfo Options
optionsWithInfo =
  info
    (helper <*> parseCommand)
    (fullDesc <> progDesc "Interact with pivotal tracker")

storiesParser :: Parser Command
storiesParser = Stories <$> (storiesDetailParser <|> storiesListParser)

storiesDetailParser :: Parser StoriesOption
storiesDetailParser = StoriesDetail <$> argument auto (metavar "story id")

storiesListParser :: Parser StoriesOption
storiesListParser =
  StoriesList <$>
  optional
    (option
       (readerEnum storyStatuses)
       (long "status" <> help "Filter by status" <> metavar "status")) <*>
  optional
    (option (readerEnum storyKinds) (long "kind" <> help "Filter by kind"))

storyParser :: B.ByteString -> Parser Command
storyParser s =
  Stories <$>
  fmap
    (mkStoriesList s)
    (optional
       (option
          (readerEnum storyKinds)
          (long "type" <>
           help "Filter by story type [feature|bug|chore|release]")))

commandParser :: Parser Command
commandParser =
  subparser $
  command "stories" (withInfo storiesParser "View story") <>
  command "todo" (withInfo (storyParser "unstarted") "View unstarted stories") <>
  command "started" (withInfo (storyParser "started") "View started stories") <>
  command "finished" (withInfo (storyParser "finished") "View finished stories") <>
  command
    "delivered"
    (withInfo (storyParser "delivered") "View delivered stories") <>
  command
    "icebox"
    (withInfo (storyParser "unscheduled") "View delivered stories") <>
  command "profile" (withInfo (pure Me) "View user's profile") <>
  command "projects" (withInfo (pure Projects) "View user's projects") <>
  command "setup" (withInfo (pure Setup) "Create a local project file")

parseCommand :: Parser Options
parseCommand =
  Options <$>
  optional
    (option
       (fmap ProjectId readerText)
       (long "project-id" <> help "Project id" <> metavar "PROJECTID")) <*>
  optional
    (option
       (fmap Token readerByteString)
       (long "token" <> help "Pivotal API token" <> metavar "TOKEN")) <*>
  commandParser

readerText :: ReadM T.Text
readerText = T.pack <$> readerAsk

readerByteString :: ReadM BC.ByteString
readerByteString = BC.pack <$> readerAsk

readerEnum :: Foldable t => t B.ByteString -> ReadM B.ByteString
readerEnum xs = eitherReader pred'
  where
    pred' arg =
      let x = BC.pack arg
      in if x `elem` xs
           then return x
           else Left $ "cannot parse value `" ++ arg ++ "'"

run :: App -> IO T.Text
run (App token pid cmd) = do
  let (handler',url') =
        case cmd of
          Setup -> (projectMembersHandler, mkMembershipsURL pid)
          Stories (StoriesList status sType) ->
            (storiesHandler, mkStoriesURL' pid $ mkListParams sType status)
          Stories (StoriesDetail sId) ->
            (storyHandler, mkStoriesURL' pid $ mkDetailParams sId)
          Projects ->
            (myProjectsHandler, "https://www.pivotaltracker.com/services/v5/me")
          Me ->
            (genericHandler, "https://www.pivotaltracker.com/services/v5/me")
  runReaderT processEndpoint (mkConfig token url' handler')
