{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pivotal.Options
  -- where
    ( execParser
    , run
    , optionsWithInfo
    , App(..)
    , Options(..)
    , Command(..)
    , mkApp
    ) where

import Pivotal.Lib (me, stories, myProjects, setToken, defaultOptions)
import Pivotal.Url (mkStoriesURL', StoriesParams(..))
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import Debug.Trace
import Control.Lens hiding (argument)

type ProjectId = BC.ByteString
type Token = BC.ByteString

data StoriesOption = StoriesDetail Integer
                   | StoriesList { sStatus :: Maybe B.ByteString
                                 , sKind   :: Maybe B.ByteString
                                 }
    deriving (Show)

data Command = Stories StoriesOption
             | Me
             | Projects
    deriving (Show)

data Options = Options { _optionsProjectId :: (Maybe ProjectId)
                       , _optionsToken     :: (Maybe Token)
                       , _optionsCommand   :: Command
                       }
    deriving (Show)


data App = App { _appToken     :: Token
               , _appProjectId :: ProjectId
               }
    deriving (Show)

makeLenses ''Options
makeLenses ''App

mkApp :: Token -> ProjectId -> App
mkApp token projectId = App { _appToken=token, _appProjectId=projectId }

storyStatuses :: [B.ByteString]
storyStatuses = [ "accepted"
                , "delivered"
                , "finished"
                , "started"
                , "rejected"
                , "planned"
                , "unstarted"
                , "unscheduled"
                ]

storyKinds :: [B.ByteString]
storyKinds = [ "feature", "bug", "chore", "release" ]

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
storiesListParser = StoriesList <$> optional (option (readerEnum storyStatuses) (long "status" <> help "Filter by status" <> metavar "status"))
                                <*> optional (option (readerEnum storyKinds) (long "kind" <> help "Filter by kind"))

commandParser :: Parser Command
commandParser = subparser $
           command "stories" (withInfo storiesParser "View story")
        <> command "profile" (withInfo (pure Me) "View user's profile")
        <> command "projects" (withInfo (pure Projects) "View user's projects")

parseCommand :: Parser Options
parseCommand = Options <$> optional (option readerByteString (long "project-id" <> help "Project id" <> metavar "PROJECTID"))
                       <*> optional (option readerByteString (long "pivotal-token" <> help "Pivotal API token" <> metavar "TOKEN"))
                       <*> commandParser


readerByteString :: ReadM BC.ByteString
readerByteString = do
  s <- readerAsk
  return $ BC.pack s

readerEnum :: Foldable t => t B.ByteString -> ReadM B.ByteString
readerEnum xs = eitherReader pred'
  where
    pred' arg = let x = BC.pack arg
               in
                   if x `elem` xs
                   then return x
                   else Left $ "cannot parse value `" ++ arg ++ "'"

run :: App -> Options -> IO T.Text
run app (Options _ _ cmd) =
    case cmd of
        Me -> run' me
        Stories (StoriesList status sType) -> do
            run' stories (mkStoriesURL' "xxxxxxx" $ StoriesParams sType status)
        Stories sd@(StoriesDetail _) ->
            trace (show sd) (run' stories ("okok"))
        Projects -> run' myProjects
  where
    run' f = f $ setToken (app ^. appToken) defaultOptions
