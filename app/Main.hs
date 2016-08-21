{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Pivotal               ( (^.), ProjectId(..), Token(..)
                                       , execParser, mkApp, optionsCommand
                                       , optionsProjectId, optionsToken
                                       , optionsWithInfo, run )
import           System.Environment    ( lookupEnv )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import           Control.Applicative   ( (<|>) )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.IO          as TIO
import qualified Data.Text             as T

main :: IO ()
main = do
    maybeEnvToken <- lookupEnvWith (Token . BC.pack) "PIVOTAL_TOKEN"
    maybeEnvProjectId <- lookupEnvWith (ProjectId . T.pack) "PIVOTAL_PROJECT_ID"

    -- Get the ARGV values. When determining the token|project-id to use we prefer:
    -- 1. ARGV
    -- 2. Env var
    -- 3. (one day) local config file
    options <- execParser optionsWithInfo

    -- FIXME: some commands don't need token + project-id
    bestToken <- failIf (options ^. optionsToken <|> maybeEnvToken) "Must set PIVOTAL_TOKEN"
    bestProjectId <- failIf (options ^. optionsProjectId <|> maybeEnvProjectId) "Must set PIVOTAL_PROJECT_ID"
    let app = mkApp bestToken bestProjectId (options ^. optionsCommand)
    run app >>= TIO.putStrLn
  where
    lookupEnvWith :: (String -> b) -> String -> IO (Maybe b)
    lookupEnvWith f = fmap (fmap f) . lookupEnv

    failIf :: Maybe b -> T.Text -> IO b
    failIf cmp msg = case cmp of
                     Nothing -> do
                         TIO.putStrLn msg
                         exitWith $ ExitFailure 1
                     Just t -> return t
