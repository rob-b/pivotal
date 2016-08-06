{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Pivotal               ( Options(..), execParser
                                       , mkApp, optionsWithInfo, run )
import qualified Data.Text.IO          as TIO
import           System.Environment    ( lookupEnv )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BC
import           Control.Applicative   ( (<|>) )

main :: IO ()
main = do
    maybeEnvToken <- lookupEnv' "PIVOTAL_TOKEN"
    maybeEnvProjectId <- lookupEnv' "PIVOTAL_PROJECT_ID"

    -- Get the ARGV values. When determining the token|project-id to use we prefer:
    -- 1. ARGV
    -- 2. Env var
    -- 3. (one day) local config file
    options <- execParser optionsWithInfo
    bestToken <- case (_optionsToken options <|> maybeEnvToken) of
                     Nothing -> do
                         TIO.putStrLn "Must set PIVOTAL_TOKEN"
                         exitWith $ ExitFailure 1
                     Just t -> return t

    bestProjectId <- case (_optionsProjectId options <|> maybeEnvProjectId) of
                     Nothing -> do
                         TIO.putStrLn "Must set PIVOTAL_PROJECT_ID"
                         exitWith $ ExitFailure 1
                     Just t -> return t

    result <- run (mkApp bestToken bestProjectId) options
    TIO.putStrLn result
  where
    lookupEnv' = fmap (fmap BC.pack) . lookupEnv
