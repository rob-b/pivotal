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
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

main :: IO ()
main = do
    args <- fmap headMaybe getArgs
    let cmd = case fromMaybe "me" args of
            "me" -> me
            "stories" -> stories
            "projects" -> myProjects
            _ -> me

    token <- lookupEnv "PIVOTAL_TOKEN"
    case token of
        Nothing -> do
            TIO.putStrLn "Must set PIVOTAL_TOKEN"
            exitWith $ ExitFailure 1
        Just token' -> do
            result <- cmd $ setToken (BC.pack token') defaultOptions
            TIO.putStrLn result
