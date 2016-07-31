{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Pivotal
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Environment    ( lookupEnv )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BC

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
