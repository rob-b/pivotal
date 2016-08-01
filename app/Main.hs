{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Pivotal               ( execParser, optionsWithInfo, run )
import qualified Data.Text.IO          as TIO
import           System.Environment    ( lookupEnv )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BC

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
