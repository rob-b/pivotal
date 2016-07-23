{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib (me, setToken, defaultOptions)

import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(ExitFailure))
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  token <- lookupEnv "PIVOTAL_TOKEN"
  case token of
    Nothing -> do
      TIO.putStrLn "Must set PIVOTAL_TOKEN"
      exitWith $ ExitFailure 1
    Just token -> do
      result <- me $ setToken (BC.pack token) defaultOptions
      TIO.putStrLn result
