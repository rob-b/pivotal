{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Pivotal               ( execParser, optionsWithInfo, run, App(..) )
import qualified Data.Text.IO          as TIO
import           System.Environment    ( lookupEnv )
import           System.Exit           ( ExitCode(ExitFailure), exitWith )
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  token <- lookupEnv "PIVOTAL_TOKEN"
  pId <- lookupEnv "PIVOTAL_PROJECT_ID"
  valid' <- case token of
    Nothing -> do
      TIO.putStrLn "Must set PIVOTAL_TOKEN"
      exitWith $ ExitFailure 1
    Just t -> return (BC.pack t)

  result <- (run (App{pivotalToken=valid', projectId=pId} )) =<< execParser optionsWithInfo
  TIO.putStrLn result
