module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Ast
import Context

typeCheckFile :: String -> IO ()
typeCheckFile = undefined
    {-
typeCheckFile filePath = do
  putStrLn $ "loading " ++ filePath ++ "..."
  program <- read <$> readFile filePath
  putStrLn "type checking..."
  runTcMonad program
  --defs <- d `exitWith` putTypeError
  ---}

main :: IO ()
main = do
    [pathToFile] <- getArgs
    typeCheckFile pathToFile
    exitSuccess
