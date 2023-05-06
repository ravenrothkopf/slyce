module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Ast
import Context
import TypeCheck

import Parser (parseProgram )

readInput :: String -> IO String
readInput "-" = getContents
readInput filename = readFile filename

parseAst :: String -> Module
parseAst = parseProgram

typeCheckFile :: String -> IO ()
typeCheckFile contents = undefined
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
    contents <- readInput pathToFile
    typeCheckFile contents
    exitSuccess
