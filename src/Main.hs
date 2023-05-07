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

typeCheckFile :: String -> IO ()
typeCheckFile contents = do
    let mod = parseProgram contents
    types <- runTcMonad $ typeCheckModule mod
    mapM_ print types


main :: IO ()
main = do
    [pathToFile] <- getArgs
    contents <- readInput pathToFile
    typeCheckFile contents
    exitSuccess
