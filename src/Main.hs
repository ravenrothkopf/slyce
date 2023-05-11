module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import System.Console.GetOpt

import Control.Monad (unless, when)
import Control.Monad.State.Strict (evalState)

import Ast
import Context
import TypeCheck
import PrettyPrint

import Parser (parse)
import Scanner (scanTokens)

readInput :: String -> IO String
readInput "-" = getContents
readInput filename = readFile filename

typeCheckProgram :: Module -> IO [Sig]
typeCheckProgram mod = do
    res <- runTcMonad $ typeCheckModule mod
    case res of
        Left typeError -> do
            print typeError
            exitFailure
        Right sigs -> return sigs

data Flag = Scan | Parse | CheckTypes | Verbose
    deriving (Show, Eq)

options :: [OptDescr Flag]
options = 
    [ Option ['v']      ["verbose"]     (NoArg Verbose)    "chatty output"
    , Option ['s', 'l'] ["scan", "lex"] (NoArg Scan)       "output scanned tokens"
    , Option ['p']      ["parse"]       (NoArg Parse)      "output parsed AST"
    , Option ['t']      ["type-check"]  (NoArg CheckTypes) "output checked types"
    ]

main :: IO ()
main = do
    argv <- getArgs
    let (opts, args, errs) = getOpt Permute options argv

    when (not (null errs)) $ do
        mapM_ putStrLn errs
        exitFailure

    unless (length args == 1) $ do
        putStrLn "Error: must provide one file as input"
        exitFailure

    let pathToFile = head args
    contents <- readInput pathToFile

    let tokens = scanTokens contents
    when (Scan `elem` opts) $ do
        mapM_ print tokens

    let mod = flip evalState emptyConstructorNames . parse $ tokens
    when (Parse `elem` opts) $ do
        print mod

    sigs <- typeCheckProgram mod
    when (CheckTypes `elem` opts) $ do
        mapM_ (putStrLn . ppSig) sigs

    exitSuccess
