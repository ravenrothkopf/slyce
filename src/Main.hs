module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Ast
import Context

typeCheckFile :: String -> IO ()
typeCheckFile = undefined

main :: IO ()
main = do
    [pathToFile] <- getArgs
    typeCheckFile pathToFile
    exitSuccess

    {-
type Ctx = [(Var, Type)]

inferType :: Term -> Ctx -> Maybe Type
inferType (Var x) ctx = lookupTy ctx x
inferType Type ctx = Just Type

lookupTy = undefined
-}
