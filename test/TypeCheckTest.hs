module TypeCheckTest where

import qualified Unbound.Generics.LocallyNameless as Unbound
import TypeCheck
import Parser
import Scanner
import Context
import Ast

main = do
    let ast = parse . scanTokens $ "x = y"
    print ast
    runTcMonad $ typeCheckDef (last $ moduleDecls ast)
    --typeCheckDef ast
    --runTcMonad

    --ast2 <- parse . scanTokens <$> readFile "../examples/hello.py"

