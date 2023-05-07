{
module Parser
  ( parseProgram
  ) where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Scanner
import Ast
}

%name parse
%error { parseError }
%tokentype { Token }

%token
  'of'      { TokenOf }
  'data'    { TokenData }
  'if'      { TokenIf }
  'then'    { TokenThen }
  'else'    { TokenElse }
  'let'     { TokenLet }
  'in'      { TokenIn }
  'where'   { TokenWhere }
  'U'       { TokenType }
  'Bool'    { TokenBool }
  'True'    { TokenTrue }
  'False'   { TokenFalse }
  'Unit'    { TokenUnit }
  '\\'      { TokenLam }
  '='       { TokenEq }
  '->'      { TokenArrow }
  '|'       { TokenBar }
  ':'       { TokenColon }
  ';'       { TokenSemi }
  ','       { TokenComma }
  '.'       { TokenDot }
  '('       { TokenLparen }
  ')'       { TokenRparen }
  '{'       { TokenLbrace }
  '}'       { TokenRbrace }
  '['       { TokenLbracket }
  ']'       { TokenRbracket }
  VAR       { TokenVar $$ }

%nonassoc NOELSE 'else'

%%
program            --> Module
    : decls          { Module $1 }
    | {- empty -}    { Module [] }

decls               --> [Decl]
    : decl ';' decls  { $1 : $3 }
    | decl ';'        { [$1] }
    | decl            { [$1] }

decl               --> Decl
    : typeSig        { TypeSig $1 } 
    | name '=' term  { Def $1 $3 }

--TODO: the rest...

name               --> TermName
    : VAR            { Unbound.s2n $1 }

typeSig            --> Sig
    : name ':' term  { Sig $1 $3 }

term                                   --> Term
    : '\\' name '.' term                 { Lam (Unbound.bind $2 $4) }
    | '(' name ':' term ')' '->' term    { Pi $4 (Unbound.bind $2 $7) }
    | term '->' term                     { Pi $1 (Unbound.bind (Unbound.s2n "_") $3) }
    | term term                          { App $1 $2 }
    | '(' term ':' term ')'              { Ann $2 $4 }
    | 'U'                                { U }
    | 'Unit'                             { UnitType }
    | '(' ')'                            { UnitLit }
    | 'Bool'                             { BoolType }
    | 'True'                             { BoolLit True }
    | 'False'                            { BoolLit False}
    | 'if' term 'then' term 'else' term  { If $2 $4 $6 }
    | '(' name ':' term ',' term ')'     { Sigma $4 (Unbound.bind $2 $6) }
    | '(' term ',' term ')'              { Pair $2 $4 }
    | 'let' '(' name ',' name ')' '=' term 'in' term { LetPair $10 (Unbound.bind ($3, $5) $8) }
    | name                               { Var $1 }
--TokenLet VAR TokenEq Expr TokenIn Expr { App (Lam [$2] [$6]) $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseProgram :: String -> Module
parseProgram = parse . scanTokens
}
