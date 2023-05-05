{
module Parser
  ( parseProgram
  ) where
import Scanner
import Ast
}

%name parse
%error { parseError }
%tokentype { Token }

%token
  'U'       { TokenType }
  'of'      { TokenOf }
  'data'    { TokenData }
  'if'      { TokenIf }
  'then'    { TokenThen }
  'else'    { TokenElse }
  'let'     { TokenLet }
  'in'      { TokenIn }
  'where'   { TokenWhere }
  'True'    { TokenTrue }
  'False'   { TokenFalse }
  '\'       { TokenLam }
  '='       { TokenEq }
  '->'      { TokenArrow }
  '|'       { TokenBar }
  ':'       { TokenColon }
  ','       { TokenComma }
  '.'       { TokenDot }
  '('       { TokenLparen }
  ')'       { TokenRparen }
  '{'       { TokenLbrace }
  '}'       { TokenRbrace }
  '['       { TokenLbracket }
  ']'       { TokenRbracket }
  Bool      { TokenBool $$ }
  Unit      { TokenUnit $$ }
  VAR       { TokenVar $$ }

%nonassoc NOELSE 'else'

%%
Expr: let VAR '=' Expr in Expr { App (Lam [$2] [$6]) $4 }
    | '\' VAR '->' Expr { Lam [$2] [$4] }
    | Pair { $1 }

Pair: Pair Single { App $1 $2 }
    | Single { $1 }

Single: '(' Expr ')' { $2 }
    | VAR   { Var [$1] }  
    | true  { BoolLit True}
    | false { BoolLit False}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseProgram :: String -> Expr
parseProgram = parse . scanTokens
}