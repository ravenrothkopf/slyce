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

%left '+' '-'
%left '*'
%nonassoc NOELSE 'else'

%%
Expr: let VAR '=' Expr in Expr { App (Body [$2] [$6]) $4 }
    | '\' VAR '->' Expr { Body [$2] [$4] }
    | Pair { $1 }

Pair: Pair Single { App $1 $2 }
    | Single { $1 }

Single: '(' Expr ')' { $2 }
    | int   { Int $1 }
    | VAR   { Var [$1] }  
    | true  { Lit (BoolLit True)}
    | false { Lit (BoolLit False)}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseProgram :: String -> Expr
parseProgram = parse . scanTokens
}