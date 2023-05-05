{
module Scanner (Token(..),scanTokens) where
import Ast
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  \,                            ;
  $eol                          ;
  $white+                       ;
  "#".*                         ;
  U                             { \s -> TokenType }
  of                            { \s -> TokenOf }
  data                          { \s -> TokenData }
  let                           { \s -> TokenLet }
  in                            { \s -> TokenIn }
  where                         { \s -> TokenWhere }
  if                            { \s -> TokenIf }
  then                          { \s -> TokenThen }
  else                          { \s -> TokenElse }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  \|                            { \s -> TokenBar }
  \.                            { \s -> TokenDot }
  \:                            { \s -> TokenColon }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLam }
  \,                            { \s -> TokenComma }
  \(                            { \s -> TokenLparen }
  \)                            { \s -> TokenRparen }
  \{                            { \s -> TokenLbrace }
  \}                            { \s -> TokenRbrace }
  \[                            { \s -> TokenLbracket }
  \]                            { \s -> TokenRbracket }
  $alpha [$alpha $digit \, \_ \']* { \s -> TokenVar s }
{
data Token = TokenLet
    | TokenIn
    | TokenWhere
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenData
    | TokenOf
    | TokenNum Int
    | TokenVar String
    | TokenEq 
    | TokenLambda
    | TokenArrow 
    | TokenBar
    | TokenColon
    | TokenComma    
    | TokenLparen 
    | TokenRparen 
    | TokenLbrace
    | TokenRbrace
    | TokenLbracket
    | TokenRbracket
    | TokenDot
    | TokenType
    deriving (Eq,Show)
scanTokens = alexScanTokens
}