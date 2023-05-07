{
module Scanner
    ( Token(..)
    , scanTokens
    ) where

import Ast
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
@identifier = [a-zA-Z_] [a-zA-Z0-9_\']*
$whitespace = [\ \t]

tokens :-
  \,                            ;
  $eol+                         { \s -> TokenSemi }
  $whitespace+                  ;
  "#".*                         ;
  -- <comment> "(*"                { nestComment }
  -- <comment> "*)"                { unNestComment }
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
  Unit                          { \s -> TokenUnit }
  Bool                          { \s -> TokenBool }
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }
  "->"                          { \s -> TokenArrow }
  \;                            { \s -> TokenSemi }
  \|                            { \s -> TokenBar }
  \.                            { \s -> TokenDot }
  \:                            { \s -> TokenColon }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLam }
  \,                            { \s -> TokenComma }
  \(                            { \s -> TokenLparen }
  \)                            { \s -> TokenRparen }
  \{                            { \s -> TokenLbrace }   --TODO: multiline defs
  \}                            { \s -> TokenRbrace }
  \[                            { \s -> TokenLbracket }
  \]                            { \s -> TokenRbracket }
  @identifier                   { \s -> TokenVar s }
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
    | TokenLam
    | TokenArrow 
    | TokenBar
    | TokenColon
    | TokenSemi
    | TokenComma    
    | TokenLparen 
    | TokenRparen 
    | TokenLbrace
    | TokenRbrace
    | TokenLbracket
    | TokenRbracket
    | TokenDot
    | TokenType
    | TokenTrue
    | TokenFalse
    | TokenBool
    | TokenUnit
    deriving (Eq,Show)
scanTokens = alexScanTokens
}
