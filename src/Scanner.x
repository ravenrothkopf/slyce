{
module Scanner
    ( Token(..)
    , TokenPos(..)
    , scanTokens
    , getPos
    , getToken
    ) where

import Ast
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
@identifier = [a-zA-Z_] [a-zA-Z0-9_\']*
$whitespace = [\ \t]

tokens :-
  $eol+                         { \p s -> TokenPos p TokenLineSep }
  \;$eol+                       ;
  \;                            ;
  $whitespace+                  ;
  "#".*                         ;
  -- <comment> "(*"                { nestComment }
  -- <comment> "*)"                { unNestComment }
  U                             { \p s -> TokenPos p TokenType }
  of                            { \p s -> TokenPos p TokenOf }
  data                          { \p s -> TokenPos p TokenData }
  let                           { \p s -> TokenPos p TokenLet }
  in                            { \p s -> TokenPos p TokenIn }
  where                         { \p s -> TokenPos p TokenWhere }
  if                            { \p s -> TokenPos p TokenIf }
  then                          { \p s -> TokenPos p TokenThen }
  else                          { \p s -> TokenPos p TokenElse }
  $digit+                       { \p s -> TokenPos p (TokenNum (read s)) }
  Unit                          { \p s -> TokenPos p TokenUnit }
  Bool                          { \p s -> TokenPos p TokenBool }
  True                          { \p s -> TokenPos p TokenTrue }
  False                         { \p s -> TokenPos p TokenFalse }
  "->"                          { \p s -> TokenPos p TokenArrow }
  \,                            { \p s -> TokenPos p TokenComma }
  \|                            { \p s -> TokenPos p TokenBar }
  "||"                          { \p s -> TokenPos p TokenLineSep }
  \.                            { \p s -> TokenPos p TokenDot }
  \*                            { \p s -> TokenPos p TokenStar }
  \:                            { \p s -> TokenPos p TokenColon }
  \=                            { \p s -> TokenPos p TokenEq }
  \\                            { \p s -> TokenPos p TokenLam }
  \(                            { \p s -> TokenPos p TokenLparen }
  \)                            { \p s -> TokenPos p TokenRparen }
  \{                            { \p s -> TokenPos p TokenLbrace }
  \}                            { \p s -> TokenPos p TokenRbrace }
  \[                            { \p s -> TokenPos p TokenLbracket }
  \]                            { \p s -> TokenPos p TokenRbracket }
  @identifier                   { \p s -> TokenPos p (TokenVar s) }
{
data TokenPos = TokenPos AlexPosn Token
    deriving (Eq, Show)

data Token = TokenLet
    | TokenLineSep
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
    | TokenStar
    | TokenType
    | TokenTrue
    | TokenFalse
    | TokenBool
    | TokenUnit
    deriving (Eq,Show)
scanTokens = alexScanTokens

getPos :: TokenPos -> SourcePos
getPos (TokenPos (AlexPn _ line col) _) = Posn line col

getToken :: TokenPos -> Token
getToken (TokenPos _ t) = t
}
