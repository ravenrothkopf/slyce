{
module Scanner
    ( Token(..)
    , TokenPos(..)
    , scanTokens
    , getPos
    , getToken
    , AlexPosn(..)
    ) where

import Ast
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
@idlower = [a-z] [a-zA-Z0-9_\']*
@idupper = [A-Z] [a-zA-Z0-9_\']*
$whitespace = [\ \t]

tokens :-
  --$eol+                         { \p s -> TokenPos p TokenLineSep }
  --\;$eol+                       ;
  $eol+                         ;
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
  match                         { \p s -> TokenPos p TokenMatch }
  with                          { \p s -> TokenPos p TokenWith }
  if                            { \p s -> TokenPos p TokenIf }
  then                          { \p s -> TokenPos p TokenThen }
  else                          { \p s -> TokenPos p TokenElse }
  $digit+                       { \p s -> TokenPos p (TokenNum (read s)) }
  Unit                          { \p s -> TokenPos p TokenUnit }
  Bool                          { \p s -> TokenPos p TokenBool }
  True                          { \p s -> TokenPos p TokenTrue }
  False                         { \p s -> TokenPos p TokenFalse }
  Refl                          { \p s -> TokenPos p TokenRefl } 
  contra                        { \p s -> TokenPos p TokenContra } 
  subst                         { \p s -> TokenPos p TokenSubst } 
  by                            { \p s -> TokenPos p TokenBy } 
  "->"                          { \p s -> TokenPos p TokenArrow }
  \>                            { \p s -> TokenPos p TokenGt }
  \<                            { \p s -> TokenPos p TokenLt }
  \,                            { \p s -> TokenPos p TokenComma }
  \|                            { \p s -> TokenPos p TokenBar }
  "||"                          { \p s -> TokenPos p TokenLineSep }
  \.                            { \p s -> TokenPos p TokenDot }
  \_                            { \p s -> TokenPos p TokenUnderscore }
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
  @idlower                      { \p s -> TokenPos p (TokenIdLower s) }
  @idupper                      { \p s -> TokenPos p (TokenIdUpper s) }
{
data TokenPos = TokenPos AlexPosn Token
    deriving (Eq, Show)

data Token = TokenLet
    | TokenLineSep
    | TokenIn
    | TokenWhere
    | TokenMatch
    | TokenWith
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenData
    | TokenOf
    | TokenNum Int
    | TokenIdLower String
    | TokenIdUpper String
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
    | TokenRefl
    | TokenSubst
    | TokenBy
    | TokenContra
    | TokenGt
    | TokenLt
    | TokenUnderscore
    deriving (Eq,Show)
scanTokens = alexScanTokens

getPos :: TokenPos -> SourcePos
getPos (TokenPos (AlexPn _ line col) _) = Posn line col

getToken :: TokenPos -> Token
getToken (TokenPos _ t) = t
}
