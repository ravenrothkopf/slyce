{
module Parser
  ( parseProgram
  , parse
  ) where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Scanner
import Ast
}

%name parse
%error { parseError }
%tokentype { TokenPos }

%token
  'of'      { TokenPos p TokenOf }
  'data'    { TokenPos p TokenData }
  'if'      { TokenPos p TokenIf }
  'then'    { TokenPos p TokenThen }
  'else'    { TokenPos p TokenElse }
  'let'     { TokenPos p TokenLet }
  'in'      { TokenPos p TokenIn }
  'where'   { TokenPos p TokenWhere }
  'U'       { TokenPos p TokenType }
  'Bool'    { TokenPos p TokenBool }
  'True'    { TokenPos p TokenTrue }
  'False'   { TokenPos p TokenFalse }
  'Unit'    { TokenPos p TokenUnit }
  '\\'      { TokenPos p TokenLam }
  '='       { TokenPos p TokenEq }
  '->'      { TokenPos p TokenArrow }
  '|'       { TokenPos p TokenBar }
  ':'       { TokenPos p TokenColon }
  ';'       { TokenPos p TokenSemi }
  '||'      { TokenPos p TokenLineSep }
  ','       { TokenPos p TokenComma }
  '.'       { TokenPos p TokenDot }
  '('       { TokenPos p TokenLparen }
  ')'       { TokenPos p TokenRparen }
  '{'       { TokenPos p TokenLbrace }
  '}'       { TokenPos p TokenRbrace }
  '['       { TokenPos p TokenLbracket }
  ']'       { TokenPos p TokenRbracket }
  VAR       { TokenPos p (TokenVar s) }

%nonassoc NOELSE 'else'

%%
program            --> Module
    : decls          { Module $1 }
    | lines decls    { Module $2 }
    | {- empty -}    { Module [] }

lines              --> ()
    : '||' lines     { () }
    | {- empty -}    { () }

decls                 --> [Decl]
    : decl lines decls  { $1 : $3 }
    | decl lines        { [$1] }

decl               --> Decl
    : typeSig        { TypeSig $1 } 
    | name '=' term  { Def (snd $1) $3 }

name               --> (SourcePos, TermName)
    : VAR            { (getPos $1, Unbound.s2n (getVar . getToken $ $1)) }

typeSig            --> Sig
    : name ':' term  { Sig (snd $1) $3 }

term                                   --> Term
    : '\\' name '.' term                 { Pos (getPos $1) (Lam (Unbound.bind (snd $2) $4)) }
    | '(' name ':' term ')' '->' term    { Pos (getPos $1) (Pi $4 (Unbound.bind (snd $2) $7)) }
    | term '->' term                     { Pos (getTermPos $1) (Pi $1 (Unbound.bind (Unbound.s2n "_") $3)) }
    | term term                          { Pos (getTermPos $1) (App $1 $2) }
    | '(' term ':' term ')'              { Pos (getPos $1) (Ann $2 $4) }
    | 'U'                                { Pos (getPos $1) U }
    | 'Unit'                             { Pos (getPos $1) UnitType }
    | '(' ')'                            { Pos (getPos $1) UnitLit }
    | 'Bool'                             { Pos (getPos $1) (BoolType) }
    | 'True'                             { Pos (getPos $1) (BoolLit True) }
    | 'False'                            { Pos (getPos $1) (BoolLit False) }
    | 'if' term 'then' term 'else' term  { Pos (getPos $1) (If $2 $4 $6) }
    | '(' name ':' term ',' term ')'     { Pos (getPos $1) (Sigma $4 (Unbound.bind (snd $2) $6)) }
    | '(' term ',' term ')'              { Pos (getPos $1) (Pair $2 $4) }
    | 'let' '(' name ',' name ')' '=' term 'in' term { Pos (getPos $1) (LetPair $8 (Unbound.bind (snd $3, snd $5) $10)) }
    | 'let' name '=' term 'in' term      { Pos (getPos $1) (Let $4 (Unbound.bind (snd $2) $6)) }
    | name                               { Pos (fst $1) (Var (snd $1)) }

{
parseError :: [TokenPos] -> a
parseError (p@(TokenPos _ t):_) = error $ show (getPos p) ++ "Parse error at token " ++ show t
parseError [] = error $ "Parse error with no tokens"

parseProgram :: String -> Module
parseProgram = parse . scanTokens

getVar :: Token -> String
getVar (TokenVar s) = s
getVar _ = error "not a var"
}
