{
module Parser
  ( parseProgram
  , parse
  ) where

import Control.Monad.State.Strict ( State
                                , evalState
                                , get
                                , modify
                                )
import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Data.Set as Set
import Debug.Trace (trace)
import Scanner
import Ast
}

%name parse
%monad { State ConstructorNames } 
%error { parseError }
%tokentype { TokenPos }

%token
  'match'   { TokenPos p TokenMatch }
  'with'    { TokenPos p TokenWith }
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
  'subst'   { TokenPos p TokenSubst }
  'by'      { TokenPos p TokenBy }
  'Refl'    { TokenPos p TokenRefl }
  'contra'  { TokenPos p TokenContra }
  '\\'      { TokenPos p TokenLam }
  '='       { TokenPos p TokenEq }
  '->'      { TokenPos p TokenArrow }
  '>'       { TokenPos p TokenGt }
  '<'       { TokenPos p TokenLt }
  '|'       { TokenPos p TokenBar }
  ':'       { TokenPos p TokenColon }
  ';'       { TokenPos p TokenSemi }
  '||'      { TokenPos p TokenLineSep }
  ','       { TokenPos p TokenComma }
  '.'       { TokenPos p TokenDot }
  '_'       { TokenPos p TokenUnderscore }
  '*'       { TokenPos p TokenStar }
  '('       { TokenPos p TokenLparen }
  ')'       { TokenPos p TokenRparen }
  '{'       { TokenPos p TokenLbrace }
  '}'       { TokenPos p TokenRbrace }
  '['       { TokenPos p TokenLbracket }
  ']'       { TokenPos p TokenRbracket }
  idLower   { TokenPos p (TokenIdLower s) }
  idUpper   { TokenPos p (TokenIdUpper s) }

%right '->'
%nonassoc NOELSE 'else'

%right '('
%left ')'
%nonassoc '.'
%right ':'
%nonassoc CON

%%
program --> Module
    : decls          {% get >>= \c -> return $ Module (reverse $1) c }

decls --> [Decl]
    : decls '.' decl  { $3 : $1 }
    | decls '.'       { $1 }
    | decl            { [$1] }
    | {- empty -}     { [] }

decl --> Decl
    : typeSig           { TypeSig $1 } 
    | variable '=' term     { Def (snd $1) $3 }
    | 'data' conName telescope 'where' constructors
      {% addTC (snd $2) >> return (Data (snd $2) (Telescope (reverse $3)) (reverse $5)) }

-- variables are lower case
variable --> (SourcePos, TermName)
    : idLower
    { let x = (getVar $ getToken $1)
        in (getPos $1, Unbound.s2n x) }

-- constructors are upper case
conName --> (SourcePos, String)
    : idUpper
    { let x = (getVar $ getToken $1)
        in (getPos $1, x) }

typeSig --> Sig
    : variable ':' term  { Sig (snd $1) $3 }

-- zero or more
telescope --> [Decl]
    : telescope '(' teleDecl ')'  { $3:$1 }
    | {- empty -}                 { [] }

-- Only typeSigs and defs allowed. Names are optional for typeSigs.
-- TODO: state
teleDecl --> Decl
    : typeSig             { TypeSig $1 }
    | variable '=' term   { Def (snd $1) $3 }
    | term                { TypeSig (Sig (Unbound.s2n "_") $1) }

-- zero or more
constructors --> [ConstructorDef]
    : constructors ',' constructor  { $3:$1 }
    | constructors ','              { $1 }
    | constructor                   { [$1] }
    | {- empty -}                   { [] }

-- TODO: make sure this works with recursive data definitions
constructor --> ConstructorDef
    : conName
    {% addDC (snd $1) >> return (ConstructorDef (fst $1) (snd $1) (Telescope [])) }
    | conName 'of' telescope
    {% addDC (snd $1) >> return (ConstructorDef (fst $1) (snd $1) (Telescope (reverse $3))) } 

-- p :=  x
--       _
--       K ap*
--       (p)
--       (p, p)
-- ap ::= [p] | p

pattern --> Pattern
    : '_'             { PatVar (Unbound.s2n "_") }
    | variable        { PatVar (snd $1) }
    | conName patternlist { PatCon (snd $1) (reverse $2) }
    | '(' pattern ')' { $2 }

patternlist --> [Pattern]
    : patternlist pattern  { $2:$1 }
    | {- empty -}          { [] }

cases --> [Case]
    : cases '|' case   { $3:$1 }
    | '|' case         { [$2] }
    | case             { [$1] }

case --> Case
    : pattern '->' term   { Case (Unbound.bind $1 $3) }

term --> Term
    : '\\' variable '.' term                 { Pos (getPos $1) (Lam (Unbound.bind (snd $2) $4)) }
    | '(' variable ':' term '*' term ')'     { Pos (getPos $1) (Sigma $4 (Unbound.bind (snd $2) $6)) }
    | '(' term '*' term ')'              { Pos (getPos $1) (Sigma $2 (Unbound.bind (Unbound.s2n "_") $4)) }
    | '(' variable ':' term ')' '->' term    { Pos (getPos $1) (Pi $4 (Unbound.bind (snd $2) $7)) }
    | term '->' term                     { Pos (getTermPos $1) (Pi $1 (Unbound.bind (Unbound.s2n "_") $3)) }
    | '(' term ':' term ')'              { Pos (getPos $1) (Ann $2 $4) }
    | 'if' term 'then' term 'else' term  { Pos (getPos $1) (If $2 $4 $6) }
    | 'let' '(' variable ',' variable ')' '=' term 'in' term
                                         { Pos (getPos $1) (LetPair $8 (Unbound.bind (snd $3, snd $5) $10)) }
    | 'let' variable '=' term 'in' term      { Pos (getPos $1) (Let $4 (Unbound.bind (snd $2) $6)) }
    | 'subst' term 'by' term             { Pos (getPos $1) (Subst $2 $4) }
    | '(' term ',' term ')'              { Pos (getPos $1) (Pair $2 $4) }
    | term '=' term                      { Pos (getTermPos $1) (EqType $1 $3) }
    | 'contra' term                      { Pos (getPos $1) (Contra $2) }
    | 'match' term 'with' cases          { Pos (getPos $1) (Match $2 (reverse $4)) }
    | conApp                             { $1 }
    | funApp                             { $1 }
    | '(' term ')'                       { Pos (getPos $1) $2 }
    | variable                               { Pos (fst $1) (Var (snd $1)) }
    | conName %prec CON                  { Pos (fst $1) (Con (snd $1) []) }
    | 'U'                                { Pos (getPos $1) U }
    | 'Unit'                             { Pos (getPos $1) UnitType }
    | '(' ')'                            { Pos (getPos $1) UnitLit }
    | 'Bool'                             { Pos (getPos $1) (BoolType) }
    | 'True'                             { Pos (getPos $1) (BoolLit True) }
    | 'False'                            { Pos (getPos $1) (BoolLit False) }
    | 'Refl'                             { Pos (getPos $1) Refl }

app --> Term
    : conApp                             { $1 }
    | funApp                             { $1 }

conApp --> Term
    : conName args { Pos (fst $1) (Con (snd $1) (reverse $2)) }
    {-
    {% get >>= \s ->
       let n = snd $1 
           res = \t -> Pos (fst $1) (t n (reverse $2))
       in if n `Set.member` (tconNames s)
          then return $ res TCon
          else if n `Set.member` (dconNames s)
               then return $ res DCon
               else parseError [TokenPos (AlexPn 0 0 0) (TokenIdUpper (show (fst $1)))] }
               -}

args --> [Term]
    : args atom             { $2:$1 }
    | args '(' term ')'     { $3:$1 }
    | {- empty -}   { [] }

funApp --> Term
    : term atom                          { Pos (getTermPos $1) (App $1 $2) }

atom --> Term
    : '(' term ')'                       { Pos (getPos $1) $2 }
    | variable                               { Pos (fst $1) (Var (snd $1)) }
    | conName %prec CON                  { Pos (fst $1) (Con (snd $1) []) }
    | 'U'                                { Pos (getPos $1) U }
    | 'Unit'                             { Pos (getPos $1) UnitType }
    | '(' ')'                            { Pos (getPos $1) UnitLit }
    | 'Bool'                             { Pos (getPos $1) (BoolType) }
    | 'True'                             { Pos (getPos $1) (BoolLit True) }
    | 'False'                            { Pos (getPos $1) (BoolLit False) }
    | 'Refl'                             { Pos (getPos $1) Refl }

{
addTC :: TCName -> State ConstructorNames ()
addTC n = modify (\cs -> cs{tconNames = Set.insert n (tconNames cs)})

addDC :: DCName -> State ConstructorNames ()
addDC n = modify (\cs -> cs{dconNames = Set.insert n (dconNames cs)})

parseError :: [TokenPos] -> a
parseError (p@(TokenPos _ t):_) = error $ show (getPos p) ++ "Parse error at token " ++ show t
parseError [] = error $ "Parse error with no tokens"

parseProgram :: String -> Module
parseProgram s = evalState (parse $ scanTokens s) emptyConstructorNames

getVar :: Token -> String
getVar (TokenIdUpper s) = s
getVar (TokenIdLower s) = s
getVar _ = error "not a var"
}
