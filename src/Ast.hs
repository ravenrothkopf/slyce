module Ast where
 
import Data.Typeable (Typeable)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound

---------------------------------

type TermName = Unbound.Name Term

type Type = Term

data Term
  = Var TermName                            -- variables `x`
  | Lam (Unbound.Bind TermName Term)        -- abstraction `λx. a`
  | Pi Type (Unbound.Bind TermName Type)    -- function type `(x : A) -> B`
  | App Term Term                           -- application `a b`
  | Ann Term Type                           -- annotated terms `(a : A)`
  | U                                       -- the type of types `U`
  | UnitType                                -- Unit type
  | UnitLit                                 -- sole inhabitant of Unit, `()`
  | BoolType                                -- Bool type
  | BoolLit Bool                            -- two inhabitants of Bool, True and False
  | If Term Term Term                       -- eliminator for Bool, `p ? c | e`
  | Sigma Type (Unbound.Bind TermName Type) -- Sigma type, `(x : A, B)`
  | Pair Term Term                          -- constructor for Sigma types, `(a,b)`
  | LetPair Term (Unbound.Bind (TermName, TermName) Term) -- destructor for Sigma types
  -- TODO:
  -- Type equality (Eq)
  -- Type Constructors (TCon)
  -- Term Constructors (DCon)
  -- case analysis (Match)
  -- Let pattern matching
  deriving (Show, Generic, Unbound.Alpha, Unbound.Subst Term)

{-
xName = Unbound.s2n "x"
yName = Unbound.s2n "y"

idx = Lam (Unbound.bind xName (Var xName))
idy = Lam (Unbound.bind yName (Var yName))

> Unbound.aeq idx idy
True
-}

-- | A type declaration (or type signature)
data Sig = Sig {sigName :: TermName, sigType :: Type}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

data Decl
  = -- | Declaration for the type of a term
    TypeSig Sig
  | -- | The definition of a particular name, must
    -- already have a type declaration in scope
    Def TermName Term
--  | -- | A potentially (recursive) definition of
    -- a particular name, must be declared
    --RecDef TermName Term
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

mkSig :: TermName -> Type -> Decl
mkSig x typ = TypeSig (Sig x typ)

data Module = Module
  { --moduleName :: MName,
    --moduleImports :: [ModuleImport],
    moduleDecls :: [Decl]
  }
  deriving (Show, Generic, Typeable)
