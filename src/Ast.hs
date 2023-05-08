module Ast where
 
import Data.Typeable (Typeable)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Data.Function (on)

---------------------------------

data SourcePos = Posn
    !Int    -- line number
    !Int    -- column number

instance Show SourcePos where
    show (Posn line col) = (show line) ++ ":" ++ (show col) ++ ":"

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
  -- `let (x,y) = a in b`   ->   LetPair a (bind (x,y) b)
  | LetPair Term (Unbound.Bind (TermName, TermName) Term) -- destructor for Sigma types
  -- `let x = a in b`   ->   LetPair a (bind x b)
  | Let Term (Unbound.Bind TermName Term)   -- convenience
  | Pos SourcePos Term
  -- TODO:
  -- Type equality (Eq)
  -- Type Constructors (TCon)
  -- Term Constructors (DCon)
  -- case analysis (Match)
  deriving (Show, Generic, Unbound.Subst Term)

getTermPos :: Term -> SourcePos
getTermPos (App a _) = getTermPos a
getTermPos (Pi a _) = getTermPos a
getTermPos (Pos p _) = p
getTermPos t       = error $ "cannot get posn of " ++ show t

-- Ignore Pos and Ann for equivalence checking
instance Unbound.Alpha Term where
  aeq' ctx (Ann a _) b = Unbound.aeq' ctx a b
  aeq' ctx a (Ann b _) = Unbound.aeq' ctx a b
  aeq' ctx (Pos _ a) b = Unbound.aeq' ctx a b
  aeq' ctx a (Pos _ b) = Unbound.aeq' ctx a b
  aeq' ctx a b = (Unbound.gaeq ctx `on` from) a b

instance Unbound.Alpha SourcePos where
  aeq' _ _ _ = True
  fvAny' _ _ = pure
  open _ _ = id
  close _ _ = id
  isPat _ = mempty
  isTerm _ = mempty
  nthPatFind _ = mempty
  namePatFind _ = mempty
  swaps' _ _ = id
  freshen' _ x = return (x, mempty)
  lfreshen' _ x cont = cont x mempty
  acompare' _ _ _ = EQ

-- Substitutions ignore source positions
instance Unbound.Subst b SourcePos where
    subst _ _ = id
    substs _ = id
    substBvs _ _ = id

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

mkDef :: TermName -> Term -> Decl
mkDef x term = Def x term

data Module = Module
  { --moduleName :: MName,
    --moduleImports :: [ModuleImport],
    moduleDecls :: [Decl]
  }
  deriving (Show, Generic, Typeable)
