module Ast where
 
import Data.Typeable (Typeable)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set

---------------------------------

data SourcePos = Posn
    !Int    -- line number
    !Int    -- column number

instance Show SourcePos where
    show (Posn line col) = (show line) ++ ":" ++ (show col) ++ ":"

type TermName = Unbound.Name Term

type TCName = String
type DCName = String

-- This is used in the parser to collect constructor names as they are defined,
-- in order to distinguish TCon and DCon from function application.
data ConstructorNames = ConstructorNames
  { tconNames :: Set String,
    dconNames :: Set String
  } deriving (Show, Eq, Ord, Generic, Typeable)

emptyConstructorNames :: ConstructorNames
emptyConstructorNames = ConstructorNames Set.empty Set.empty
-- TODO: add built-in data types

type Type = Term

data Term
  = Var TermName                            -- variables `x`
  | Lam (Unbound.Bind TermName Term)        -- abstraction `λx. a`
  | Pi Type (Unbound.Bind TermName Type)    -- function type `(x : A) -> B`
  | App Term Term                           -- application `a b`
  | Ann Term Type                           -- annotated terms `(a : A)`
  | Pos SourcePos Term                      -- source position information (line and column #)
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
  | Refl                                    -- `refl` value
  | Contra Term                             -- `contra` value that witnesses a contradictory type
  | EqType Term Term                        -- equality type `a = b`
  | Subst Term Term                         -- substitute one type for another, `subst t1 by t2`
  | TCon TCName [Term]                      -- type constructor application
  | DCon DCName [Term]                      -- term/data constructor application
  | Match Term [Case]                       -- pattern matching a term over cases
  deriving (Show, Generic)

newtype Case = Case (Unbound.Bind Pattern Term)
    deriving (Show, Generic, Typeable)
    deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

data Pattern = PatCon DCName [Pattern]
             | PatVar TermName
    deriving (Show, Eq, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

data ConstructorDef = ConstructorDef SourcePos DCName Telescope
    deriving (Show, Generic)
    deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- decls must be typesigs
newtype Telescope = Telescope [Decl]
    deriving (Show, Generic)
    deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

instance Unbound.Subst Term Term where
  isvar (Var x) = Just (Unbound.SubstName x)
  isvar _ = Nothing

getTermPos :: Term -> SourcePos
getTermPos (Pos p _) = p
--getTermPos (App a _) = getTermPos a
--getTermPos (Pi a _) = getTermPos a
--getTermPos (Ann a _) = getTermPos a
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

-- | A type declaration (or type signature).
data Sig = Sig {sigName :: TermName, sigType :: Type}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

data Decl
    -- Type signatures appear in ctx after a definition is type checked.
    = TypeSig Sig
    -- Definitions are added to the context after being type checked.
    | Def TermName Term
    -- Data constructors are added to the context after being processed.
    | Data TCName Telescope [ConstructorDef]
    -- Data signatures are used internally to provide a signature for data constructors.
    | DataSig TCName Telescope
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
