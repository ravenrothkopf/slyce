module Ast where
 
import Data.Typeable (Typeable)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound

---------------------------------

type TermName = Unbound.Name Term

type Type = Term

data Term
  = U                                      -- the type of types `U`
  | Var TermName                           -- variables `x`
  | Lam (Unbound.Bind TermName Term)       -- abstraction `λx. a`
  | App Term Term                          -- application `a b`
  | Pi Type (Unbound.Bind TermName Type)   -- function type `(x : A) -> B`
  | Ann Term Type                          -- annotated terms `(a : A)`
  deriving (Show, Generic, Unbound.Alpha, Unbound.Subst Term)

{-
xName = Unbound.string2Name "x"
yName = Unbound.string2Name "y"

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
  | -- | A potentially (recursive) definition of
    -- a particular name, must be declared
    RecDef TermName Term
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

mkSig :: TermName -> Type -> Decl
mkSig x typ = TypeSig (Sig x typ)
