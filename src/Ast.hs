module Ast where
 
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
  deriving (Show, Generic)
