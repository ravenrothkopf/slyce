module Context where

--import PrettyPrint (D (..), Disp (..), Doc, SourcePos, render)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    MonadIO (..),
    runExceptT,
    unless,
    void,
  )
import Control.Monad.Reader
  ( MonadReader (local),
    ReaderT (runReaderT),
    asks,
  )

import Ast

---------------------------------

-- Type Checking Monad
-- A stack of four monads carrying various info:
-- IO, Errors, Environment, Freshness
type TcMonad = Unbound.FreshMT (ReaderT Env (ExceptT Err IO))

data Err = Err

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    ctx :: [Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    globals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [Sig]--,
    -- | what part of the file we are in (for errors/warnings)
    --sourceLocation :: [SourceLocation]
  }

emptyEnv :: Env
emptyEnv =
  Env
    { ctx = [],
      globals = 0,
      hints = []--,
      --sourceLocation = []
    }

---------------------------------

-- abstraction from `Term -> Ctx -> Maybe Type`
inferType :: Term -> TcMonad Type
inferType term = typeCheckTerm term Nothing

-- abstraction from `Term -> Type -> Ctx -> Bool`
checkType :: Term -> Type -> TcMonad ()
checkType term typ = void $ typeCheckTerm term (Just typ)

-- second argument is Nothing if used in inference mode
typeCheckTerm :: Term -> Maybe Type -> TcMonad Type
typeCheckTerm (Var x)        Nothing = lookupType x >>= return . sigType
typeCheckTerm U              Nothing = return U -- TODO: err?
typeCheckTerm (Pi typA bnd)  Nothing = return U -- TODO: check if valid
typeCheckTerm (App t1 t2)    Nothing = do
    (Pi typA bnd) <- inferType t1
    checkType t2 typA
    (x, typB) <- Unbound.unbind bnd
    return $ Unbound.subst x typA typB
typeCheckTerm (Ann term typ) Nothing = return typ
typeCheckTerm (Lam bnd)      Nothing = undefined -- TODO: throw error
typeCheckTerm (Lam bnd) (Just (Pi typA bnd2)) =
    -- add x:A to context
    -- get body of Lam
    -- get typB
    checkType body (Just typB)
typeCheckTerm (Lam _)      (Just nf) = undefined -- TODO: throw error
typeCheckTerm term (Just typ) = do
    typ' <- inferTyp term
    unless (Unbound.aeq typ typ') $ -- throw err
    return typ

{-
runtc m = runExceptT $ runReaderT (Unbound.runFreshMT m) emptyEnv
ravenName = Unbound.s2n "raven"
runtc $ typeCheckTerm (Var ravenName) Nothing

-}

lookupType :: TermName -> TcMonad Sig
lookupType = undefined

extendCtx :: Decl -> TcMonad Term -> TcMonad Term
extendCtx = undefined

--err :: (Disp a) => [a] -> TcMonad b
--err = undefined
--
--warn :: (Disp a) => a -> TcMonad ()
--warn = undefined
