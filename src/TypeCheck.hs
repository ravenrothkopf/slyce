module TypeCheck where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Except
import Context
import qualified Context as Env
import Ast

-- abstraction from `Term -> Ctx -> Maybe Type`
inferType :: Term -> TcMonad Type
inferType term = typeCheckTerm term Nothing

-- abstraction from `Term -> Type -> Ctx -> Bool`
checkType :: Term -> Type -> TcMonad ()
checkType term typ = typeCheckTerm term (Just typ) >> return ()

-- second argument is Nothing if used in inference mode
typeCheckTerm :: Term -> Maybe Type -> TcMonad Type
-- inference mode
typeCheckTerm (Var x)        Nothing = Env.lookupType x >>= return . sigType
typeCheckTerm U              Nothing = return U -- TODO: err? to avoid paradox
typeCheckTerm (Pi typA bnd)  Nothing = do
    (x, typB) <- Unbound.unbind bnd
    checkType typA U
    extendCtx (mkSig x typA) (checkType typB U)
    return U
typeCheckTerm (App t1 t2)    Nothing = do
    (Pi typA bnd) <- inferType t1       -- TODO: also allow Ann (Pi ...)
    checkType t2 typA
    (x, typB) <- Unbound.unbind bnd
    return $ Unbound.subst x typA typB
typeCheckTerm (Ann term typ) Nothing = do 
    checkType term typ
    return typ
typeCheckTerm (Lam bnd)      Nothing = undefined -- TODO: throw error
-- checking mode
typeCheckTerm (Lam bnd) (Just (Pi typA bnd2)) = do
    (x,  body) <- Unbound.unbind bnd    -- get body of Lam
    (x2, typB) <- Unbound.unbind bnd2   -- get typB
    extendCtx (mkSig x typA) (checkType body typB) -- add x:A to ctx
    return $ Pi typA bnd2
typeCheckTerm (Lam _)      (Just nf) = undefined -- TODO: throw error
typeCheckTerm term (Just typ) = do
    typ' <- inferType term
    unless (Unbound.aeq typ typ') $ error "type mistmatch" -- throw err
    return typ

{-
runtc m = runExceptT $ runReaderT (Unbound.runFreshMT m) emptyEnv
ravenName = Unbound.s2n "raven"
runtc $ typeCheckTerm (Var ravenName) Nothing

-}
