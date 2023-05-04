module TypeCheck where

import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Control.Monad.Except as Ex
import Context
import Ast
import qualified Equality as Equal

-- abstraction from `Term -> Ctx -> Maybe Type`
inferType :: Term -> TcMonad Type
inferType term = typeCheckTerm term Nothing

-- abstraction from `Term -> Type -> Ctx -> Bool`
checkType :: Term -> Type -> TcMonad ()
checkType term typ = do
    term' <- Equal.whnf term
    typeCheckTerm term' (Just typ)
    return ()

-- second argument is Nothing if used in inference mode
typeCheckTerm :: Term -> Maybe Type -> TcMonad Type
-- inference mode
typeCheckTerm (Var x)        Nothing = lookupType x >>= return . sigType
typeCheckTerm UnitType       Nothing = return U
typeCheckTerm BoolType       Nothing = return U
typeCheckTerm (Sigma typA bnd) Nothing = do
    checkType typA U
    (x, typB) <- Unbound.unbind bnd
    extendCtx (mkSig x typA) (checkType typB U)
    return U
typeCheckTerm U              Nothing = return U -- TODO: err? to avoid paradox
typeCheckTerm (Pi typA bnd)  Nothing = do
    (x, typB) <- Unbound.unbind bnd
    checkType typA U
    extendCtx (mkSig x typA) (checkType typB U)
    return U
typeCheckTerm (App a b)      Nothing = do
    t <- inferType a
    (Pi typA bnd) <- Equal.whnf t
    checkType b typA
    (x, typB) <- Unbound.unbind bnd
    return $ Unbound.subst x typA typB
typeCheckTerm (Ann term typ) Nothing = do 
    checkType term typ
    return typ
typeCheckTerm UnitLit        Nothing = return UnitType
typeCheckTerm (BoolLit True) Nothing = return BoolType
typeCheckTerm (BoolLit False) Nothing = return BoolType
typeCheckTerm (If p c e)     Nothing = do
    checkType p BoolType
    t <- inferType c
    checkType e t
    -- TODO: add definition of p to ctx
    return t
typeCheckTerm (Pair a b)    (Just t) = do
    let Sigma typA bnd = t              -- fail if t is not a Sigma type
    (x, typB) <- Unbound.unbind bnd
    checkType a typA
    extendCtx (mkSig x typA) (checkType b typB)
    -- TODO: add (WHNF-reduced?) definitions of x and y to to ctx?
    return t
typeCheckTerm (LetPair a bnd) (Just t) = do
    checkType t U
    pairType <- inferType a
    (Sigma typA bnd2) <- Equal.whnf pairType  -- `a` must be of Sigma type
    ((x,y), body) <- Unbound.unbind bnd
    let typB = Unbound.instantiate bnd2 [Var x]
    extendCtxs [mkSig x typA, mkSig y typB] (checkType body t)
    -- TODO: add (WHNF-reduced?) definitions of x and y to to ctx?
    return t
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
    Equal.equal typ typ'
    return typ

{-
runtc m = runExceptT $ runReaderT (Unbound.runFreshMT m) emptyEnv
ravenName = Unbound.s2n "raven"
runtc $ typeCheckTerm (Var ravenName) Nothing

-}

-- TODO: function to type check a Decl, handle hints, context, etc
-- TODO: function to type check all Decls in a file/module
