module TypeCheck
    ( typeCheckModule
    , inferType
    , checkType
    ) where

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
typeCheckTerm (If a b c)     Nothing = do
    checkType a BoolType
    t <- inferType b
    checkType c t
    return t
typeCheckTerm (Lam bnd)      Nothing = undefined -- TODO: throw error
-- checking mode
typeCheckTerm (If a b c)    (Just t) = do
    checkType a BoolType
    -- Flow sensitivity:
    --  if a is `Var x`, then add `x = True` or `x = False` to respective branches
    a' <- Equal.whnf a
    let defBool (Var x) b = [mkDef x (BoolLit b)]
        defBool _ _       = []
    extendCtxs (defBool a' True) $ checkType b t
    extendCtxs (defBool a' False) $ checkType c t
    return t
typeCheckTerm (Pair a b)    (Just t) = do
    let Sigma typA bnd = t              -- fail if t is not a Sigma type
    (x, typB) <- Unbound.unbind bnd
    checkType a typA
    extendCtxs [mkSig x typA, mkDef x a] (checkType b typB)
    return t
typeCheckTerm (LetPair rhs bnd) (Just t) = do
    pairType <- inferType rhs
    (Sigma typA bnd2) <- Equal.whnf pairType  -- `rhs` must be of Sigma type
    ((x,y), body) <- Unbound.unbind bnd
    let typB = Unbound.instantiate bnd2 [Var x]
    rhs' <- Equal.whnf rhs
    let defPair (Var z) = [mkDef z $ Pair (Var x) (Var y)]
        defPair _       = []
    extendCtxs ([mkSig x typA, mkSig y typB] ++ defPair rhs') $ checkType body t
    return t
typeCheckTerm (Let rhs bnd) mode = do
    typA <- inferType rhs
    (x, body) <- Unbound.unbind bnd
    typB <- extendCtxs [mkSig x typA, mkDef x rhs] $ typeCheckTerm body mode
    case mode of
        -- avoid scoping issues by substituting x with its definition
        Just _  -> return typB
        Nothing -> return $ Unbound.subst x rhs typB
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

typeCheckDef :: Decl -> TcMonad Type
typeCheckDef (Def name term) = do
    h <- lookupHint name
    case h of
        Nothing -> inferType term
            -- TODO: add these to ctx?
        Just s  -> do
            checkType term (sigType s)
            return $ sigType s
typeCheckDef _    = error "need a def"

typeCheckModule :: Module -> TcMonad [Type]
typeCheckModule mod = do
    let decls = moduleDecls mod

        -- put all the defs into the ctx
        addDef  :: Decl -> [Decl] -> [Decl]
        addDef d@(Def _ _) acc = d:acc
        addDef _ acc = acc

        -- put all the sigs into the hints
        addHint :: Decl -> [Sig] -> [Sig]
        addHint (TypeSig s) acc = s:acc
        addHint _ acc = acc

        ctx   = (foldr addDef  [] decls :: [Decl])
        hints = (foldr addHint [] decls :: [Sig])

        addCtx = extendCtxs ctx
        addHints = extendHints hints

    -- TcMonad [Type]
    --liftM (zip ((\(Def n _) -> n) ctx)) $
    addCtx $ addHints $ mapM typeCheckDef ctx
    -- go through each top level def
    -- type check each one against its signature (if exists)
    -- otherwise infer the type
    -- print the type of each def
