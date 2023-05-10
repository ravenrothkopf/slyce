module TypeCheck
    ( typeCheckModule
    , typeCheckDef
    , inferType
    , checkType
    ) where

import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Control.Monad.Except as Ex
import Debug.Trace (trace)
import Context
import Ast
import PrettyPrint
import qualified Equality as Equal
import Data.List (intercalate)

-- abstraction from `Term -> Ctx -> Maybe Type`
inferType :: Term -> TcMonad Type
inferType term = typeCheckTerm term Nothing

-- abstraction from `Term -> Type -> Ctx -> Bool`
checkType :: Term -> Type -> TcMonad ()
checkType term (Pos _ typ) = checkType term typ
checkType term (Ann typ _) = checkType term typ
checkType term typ = do
    typ' <- Equal.whnf typ
    typeCheckTerm term (Just typ')
    return ()

-- second argument is Nothing if used in inference mode
typeCheckTerm :: Term -> Maybe Type -> TcMonad Type
typeCheckTerm (Pos p term) mode = do
    extendSourceLocation p term $ typeCheckTerm term mode

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
    (typA, bnd) <- Equal.ensurePi t
    checkType b typA
    return $ Unbound.instantiate bnd [b]

typeCheckTerm (Ann term typ) Nothing = do
    checkType typ U 
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

typeCheckTerm l@(Lam bnd)      Nothing = do
    err $ "cannot infer type of lambda:\n\t" ++ ppTerm l

typeCheckTerm (EqType a b) Nothing = do
    typA <- inferType a
    --typB <- inferType b
    --Equal.equal typA typB
    -- TODO: checkType b typA instead of inferType b and equal?
    checkType b typA
    return U

-- checking mode

typeCheckTerm (If a b c)    (Just t) = do
    checkType a BoolType
    -- Flow sensitivity:
    --  if a is `Var x`, then add `x = True` or `x = False` to respective branches
    a' <- Equal.whnf a

    loc <- head <$> getSourceLocation
    traceMonad ("a' = " ++ (show a') ++ "\naka " ++ ppTerm a' ++ " at " ++ show loc ++ "\n") ()

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
    return $ Sigma typA (Unbound.bind x typB)

typeCheckTerm (LetPair rhs bnd) (Just t) = do
    ((x,y), body) <- Unbound.unbind bnd
    pairType <- inferType rhs
    (Sigma typA bnd2) <- Equal.whnf pairType  -- `rhs` must be of Sigma type
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
    -- get body of Lam and output type of Pi, unbinding them with the same name.
    (x,body,_,typB) <- Unbound.unbind2Plus bnd bnd2
    extendCtx (mkSig x typA) (checkType body typB) -- add x:A to ctx
    return $ Pi typA bnd2

typeCheckTerm t@(Lam _) (Just typ) = do
    err $ "cannot check term <== type where term is " ++ show t ++ " and type is " ++ show typ

typeCheckTerm (Contra a) (Just typ) = do
    typA <- inferType a
    (a1, a2) <- Equal.ensureEqType typA
    BoolLit True  <- Equal.whnf a1
    BoolLit False <- Equal.whnf a2
    return typ

typeCheckTerm Refl (Just t@(EqType a b)) = do
    Equal.equal a b
    return t

typeCheckTerm Refl (Just typ) = do
    err $ "Expected Refl to have Eq type, found: " ++ show typ

-- a is the term; y is the proof
typeCheckTerm (Subst a y) (Just typ) = do
    -- infer the type of the proof
    typB <- inferType y
    -- enforce that the proof has an equality type
    (left, right) <- Equal.ensureEqType typB
    left' <- Equal.whnf left
    right' <- Equal.whnf right
    y' <- Equal.whnf y
    let decls' = case (left', right') of
                     (Var l, Var r) | l == r -> [] -- if a = a, then a = a
                     (Var x, r) -> [mkDef x r]
                     (l, Var x) -> [mkDef x l]
                     (_, _)     -> []
        refl'  = case y' of
                     Var v  -> [mkDef v Refl]
                     _      -> []
    extendCtxs (decls' ++ refl') $ checkType a typ
    return typ

typeCheckTerm term (Just typ) = do
    typ' <- inferType term
    extendErr (Equal.equal typ typ') ("Inferred type:\n" ++ (show typ') ++ "\ndiffers from expected type:\n" ++ (show typ))
    return typ'

-- TODO: see examples/pair.sly. this q cannot be called p. there is some name capture problem.

-- | Type check a definiiton against its signature.
--   If there is no signature, infer the type.
typeCheckDef :: Decl -> TcMonad Type
typeCheckDef (Def name term) = do
    h <- lookupHint name
    case h of
        Nothing -> inferType term
            -- TODO: add these to ctx?
        Just s  -> do
            traceMonad ("typehint for " ++ (show name) ++ " = " ++ (ppTerm term) ++ ": ") s
            checkType (sigType s) U
            checkType term (sigType s)
            return $ sigType s
typeCheckDef _    = err $ "typeCheckDef requires a Def as input"

typeCheckModule :: Module -> TcMonad [(TermName, Type)]
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

        defs  = (foldr addDef  [] decls :: [Decl])
        hints = (foldr addHint [] decls :: [Sig])
        ctx   = defs ++ map TypeSig hints

        addCtx = extendCtxs ctx
        addHints = extendHints hints

    --traceMonad "ctx: " (map ppDecl ctx)
    trace ("ctx:\n" ++ (intercalate "\n" (map ppDecl ctx))) (return ())
    trace ("hints:\n" ++ (intercalate "\n" (map (ppDecl . TypeSig) hints))) (return ())
    traceMonad "hints: " hints

    -- TcMonad [Type]
    -- go through each top level def
    types <- addCtx $ addHints $ mapM typeCheckDef defs
    return $ zip (map (\(Def n _) -> n) ctx) types
