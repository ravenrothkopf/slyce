module TypeCheck
    ( typeCheckModule
    , typeCheckDecl
    , inferType
    , checkType
    ) where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)
import qualified Control.Monad.Except as Ex
import Control.Monad.Reader (MonadReader (local))
import Control.Monad (unless)
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

typeCheckTerm (Con name terms) Nothing = do
    res <- lookupWhichCon name
    case res of
        TCon -> do
            let tcname = name
                params = terms
            -- This uses the same judgment as DCon, but the implementation details differ.
            (Telescope tele, _) <- lookupTCon tcname
            -- Ensure actuals match telescope.
            unless (length tele == length params) $
                err $ "Type constructor " ++ tcname ++ " should have " ++ show (length tele) ++
                    " data arguments, but was given " ++ show (length params) ++ " arguments."
            -- Check that params have the correct types against the telescope.
            typeCheckTeleArgs params tele
            return U
        DCon -> do
            let dcname = name
            -- Get data constructor definition from the context.
            tcons <- lookupDConAll dcname
            case tcons of
                -- We use pattern matching to accomplish two things:
                --   Check for ambiguity: inferred DCon's can only have one associated TCon.
                --   Ensure this is not a parameterized type, since these cannot be inferred.
                [(tcname, (Telescope [], ConstructorDef _ _ (Telescope tele)))] -> do
                    let nArgs = length [d | d@(TypeSig _) <- tele]
                    unless (nArgs == length terms) $
                        err $ "Constructor " ++ dcname ++ " should have " ++ show nArgs ++
                            " data arguments, but was given " ++ show (length terms) ++ " arguments."
                    -- Check actual args against DCon telescope.
                    typeCheckTeleArgs terms tele
                    return $ Con tcname []
                [_] -> do
                    err $ "Cannot infer parameters to type constructors in data constructor: " ++ dcname
                _ -> do
                    err $ "Ambiguous data constructor: " ++ dcname

-- checking mode

typeCheckTerm (If a b c)    (Just t) = do
    checkType a BoolType
    -- Flow sensitivity:
    --  if a is `Var x`, then add `x = True` or `x = False` to respective branches
    a' <- Equal.whnf a

    --loc <- head <$> getSourceLocation
    --traceMonad ("a' = " ++ (show a') ++ "\naka " ++ ppTerm a' ++ " at " ++ show loc ++ "\n") ()

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

typeCheckTerm (Match scrut cases) (Just typ) = do
    -- Infer type of scrutinee.
    scrutTyp <- inferType scrut

    -- Put scrutinee in weak head normal form.
    -- This is probably unnecessary since this is only used as an argument to
    -- `unify`, which performs normalization on its arguments anyway, but we
    -- aren't gonna question it.
    scrut' <- Equal.whnf scrut

    -- Ensure that the type of a scrutinee is a type constructor.
    -- We cannot pattern match on things that aren't data types!
    -- Note: these params are the actual arguments, not the formal telescope.
    (tcname, params) <- Equal.ensureTCon scrutTyp

    -- Check each case.
    let checkCase (Case bnd) = do
    -- create list of defs that follow from unifying the scrutinee a with the pattern p
    --   this is also delayed substitution
    -- check the body of the case in the extended context against the expected type A

            -- get the telescope for the vars in the pattern
            (pat, body) <- Unbound.unbind bnd
            -- Add variables in pattern to context. These include type
            -- signatures of variables, as well as equality constraints on
            -- variables.
            decls <- declarePat pat (Con tcname params)
            -- TODO: what the fuck
            -- TODO: what are these
            decls' <- Equal.unify [] scrut' (pat2Term pat)
            extendCtxs (decls ++ decls') $ checkType body typ
            return ()
    mapM_ checkCase cases

    -- Make sure the cases are exhaustive.
    -- Q: what happens if they aren't? Is that not well-typed?
    -- Q: what happens if we don't use unsafeUnbind and instead unbind them normally?
    let pats = [fst (unsafeUnbind bnd) | (Case bnd) <- cases]
    exhaustivityCheck scrut' scrutTyp pats

    return typ

typeCheckTerm (Con dcname args) (Just typ@(Con tcname params)) = do
    DCon <- lookupWhichCon dcname
    TCon <- lookupWhichCon tcname
    -- ^^Use pattern matching to ensure type to check against is a TCon.

    -- Look up the DCon definition.
    (Telescope ttele, Telescope dtele) <- lookupDCon dcname tcname
    let nArgs = length [d | d@(TypeSig _) <- dtele]
    unless (nArgs == length args) $
        err $ "Constructor " ++ dcname ++ " should have " ++ show nArgs ++
            " data arguments, but was given " ++ show (length args) ++ " arguments."

    -- Since the data telescope may include vars from the type telescope, we
    -- need to substitute the type arguments into the data telescope.
    -- Substitute the names of type params with their actuals in the data telescope.
    dtele' <- substTypeParamsInTele ttele params dtele

    -- Check actual args against DCon telescope.
    typeCheckTeleArgs args dtele'
    return typ

typeCheckTerm term (Just typ) = do
    typ' <- inferType term
    extendErr (Equal.equal typ typ') ("Inferred type:\n" ++ (ppTerm typ') ++ "\ndiffers from expected type:\n" ++ (ppTerm typ))
    return typ'

typeCheckTerm t Nothing = do
    err $ "cannot infer type of term:\n\t" ++ ppTerm t


---------------------------
-- Match helper functions
---------------------------

-- TODO: implement exhaustivity checking.
exhaustivityCheck :: Term -> Type -> [Pattern] -> TcMonad ()
exhaustivityCheck scrut scrutType pats = do
    warn $ "Exhaustivity checking not implemented."
    return ()

----------------------------
-- Datatype helper functions
----------------------------

-- | Type check all of the types contained in a telescope.
typeCheckTele :: [Decl] -> TcMonad ()
typeCheckTele [] = return ()
typeCheckTele (Def x y : tele) = do
    tx <- inferType (Var x)
    checkType y tx
    extendCtx (mkDef x y) $ typeCheckTele tele
typeCheckTele (TypeSig sig : tele) = do
    checkType (sigType sig) U
    extendCtx (TypeSig sig) $ typeCheckTele tele
typeCheckTele tele = err $ "Invalid telescope: " ++ show tele

-- | check a list of args against the telescope for the constructor
typeCheckTeleArgs :: [Term] -> [Decl] -> TcMonad ()
typeCheckTeleArgs [] [] = return () -- nil case
typeCheckTeleArgs args (Def x y : tele) = do
    -- If you encounter an equality constraint, substitute all instances of
    -- that variable in the rest of the unprocessed telescope. tele' is the new
    -- telescope.
    tele' <- substDefsInTele [(x,y)] tele
    typeCheckTeleArgs args tele'
typeCheckTeleArgs (term:args) (TypeSig (Sig name typ) : tele) = do
    -- If you encounter a signature, check if the arg has that type.
    checkType term typ
    -- Because of dependent types, we add this definition to the rest of the telescope using substitution.
    -- Q: why not just extend the context? Is that the same as this? Is this a
    -- form of delayed substitution?
    tele' <- substDefsInTele [(name, term)] tele
    typeCheckTeleArgs args tele'
typeCheckTeleArgs [] _ = err $ "Not enough arguments for constructor."
typeCheckTeleArgs _ [] = err $ "Too many arguments for constructor."
typeCheckTeleArgs _ tele = err $ "Invalid telescope."

-- | substitute variables for their definition in a telescope
substDefsInTele :: [(TermName, Term)] -> [Decl] -> TcMonad [Decl]
substDefsInTele defs [] = return []
substDefsInTele defs (Def x y : tele) = do
    -- TODO: i have literally no idea how this works
    let tx' = Unbound.substs defs (Var x)
        ty' = Unbound.substs defs y
    decls <- Equal.unify [] tx' ty'
    decls' <- extendCtxs decls $ substDefsInTele defs tele
    return (decls ++ decls)
substDefsInTele defs (TypeSig (Sig name typ) : tele) = do
    -- sure why not
    typ' <- Equal.whnf $ Unbound.substs defs $ typ
    let sig' = mkSig name typ'
    tele' <- substDefsInTele defs tele
    return (sig' : tele')
substDefsInTele _ tele = err $ "Invalid telescope: " ++ show tele

-- | Substitute type constructor arguments into data constructor telescope,
-- since the names of the type constructor telescope may be used in the data
-- constructor telescope.
-- Takes the TCon telescope (so we can extract the parameter names), the args
-- to the telescope, and the DCon telescope to substitute them into.
substTypeParamsInTele :: [Decl] -> [Term] -> [Decl] -> TcMonad [Decl]
substTypeParamsInTele ttele params dtele = do
    let defs = zip [n | TypeSig (Sig n _) <- ttele] params
    substDefsInTele defs dtele

----------------------------
-- pattern helper functions
----------------------------

-- | Convert a pattern to a term 
pat2Term :: Pattern -> Term
pat2Term (PatVar x) = Var x
pat2Term (PatCon dc pats) = Con dc (map pat2Term pats) 

-- | Create Decls from a pattern of a given type.
declarePat :: Pattern -> Type -> TcMonad [Decl]
declarePat (PatVar x) typ = return [mkSig x typ]
declarePat (PatCon dcname pats) typ = do
    (tcname, params) <- Equal.ensureTCon typ    -- TODO: redundant?
    (Telescope ttele, Telescope dtele) <- lookupDCon dcname tcname
    dtele' <- substTypeParamsInTele ttele params dtele
    declarePats dcname pats dtele'

-- | Create Decls from a list of patterns and associated data constructor + telescope.
declarePats :: DCName -> [Pattern] -> [Decl] -> TcMonad [Decl]
-- Telescope Decl which is an equality constraint on variable x.
declarePats dcname pats (Def x y:tele) = do
    -- Extend the context so that the rest of the pattern has access to the constraint.
    let constraint = mkDef x y
    decls <- extendCtx constraint $ declarePats dcname pats tele
    return (constraint : decls)
declarePats dcname (pat : pats) (TypeSig (Sig name typ) : tele) = do
    decls <- declarePat pat typ
    -- Dependent types, so we need to make the term available to the rest of the telescope.
    let term = pat2Term pat
    decls' <- extendCtxs decls $ declarePats dcname pats (Unbound.subst name term tele)
    return (decls ++ decls')
declarePats _ [] [] = return []
declarePats dc [] _ = err $ "Not enough patterns in match for data constructor " ++ dc
declarePats dc _ [] = err $ "Too many patterns in match for data constructor " ++ dc
declarePats dc _ _  = err $ "Invalid telescope: " ++ dc

----------------------------
-- module processing
----------------------------

-- After processing a decl, either add something to the hints or to the context.
data EnvItem = AddHint Sig | AddCtx [Decl]

typeCheckDecl :: Decl -> TcMonad EnvItem
-- Type check a definition against its signature.
-- If there is no signature, infer the type.
typeCheckDecl (Def name term) = do
    h <- lookupHint name
    case h of
        Nothing -> do
            typ <- inferType term
            return $ AddCtx [mkSig name typ, mkDef name term]
        Just s  -> do
            --traceMonad ("typehint for " ++ (show name) ++ " = " ++ (ppTerm term) ++ ": ") s

            -- No need to check sig. If sig is in hints, then it was already
            -- checked by the function below.
            extendCtx (TypeSig s) $ checkType term (sigType s)
            return $ AddCtx [TypeSig s, mkDef name term]
typeCheckDecl (TypeSig s) = do
    checkType (sigType s) U
    return $ AddHint s
typeCheckDecl (Data tcname (Telescope ttele) constructors) = do
    -- Ensure the telescope is well-typed.
    typeCheckTele ttele
    -- Check each data constructor's telescope.
    let checkConstructorDef defn@(ConstructorDef pos dcname (Telescope dtele)) =
            --extendSourceLocation pos defn $
                extendCtx (DataSig tcname (Telescope ttele)) $
                     extendCtxTele ttele $
                        typeCheckTele dtele
    mapM_ checkConstructorDef constructors
    -- TODO: ensure uniqueness of constructor names within a given data type declaration
    warn $ "Checked data def:\n" ++ ppDecl (Data tcname (Telescope ttele) constructors)
    return $ AddCtx [Data tcname (Telescope ttele) constructors]
typeCheckDecl (DataSig _ _) = err $ "internal construct"

typeCheckModule :: Module -> TcMonad [Sig]
typeCheckModule modu = do
    let decls    = moduleDecls modu
        --defs     = [d | d <- decls, (Def _ _) = d]
        --hints    = [d | d <- decls, (TypeSig _) = d]
        --datadefs = [d | d <- decls, (Data _) = d]

        --extendData 

    --traceMonad "ctx: " (map ppDecl ctx)
    --trace ("ctx:\n" ++ (intercalate "\n" (map ppDecl ctx))) (return ())
    --trace ("hints:\n" ++ (intercalate "\n" (map (ppDecl . TypeSig) hints))) (return ())
    --traceMonad "hints: " hints

    -- Go through each top level def and type check them one by one, extending
    -- the environment as you go.
    -- Build up a list of signatures to return, identifying the type of each
    -- top level def.
    let extendCNames = local (\e -> e{getCNames = moduleConstructorNames modu})

    extendCNames $ foldr process (pure [] :: TcMonad [Sig]) decls
        where
            process :: Decl -> TcMonad [Sig] -> TcMonad [Sig]
            process d sigs = do
                action <- typeCheckDecl d
                case action of
                    AddHint sig  -> do
                        extendHints [sig] sigs
                    AddCtx decls -> do
                        -- When a term has been type checked, add its signature
                        ([s | TypeSig s <- decls]++) <$> extendCtxs decls sigs
