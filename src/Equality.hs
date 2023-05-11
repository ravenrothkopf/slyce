module Equality
    ( equal
    , ensurePi
    , ensureEqType
    , ensureTCon
    , unify
    , whnf
    ) where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad (guard)
import Control.Monad.Except (unless, catchError, zipWithM, zipWithM_)
import Ast
import Context
import PrettyPrint
import Data.List (intercalate)

---------------------------------
-- | external
---------------------------------

equal :: Term -> Term -> TcMonad ()
equal a b =
    if (Unbound.aeq a b) then return () else do
        a' <- whnf a
        b' <- whnf b
        case (a',b') of
            (Var x, Var y) | x == y -> return ()
            (Lam bnd1, Lam bnd2) -> do
                (_, body1, _, body2) <- Unbound.unbind2Plus bnd1 bnd2
                equal body1 body2
            (App a1 a2, App b1 b2) -> do
                equal a1 b1
                equal a2 b2
            (Pi typA1 bnd1, Pi typA2 bnd2) -> do
                equal typA1 typA2
                (_, typB1, _, typB2) <- Unbound.unbind2Plus bnd1 bnd2
                equal typB1 typB2
            (UnitType, UnitType) -> return ()
            (UnitLit, UnitLit)   -> return ()
            (BoolType, BoolType) -> return ()
            (BoolLit True, BoolLit True) -> return ()
            (BoolLit False, BoolLit False) -> return ()
            (Refl, Refl) -> return ()
            (EqType a1 b1, EqType a2 b2) -> do
                equal a1 a2
                equal b1 b2
            (Contra a, Contra b) -> do
                equal a b
            (Subst a1 b1, Subst a2 b2) -> do
                equal a1 a2
                equal b1 b2
            (If a1 b1 c1, If a2 b2 c2) ->
                equal a1 a2 >> equal b1 b2 >> equal c1 c2
            (Sigma t1 b1, Sigma t2 b2) -> do
                equal t1 t2
                Just (_, typB1, _, typB2) <- Unbound.unbind2 b1 b2
                equal typB1 typB2
            (Pair a1 b1, Pair a2 b2) -> do
                equal a1 a2
                equal b1 b2
            (LetPair a1 bnd1, LetPair a2 bnd2) -> do
                equal a1 a2
                Just (_, b1, _, b2) <- Unbound.unbind2 bnd1 bnd2
                equal b1 b2
            (Let rhs1 bnd1, Let rhs2 bnd2) -> do
                equal rhs1 rhs2
                Just (_, body1, _, body2) <- Unbound.unbind2 bnd1 bnd2
                equal body1 body2
            (TCon c1 a1, TCon c2 a2) | c1 == c2 -> do
                zipWithM_ equal a1 a2
            (DCon c1 a1, DCon c2 a2) | c1 == c2 -> do
                zipWithM_ equal a1 a2
            (Match s1 cs1, Match s2 cs2) | length cs1 == length cs2 -> do
                equal s1 s2
                let equalCase (Case bnd1) (Case bnd2) = do
                        Just (p1, a1, p2, a2) <- Unbound.unbind2 bnd1 bnd2
                        guard (p1 == p2)
                        equal a1 a2
                zipWithM_ equalCase cs1 cs2
            (_, _) -> err $ "terms are not equal:\n\t" ++ (show a') ++ "\n\t" ++ (show b') ++ "\n"

----------------------------

-- used for returning the subcomponents of Pi types
ensurePi :: Term -> TcMonad (Type, Unbound.Bind TermName Type)
ensurePi typ = do
    nf <- whnf typ
    case nf of
        (Pi x y) -> return (x, y)
        t -> (flip extendErr) ("\nwith original type " ++ show typ) $ err $ "expected Pi type, found: " ++ show t

ensureEqType :: Term -> TcMonad (Term, Term)
ensureEqType t = do
    t' <- whnf t
    case t' of
        (EqType a b) -> return (a, b)
        _            -> err $ "expected Eq type, found: " ++ show t'

ensureTCon :: Term -> TcMonad (TCName, [Term])
ensureTCon t = do
    t' <- whnf t
    case t' of
        TCon name params -> return (name, params)
        _ -> err $ "Expected a data type but found " ++ show t'

----------------------------

whnf :: Term -> TcMonad Term
whnf v@(Var x) = do
    d <- lookupDef x
    case d of
        Just def -> whnf def
        Nothing  -> return v
whnf (Ann t _) = whnf t
whnf (Pos _ t) = whnf t
whnf (App a b) = do
    a' <- whnf a
    traceMonad ("in whnf App: a' = " ++ show a') ()
    traceMonad ("in whnf App: b = " ++ show b) ()
    b' <- whnf b
    traceMonad ("in whnf App: b' = " ++ show b') ()
    c <- getContext 
    traceMonad (concat $ map ((++"\n"). show) c) ()
    case a' of
        Lam bnd -> do                          -- beta reduction
            whnf $ Unbound.instantiate bnd [b] -- substitute b for x in body
        _       -> return $ App a' b           -- return App with normal form of a
whnf (If a b c) = do
    traceMonad ("in whnf If: a = " ++ show a) ()
    a' <- whnf a
    traceMonad ("in whnf If: a' = " ++ show a') ()
    case a' of
        BoolLit True  -> whnf b
        BoolLit False -> whnf c
        _             -> return $ If a' b c
-- TODO: should we reduce (Pair a b)?
whnf (LetPair a bnd)  = do
    a' <- whnf a
    case a' of
        Pair t1 t2 -> do
            -- substitute t1 for x and t2 for y in the body b
            whnf $ Unbound.instantiate bnd [t1, t2]
        _          -> return $ LetPair a' bnd
whnf (Let rhs bnd)    = do
    whnf $ Unbound.instantiate bnd [rhs]
whnf (Subst a y) = do
    y' <- whnf y
    case y' of
        Refl -> whnf a
        _    -> return $ Subst a y'
whnf (Match scrut cases) = do
    scrut' <- whnf scrut
    case scrut' of
        (DCon dcname args) -> f cases where
          f (Case bnd : alts) = (do
              (pat, body) <- Unbound.unbind bnd
              defs <- patternMatches scrut' pat
              whnf (Unbound.substs defs body))
                `catchError` \ _ -> f alts
          f [] = err $ "Internal error: couldn't find a matching branch for " ++
                 show scrut' ++ " in " ++ show cases
        _ -> return (Match scrut' cases)

whnf term      = return term                   -- all types and lambda

-- | Determine whether the pattern matches the argument
-- If so return the appropriate substitution
-- otherwise throws an error
patternMatches :: Term -> Pattern -> TcMonad [(TermName, Term)]
patternMatches term (PatVar x) = return [(x, term)]
patternMatches term pat = do
  t' <- whnf term
  case (t', pat) of
      (DCon d [], PatCon d' pats)   | d == d' -> return []
      (DCon d args, PatCon d' pats) | d == d' ->
          -- TODO: does this work?
         concat <$> zipWithM patternMatches args pats
      _ -> err $ "arg " ++ show t' ++ " doesn't match pattern " ++ show pat

-----------------------

-- | 'Unify' the two terms, producing a list of Defs
-- If there is an obvious mismatch, this function produces an error
-- If either term is "ambiguous" just fail instead.
unify :: [TermName] -> Term -> Term -> TcMonad [Decl]
unify ns tx ty = do
  txnf <- whnf tx
  tynf <- whnf ty
  if Unbound.aeq txnf tynf
    then return []
    else case (txnf, tynf) of
      (Var y, yty) | y `notElem` ns -> return [Def y yty]
      (yty, Var y) | y `notElem` ns -> return [Def y yty]
      (Pair a1 a2, Pair b1 b2) -> unifyArgs [a1, a2] [b1, b2]
      (EqType a1 a2, EqType b1 b2) -> unifyArgs [a1, a2] [b1, b2]
      (TCon s1 tms1, TCon s2 tms2)
        | s1 == s2 -> unifyArgs tms1 tms2
      (DCon s1 a1s, DCon s2 a2s)
        | s1 == s2 -> unifyArgs a1s a2s
      (Lam bnd1, Lam bnd2) -> do
        (x, b1, _, b2) <- Unbound.unbind2Plus bnd1 bnd2
        unify (x:ns) b1 b2
      (Pi tyA1 bnd1, Pi tyA2 bnd2) -> do
        (x, tyB1, _, tyB2) <- Unbound.unbind2Plus bnd1 bnd2
        ds1 <- unify ns tyA1 tyA2
        ds2 <- unify (x:ns) tyB1 tyB2
        return (ds1 ++ ds2)
      _ ->
        if amb txnf || amb tynf
          then return []
          else err $ "Cannot equate " ++ show txnf ++ " and " ++ show tynf
  where
    unifyArgs (t1 : a1s) (t2 : a2s) = do
      ds <- unify ns t1 t2
      ds' <- unifyArgs a1s a2s
      return $ ds ++ ds'
    unifyArgs [] [] = return []

-- | Is a term "ambiguous" when it comes to unification?
-- In general, elimination forms are ambiguous because there are multiple
-- solutions.
amb :: Term -> Bool
amb (App t1 t2) = True
amb (If _ _ _) = True
amb (LetPair _ _) = True
amb (Match _ _) = True
amb _ = False



