module Equality
    ( equal
    --, inferPi
    , whnf
    ) where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad (guard)
import Ast
import Context

---------------------------------
-- | external
---------------------------------

equal :: Term -> Term -> TcMonad ()
equal a b =
    if (Unbound.aeq a b) then return () else do
        a' <- whnf a
        b' <- whnf b
        case (a',b') of
            (Lam bnd1, Lam bnd2) -> do
                (_, body1) <- Unbound.unbind bnd1
                (_, body2) <- Unbound.unbind bnd2
                equal body1 body2
            (App a1 a2, App b1 b2) -> do
                equal a1 b1
                equal a2 b2
            (Pi typA1 bnd1, Pi typA2 bnd2) -> do
                equal typA1 typA2
                (_, typB1) <- Unbound.unbind bnd1
                (_, typB2) <- Unbound.unbind bnd2
                equal typB1 typB2
            (UnitType, UnitType) -> return ()
            (UnitLit, UnitLit)   -> return ()
            (BoolType, BoolType) -> return ()
            (BoolLit True, BoolLit True) -> return ()
            (BoolLit False, BoolLit False) -> return ()
            (If a1 b1 c1, If a2 b2 c2) ->
                equal a1 a2 >> equal b1 b2 >> equal c1 c2
            (Sigma t1 b1, Sigma t2 b2) -> do
                equal t1 t2
                (_, typB1) <- Unbound.unbind b1
                (_, typB2) <- Unbound.unbind b2
                equal typB1 typB2
            (Pair a1 b1, Pair a2 b2) -> do
                equal a1 a2
                equal b1 b2
            (LetPair a1 bnd1, LetPair a2 bnd2) -> do
                equal a1 a2
                (_, b1) <- Unbound.unbind bnd1
                (_, b2) <- Unbound.unbind bnd2
                equal b1 b2
            (_, _) -> error "terms are not equal" -- TODO: use Env.err



inferPi :: Term -> TcMonad Type
inferPi = whnf      -- used for returning the subcomponents of Pi types
-- TODO: add error checking
{-
case nf of
    t@(Pi x y) -> t
    _ -> error 
-}

whnf :: Term -> TcMonad Term
whnf v@(Var x) = do
    d <- lookupDef x
    case d of
        Just def -> whnf def
        Nothing  -> return v
whnf (Ann t _) = whnf t
whnf (App a b) = do
    a' <- whnf a
    case a' of
        Lam bnd -> do                          -- beta reduction
            (x,body) <- Unbound.unbind bnd
            v <- whnf $ Unbound.subst x b body -- substitute b for x in body
            return v
        _       -> return $ App a' b           -- return App with normal form of a
whnf (If a b c) = do
    a' <- whnf a
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
whnf term      = return term                   -- all types and lambda

