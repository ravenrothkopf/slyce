module PrettyPrint where

import qualified Unbound.Generics.LocallyNameless as Unbound
import Ast

ppName :: TermName -> String
ppName = Unbound.name2String

ppTerm :: Term -> String
ppTerm (Var x) = ppName x
ppTerm (Lam bnd) =
    let (x,body) = Unbound.runFreshM $ Unbound.unbind bnd
     in "\\" ++ ppName x ++ "." ++ ppTerm body
ppTerm (Pi typA bnd) =
    let (x,typB) = Unbound.runFreshM $ Unbound.unbind bnd
        x' = ppName x
        var = if x' == "_" then "" else x' ++ " : "
     in "(" ++ var ++ ppTerm typA ++ ") -> " ++ ppTerm typB ++ ""
ppTerm (App a b) = "(" ++ ppTerm a ++ " " ++ ppTerm b ++ ")"
ppTerm (Ann term typ) = "(" ++ ppTerm term ++ " : " ++ ppTerm typ ++ ")"
ppTerm U = "U"
ppTerm UnitType = "Unit"
ppTerm UnitLit = "()"
ppTerm BoolType = "Bool"
ppTerm (BoolLit True) = "True"
ppTerm (BoolLit False) = "False"
ppTerm (If a b c) = "if " ++ ppTerm a ++ " then " ++ ppTerm b ++ " else " ++ ppTerm c ++ ""
ppTerm (Sigma typA bnd) =
    let (x,typB) = Unbound.runFreshM $ Unbound.unbind bnd
        x' = ppName x
        var = if x' == "_" then "" else x' ++ " : "
     in "(" ++ var ++ ppTerm typA ++ " * " ++ ppTerm typB ++ ")"
ppTerm (Pair a b) = "(" ++ ppTerm a ++ ", " ++ ppTerm b ++ ")"
ppTerm (LetPair rhs bnd) =
    let ((x,y), body) = Unbound.runFreshM $ Unbound.unbind bnd
     in "let " ++ "(" ++ ppName x ++ ", " ++ ppName y ++ ") = " ++ ppTerm rhs ++ " in " ++ ppTerm body
ppTerm (Let rhs bnd) =
    let (x, body) = Unbound.runFreshM $ Unbound.unbind bnd
     in "let " ++ ppName x ++ " = " ++ ppTerm rhs ++ " in " ++ ppTerm body
ppTerm (Pos _ term) = ppTerm term
ppTerm Refl = "Refl"
ppTerm (Contra a) = "contra " ++ ppTerm a
ppTerm (EqType a b) = ppTerm a ++ " = " ++ ppTerm b
ppTerm (Subst a y) = "subst " ++ ppTerm a ++ " by " ++ ppTerm y
ppTerm (Con n args) = n ++ concatMap ((' ':). ppTerm) args
ppTerm (Match scrut cases) = "match " ++ ppTerm scrut ++ " with " ++
    concatMap (\c -> "\n  | " ++ ppCase c) cases

ppCase :: Case -> String
ppCase (Case bnd) =
    let (pat, body) = Unbound.runFreshM $ Unbound.unbind bnd
     in ppPattern pat ++ " -> " ++ ppTerm body

ppPattern :: Pattern -> String
ppPattern (PatVar x) = ppName x
ppPattern (PatCon dc pats) = "(" ++ dc ++ concatMap ((' ':).ppPattern) pats ++ ")"

ppDecl :: Decl -> String
ppDecl (Def name term) = ppName name ++ " = " ++ ppTerm term ++ "."
ppDecl (TypeSig s) = ppSig s ++ "."
ppDecl (Data tcname ttele constructors) =
    let ppTelescope (Telescope t) =
            concatMap (\d -> " (" ++ ppDecl d ++ ")") t
        ppConstructor (ConstructorDef _ dcname (Telescope [])) = dcname
        ppConstructor (ConstructorDef _ dcname dtele) =
            dcname ++ " of " ++ ppTelescope dtele
     in "data " ++ tcname ++ ppTelescope ttele ++ " where " ++
        concatMap (\c -> "\n  " ++ ppConstructor c ++ ",") constructors ++ "."

ppSig :: Sig -> String
ppSig s = ppName (sigName s) ++ " : " ++ ppTerm (sigType s)

ppModule :: Module -> String
ppModule = undefined
