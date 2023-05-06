import TypeCheck

infer x = runTcMonad $ typeCheckTerm x Nothing
check x t = runTcMonad $ typeCheckTerm x t

x = Unbound.s2n "x"
y = Unbound.s2n "y"
z = Unbound.s2n "z"
t = Unbound.s2n "t"

term1 = (Ann
    (Var x)
    BoolType)

--id : [t : U] -> (x : t) -> t

id' = Lam (Unbound.bind x (Var x))

idsig = (Ann id
  (Pi U (Unbound.bind t (
      (Pi (Var t) (Unbound.bind y (Var t)))
      ))))

infer idsig

not = Lam (Unbound.bind x (If (Var x) (BoolLit False) (BoolLit True)))

notsig = (Ann not
    (Pi BoolType (Unbound.bind t BoolType)))

infer notsig
-- should be: (t:BoolType) -> BoolType

term = (App (Ann id (Pi U (Unbound.bind x U))) U) (Just U)


