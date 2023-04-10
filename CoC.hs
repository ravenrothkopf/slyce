data Term
  = Lam (Term -> Term)
  | Pi (Term, Term -> Term)
  | Appl (Term, Term)
  | Ann (Term, Term)
  | FreeVar Int
  | Star
  | Box
  -- deriving (Eq, Ord), do we need to be deriving here??

unfurl :: Int -> (Term -> a) -> a
unfurl lvl f = f (FreeVar lvl)

unfurl2 :: Int -> (Term -> a, Term -> b) -> (a, b)
unfurl2 lvl (f, g) = (unfurl lvl f, unfurl lvl g)

printf :: Int -> Term -> String
printf lvl t = case t of
  (Lam f) -> "(λ" ++ plunge f ++ ")"
  (Pi (a, f)) -> "(Π" ++ printf lvl a ++ "." ++ plunge f ++ ")"
  (Appl (m, n)) -> "(" ++ printf lvl m ++ " " ++ printf lvl n ++ ")"
  (Ann (m, a)) -> "(" ++ printf lvl m ++ " : " ++ printf lvl a ++ ")"
  (FreeVar x) -> show x
  Star -> "*"
  Box -> "☐"
  where plunge f = printf (lvl + 1) (unfurl lvl f)

eval :: Term -> Term
eval a = case a of
  Lam f -> Lam (eval . f)
  Pi (a, f) -> Pi (eval a, eval . f)
  Appl (m, n) -> case (eval m, eval n) of
      (Lam f, n) -> eval (f n)
      (m, n) -> Appl (m, n)
  Ann (m, _a) -> eval m
  t@(FreeVar _) -> t
  Star -> Star
  Box -> Box

equate :: Int -> (Term, Term) -> Bool
equate lvl (r, s) = case (r, s) of
  (Lam f, Lam g) -> plunge (f, g)
  (Pi (a, f), Pi (b, g)) -> equate lvl (a, b) && plunge (f, g)
  (Appl (m, n), Appl (m', n')) -> equate lvl (m, m') && equate lvl (n, n')
  (Ann (m, a), Ann (m', b)) -> equate lvl (m, m') && equate lvl (a, b)
  (FreeVar x, FreeVar y) -> x == y
  (Star, Star) -> True
  (Box, Box) -> True
  (_, _) -> False
  where plunge (f, g) = equate (lvl + 1) (unfurl2 lvl (f, g))

panic :: Int -> Term -> String -> a' --theres probably a better way to print error messages in Haskell
panic lvl t str = error (show (str ++ ":" ++ printf lvl t))

inferTy :: Int -> [Term] -> Term -> Term
inferTy lvl ctx a = case a of
  Pi (a, f) ->
      let _s1 = inferSort lvl ctx a in
      let s2 = inferSort (lvl + 1) (eval a:ctx) (unfurl lvl f) in
      s2
  Appl (m, n) -> (
      case inferTy lvl ctx m of
      Pi (a, f) ->
          let _ = checkTy lvl ctx (n, a) in
          eval (f n)
      m_ty -> panic lvl m ("Want a Pi type, got " ++ printf lvl m_ty))
  Ann (m, a) ->
      let _s = inferSort lvl ctx a in
      checkTy lvl ctx (m, eval a)
  FreeVar x -> ctx!!(lvl - x - 1)
  Star -> Box
  Box -> panic lvl Box "Has no type"
  t -> panic lvl t "Not inferrable"

inferSort :: Int -> [Term] -> Term -> Term
inferSort lvl ctx a = case inferTy lvl ctx a of
  Star -> Star
  Box -> Box
  ty -> panic lvl a ("Want a sort, got" ++ printf lvl ty)

checkTy :: Int -> [Term] -> (Term, Term) -> Term
checkTy lvl ctx (n, a) = case (n, a) of
  (Lam f, Pi (a, g)) ->
      let _ = checkTy (lvl + 1) (a:ctx) (unfurl2 lvl (f, g)) in
      Pi (a, g)
  (Lam f, ty) -> panic lvl (Lam f) ("Want a Pi type, got" ++ printf lvl ty)
  (t, ty) ->
      let got_ty = inferTy lvl ctx t in
      if equate lvl (ty, got_ty) then ty
      else panic lvl t ("Want type " ++ printf lvl ty ++ ", got" ++ printf lvl got_ty)