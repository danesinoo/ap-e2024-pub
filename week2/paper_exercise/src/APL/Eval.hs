module APL.Eval
  ( unitM,
    bindM,
    showM,
    Value (..),
    Environment,
    interp,
    test,
  )
where

import APL.AST (Name, Term (..))
import Numeric (showInt)

type M a = a

unitM :: a -> M a
unitM a = a

bindM :: M a -> (a -> M b) -> M b
bindM a k = k a

showM :: M Value -> String
showM a = showval a

data Value
  = Wrong
  | Num Int
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showval :: M Value -> String
showval Wrong = "<wrong>"
showval (Num i) = showInt i ""
showval (Fun _f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup1 x e
interp (Con i) _ = unitM (Num i)
interp (Add u v) e = bindM (interp v e) $ bindM (interp u e) add
interp (Lam x v) e = unitM (Fun (\a -> interp v ((x, a) : e)))
interp (App t u) e = bindM (interp u e) $ bindM (interp t e) apply

lookup1 :: Name -> Environment -> M Value
lookup1 _x [] = unitM Wrong
lookup1 x ((y, b) : e) = if x == y then unitM b else lookup1 x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i + j))
add _a _b = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply _f _a = unitM Wrong

test :: Term -> String
test t = showM (interp t [])
