module APL.Eval
  ( unitM,
    bindM,
    showM,
    Value (..),
    Environment,
    interp,
    test,
    Term (..),
  )
where

import APL.AST (Name)

data Term
  = Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term
  | Count

-- | Fail
-- | Amb Term Term
-- | Out Term
-- | At Position Term
data Value
  = Wrong
  | Num Int
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun _f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup1 x e
interp (Con i) _ = unitM (Num i)
interp (Add u v) e = bindM (interp u e) $ bindM (interp v e) . add
interp (Lam x v) e = unitM (Fun (\a -> interp v ((x, a) : e)))
interp (App t u) e = bindM (interp t e) $ bindM (interp u e) . apply
interp Count _e = bindM fetchM (unitM . Num)

-- interp Fail _ = zeroL
-- interp (Amb u v) e = interp u e `plusL` interp v e

-- interp (Out u) e = bindM (interp u e) outM
-- interp (At p t) e = resetM p (interp t e)

lookup1 :: Name -> Environment -> M Value
lookup1 _x [] = unitM Wrong
-- lookup1 x [] = errorM ("unbound variable: " ++ x)
lookup1 x ((y, b) : e) = if x == y then unitM b else lookup1 x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = bindM tickM (\() -> unitM (Num (i + j)))
-- add (Num i) (Num j) = unitM (Num (i + j))
-- add a b = errorM ("should be numbers: " ++ showval a ++ ", " ++ showval b)
add _a _b = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = bindM tickM (\() -> k a)
-- apply (Fun k) a = k a
-- apply f _a = errorM ("should be function: " ++ showval f)
apply _f _a = unitM Wrong

test :: Term -> String
test t = showM (interp t [])

{- The Identity Monad -}
type M a = a

unitM :: a -> M a
unitM a = a

bindM :: M a -> (a -> M b) -> M b
bindM a k = k a

showM :: M Value -> String
showM a = showval a

{- The Error messages Monad -}
{-

data M a = Suc a | Err String

unitM :: a -> M a
unitM = Suc

errorM :: String -> M a
errorM s = Err s

bindM :: M a -> (a -> M b) -> M b
bindM (Suc a) k = k a
bindM (Err s) _ = Err s

showM :: M Value -> String
showM (Suc a) = "Success: " ++ showval a
showM (Err s) = "Error: " ++ s

-}

{- The Error messages with position Monad -}

{-
type Position = Int

type P a = Position -> E a

data E a
  = Suc a
  | Err String

unitE :: a -> E a
unitE = Suc

errorE :: String -> E a
errorE s = Err s

bindE :: E a -> (a -> E b) -> E b
bindE (Suc a) k = k a
bindE (Err s) _ = Err s

showE :: E Value -> String
showE (Suc a) = "Success: " ++ showval a
showE (Err s) = "Error: " ++ s

apply :: Value -> Value -> M Value
apply (Fun k) a = k a

type M a = Position -> E a

unitM :: a -> Position -> E a
unitM a _p = unitE a

errorM :: String -> Position -> E a
errorM s p = errorE (showInt p "" ++ ": " ++ s)

bindM :: M a -> (a -> M b) -> M b
bindM m k p = bindE (m p) (`k` p)

showM :: M Value -> String
showM m = showE (m 0)

resetM :: Position -> P x -> P x
resetM q m p = m q
-}

{- The State Monad -}

{-
type State = Int

type M a = State -> (a, State)

unitM :: a -> M a
unitM a s0 = (a, s0)

bindM :: M a -> (a -> M b) -> M b
bindM m k s0 =
  let (a, s1) = m s0
      (b, s2) = k a s1
   in (b, s2)

showM :: M Value -> String
showM m =
  let (a, s1) = m 0
   in "Value: " ++ showval a ++ "; Count: " ++ showInt s1 ""

tickM :: M ()
tickM s = ((), s + 1)

fetchM :: M State
fetchM s = (s, s)
-}

{- The Output Monad -}

{-
type M a = (String, a)

unitM :: a -> M a
unitM a = ("", a)

bindM :: M a -> (a -> M b) -> M b
bindM m k =
  let (r, a) = m
      (s, b) = k a
   in (r ++ s, b)

showM :: M Value -> String
showM (s, a) = "Output: " ++ s ++ "; Value: " ++ showval a

outM :: Value -> M Value
outM a = (showval a ++ "; ", a)
-}

{- The Non-Deterministic Monad -}

{-
type M a = [a]

unitM :: a -> M a
unitM a = [a]

bindM :: M a -> (a -> M b) -> M b
bindM m k = [b | a <- m, b <- k a]

zeroL :: M a
zeroL = []

plusL :: M a -> M a -> M a
plusL l m = l ++ m

showM :: M Value -> String
showM m = show [showval a | a <- m]
-}

{- The Propagate backwords Monad -}
{-

type State = Int

type M a = State -> (a, State)

unitM :: a -> M a
unitM a s0 = (a, s0)

bindM :: M a -> (a -> M b) -> M b
bindM m k s2 =
  let (a, s0) = m s1
      (b, s1) = k a s2
   in (b, s0)

showM :: M Value -> String
showM m =
  let (a, s1) = m 0
   in "Value: " ++ showval a ++ "; Count: " ++ show s1

tickM :: M ()
tickM s = ((), s + 1)

fetchM :: M State
fetchM s = (s, s)
-}
