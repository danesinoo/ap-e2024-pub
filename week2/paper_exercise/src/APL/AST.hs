module APL.AST
  ( Name,
    Term (..),
  )
where

-- import Control.Monad (ap, liftM)

type Name = String

data Term
  = Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term
