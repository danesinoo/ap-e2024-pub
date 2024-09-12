module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt n) = show n
printExp (CstBool b) = show b
printExp (Var x) = x
printExp (Add x y) = "(" ++ printExp x ++ " + " ++ printExp y ++ ")"
printExp (Sub x y) = "(" ++ printExp x ++ " - " ++ printExp y ++ ")"
printExp (Mul x y) = "(" ++ printExp x ++ " * " ++ printExp y ++ ")"
printExp (Div x y) = "(" ++ printExp x ++ " / " ++ printExp y ++ ")"
printExp (Pow x y) = "(" ++ printExp x ++ " ** " ++ printExp y ++ ")"
printExp (Eql x y) = "(" ++ printExp x ++ " == " ++ printExp y ++ ")"
printExp (If x y z) = "(if " ++ printExp x ++ " then " ++ printExp y ++ " else " ++ printExp z ++ ")"
printExp (Let x y z) = "(let " ++ x ++ " = " ++ printExp y ++ " in " ++ printExp z ++ ")"
printExp (Lambda name body) = "(\\" ++ name ++ " -> " ++ printExp body ++ ")"
printExp (Apply f (Apply f' x)) = printExp f ++ " (" ++ printExp f' ++ printExp x ++ ")"
printExp (Apply f x) = printExp f ++ " " ++ printExp x
printExp (TryCatch body handler) = "(try " ++ printExp body ++ " catch " ++ printExp handler ++ ")"
