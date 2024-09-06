module APL.Eval
  ( Val (..),
    eval,
    envEmpty,
  )
where

import APL.AST
  ( Exp (..),
    VName,
  )

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend k v env = (k, v) : filter (\(y, _) -> y /= k) env

envLookup :: VName -> Env -> Maybe Val
envLookup _ [] = Nothing
envLookup x ((k, v) : env)
  | x == k = Just v
  | otherwise = envLookup x env

type Error = String

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt x) = Right $ ValInt x
eval _ (CstBool x) = Right $ ValBool x
eval env (Add e1 e2) = binaryOp env (+) e1 e2
eval env (Sub e1 e2) = binaryOp env (-) e1 e2
eval env (Mul e1 e2) = binaryOp env (*) e1 e2
eval env (Div _ (CstInt 0)) = Left "Divide by zero"
eval env (Div e1 e2) = binaryOp env div e1 e2
eval env (Pow e1 e2) = case eval env e2 of
  Left err -> Left err
  Right (ValInt y)
    | y < 0 -> Left "Negative exponent"
    | otherwise -> binaryOp env (^) e1 e2
  _ -> Left "Exponent must be an integer"
eval env (Eql e1 e2) = case (eval env e1, eval env e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
  (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
  _ -> Left "Operands must be of the same type"
eval env (If cond e1 e2) = case eval env cond of
  Right (ValBool True) -> eval env e1
  Right (ValBool False) -> eval env e2
  Left err -> Left err
  _ -> Left "Condition must be a boolean"
eval env (Let var e1 e2) = case eval env e1 of
  Left err -> Left err
  Right v -> eval (envExtend var v env) e2
eval env (Var x) = case envLookup x env of
  Just v -> Right v
  Nothing -> Left $ "Unknown variable: " ++ x

binaryOp :: Env -> (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
binaryOp env op e1 e2 = case (eval env e1, eval env e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (op x y)
  (Right _, Right _) -> Left "Operands must be integers"
