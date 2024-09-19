module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

-- Monad implementation
instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    do
      x' <- x env
      let EvalM y = f x'
      y env

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  (<*>) = ap

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

runEval :: EvalM a -> Either Error a
runEval (EvalM m) = m envEmpty

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

-- localEnv :: (Env -> Env) -> EvalM a
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM x) = EvalM $ \env -> x (f env)

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x
eval (Add e1 e2) = binaryOp (+) e1 e2
eval (Sub e1 e2) = binaryOp (-) e1 e2
eval (Mul e1 e2) = binaryOp (*) e1 e2
eval (Div _ (CstInt 0)) = failure "Divide by zero"
eval (Div e1 e2) = binaryOp div e1 e2
eval (Pow e1 e2) = do
  y <- eval e2
  case y of
    ValInt y'
      | y' > 0 -> binaryOp (^) e1 e2
      | otherwise -> failure "Negative exponent"
    _ -> failure "Non-integer operand"
eval (Eql e1 e2) = do
  x <- eval e1
  y <- eval e2
  pure $ ValBool $ x == y
eval (If cond e1 e2) = do
  flag <- eval cond
  case flag of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Condition mut be a boolean"
eval (Let var e1 e2) = do
  x <- eval e1
  localEnv (envExtend var x) $ eval e2
eval (Var x) = do
  env <- askEnv
  case envLookup x env of
    Just v -> pure v
    Nothing -> failure $ "Unknown variable: " ++ x
eval (Lambda s e) = do
  env <- askEnv
  pure $ ValFun env s e
eval (Apply e1 e2) = do
  f <- eval e1
  x <- eval e2
  case (f, x) of
    (ValFun env' s e, x') -> localEnv (\_ -> envExtend s x' env') $ eval e
    _ -> failure $ "should be function: " ++ show f
eval (TryCatch e1 e2) = eval e1 `catch` eval e2

binaryOp :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
binaryOp op e1 e2 = do
  x1 <- eval e1
  x2 <- eval e2
  case (x1, x2) of
    (ValInt x1', ValInt x2') -> pure $ ValInt $ op x1' x2'
    _ -> failure "Non-integer operand"

catch :: EvalM a -> EvalM a -> EvalM a
catch x y = case runEval x of
  Right _ -> x
  Left _ -> y
