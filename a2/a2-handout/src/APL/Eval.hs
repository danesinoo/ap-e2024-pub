module APL.Eval
  ( Val (..),
    State,
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

type Store = [(Val, Val)]

storeExtend :: Val -> Val -> Store -> Store
storeExtend key val store = (key, val) : filter (\(y, _) -> y /= key) store

type Error = String

type State = [String]

newtype EvalM a = EvalM (Env -> Store -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_ _ -> ([], Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env store ->
    case x env store of
      (s, Left err) -> (s, Left err)
      (s, Right x') ->
        let EvalM y = f x'
            (s', res) = y env store
         in (combineStates s s', res)

askEnv :: EvalM Env
askEnv = EvalM $ \env _ -> ([], Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env store -> m (f env) store

failure :: String -> EvalM a
failure s = EvalM $ \_ _ -> ([], Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env store ->
  case m1 env store of
    (s, Left _) ->
      case m2 env store of
        (s', Left err) -> (combineStates s s', Left err)
        (s', Right x) -> (combineStates s s', Right x)
    (s, Right x) -> (s, Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = m envEmpty []

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f =
  evalIntBinOp f'
  where
    f' x y = pure $ f x y

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_ _ -> ([s], Right ())

combineStates :: State -> State -> State
combineStates s1 s2 = s1 ++ s2

showVal :: Val -> String
showVal (ValInt x) = show x
showVal (ValBool x) = show x
showVal (ValFun {}) = "#<fun>"

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = EvalM $ \_ -> (\_ -> ([], Right ())) . storeExtend key val

evalKvGet :: Val -> EvalM Val
evalKvGet key = EvalM $ \_ store ->
  case lookup key store of
    Just val -> ([], Right val)
    Nothing -> ([], Left $ "Invalid key: " ++ show key)

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print vname e) =
  eval e >>= \val -> (\() -> val) <$> evalPrint (vname ++ ": " ++ showVal val)
eval (KvPut k v) = eval v >>= \val -> (\() -> val) <$> (eval k >>= \key -> evalKvPut key val)
-- instance Monad EvalM where
--   EvalM x >>= f = EvalM $ \env store ->
--     case x env store of
--       (s, Left err) -> (s, Left err)
--       (s, Right x') ->
--         let EvalM y = f x'
--             (s', res) = y env store
--          in (combineStates s s', res)
eval (KvGet k) =
  do
    key <- eval k
    evalKvGet key
