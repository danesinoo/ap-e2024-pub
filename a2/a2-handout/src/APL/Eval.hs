module APL.Eval
  ( Val (..),
    State,
    eval,
    runEval,
    Error,
    Console,
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

mapExtend :: key -> val -> [(key, val)] -> [(key, val)]
mapExtend v val env = (v, val) : env

mapLookup :: (Eq key) => key -> [(key, val)] -> Maybe val
mapLookup = lookup

type Store = [(Val, Val)]

type Error = String

type Console = [String]

type State = (Console, Store)

newtype EvalM a = EvalM (Env -> Store -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_ store -> (([], store), Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env store ->
    case x env store of
      (s, Left err) -> (s, Left err)
      ((console, store'), Right x') ->
        let EvalM y = f x'
            (state', res) = y env store'
         in (combineStates (console, store') state', res)

askEnv :: EvalM Env
askEnv = EvalM $ \env store -> (([], store), Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env store -> m (f env) store

failure :: String -> EvalM a
failure s = EvalM $ \_ store -> (([], store), Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env store ->
  case m1 env store of
    (s, Left _) ->
      case m2 env store of
        (s', Left err) -> (combineStates s s', Left err)
        (s', Right x) -> (combineStates s s', Right x)
    (s, Right x) -> (s, Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  let ((console, _), val) = m envEmpty []
   in (console, val)

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
evalPrint s = EvalM $ \_ store -> (([s], store), Right ())

combineStates :: State -> State -> State
combineStates (console1, store1) (console2, store2) =
  (console1 ++ console2, foldr expand store1 store2)
  where
    expand (key, val) = mapExtend key val

showVal :: Val -> String
showVal (ValInt x) = show x
showVal (ValBool x) = show x
showVal (ValFun {}) = "#<fun>"

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = EvalM $ \_ store -> (([], mapExtend key val store), Right ())

evalKvGet :: Val -> EvalM Val
evalKvGet key = EvalM $ \_ store ->
  case lookup key store of
    Just val -> (([], store), Right val)
    Nothing -> (([], store), Left $ "Invalid key: " ++ show key)

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case mapLookup v env of
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
  localEnv (mapExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ mapExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print vname e) =
  eval e >>= \val -> (\() -> val) <$> evalPrint (vname ++ ": " ++ showVal val)
eval (KvPut k v) = eval v >>= \val -> 
    (\() -> val) <$> (eval k >>= \key -> evalKvPut key val)
eval (KvGet k) = eval k >>= \key -> evalKvGet key
