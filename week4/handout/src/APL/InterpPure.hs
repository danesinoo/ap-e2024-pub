module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' e s (Free (ReadOp k)) = runEval' e s $ k e
    runEval' e s (Free (StateGetOp k)) = runEval' e s $ k s
    runEval' e _ (Free (StatePutOp s' m)) = runEval' e s' m
    runEval' e state (Free (PrintOp msg e1)) =
      let (msgs, res) = runEval' e state e1
       in (msg : msgs, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
