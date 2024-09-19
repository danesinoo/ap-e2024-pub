module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, State, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> (State, Either Error Val)
eval' = runEval . eval

emptyState :: State
emptyState = []

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= (emptyState, Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= (emptyState, Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= (emptyState, Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= (emptyState, Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= (emptyState, Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= (emptyState, Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= (emptyState, Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= (emptyState, Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= (emptyState, Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= (emptyState, Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= (emptyState, Right (ValInt 2)),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= (emptyState, Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= (emptyState, Right (ValBool True)),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= (emptyState, Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= (emptyState, Right (ValBool True)),
      --
      testCase "Print 1" $
        eval' (Print "foo" $ CstInt 2)
          @?= (["foo: 2"], Right (ValInt 2)),
      --
      testCase "Print 2" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Print "bar" $ CstInt 3))
          @?= (["foo: 2", "bar: 3"], Right (ValInt 3)),
      --
      testCase "Print 3" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Var "bar"))
          @?= (["foo: 2"], Left "Unknown variable: bar"),
      --
      testCase "Print 4" $
        eval' (Print "foo" (Lambda "x" (Var "x")))
          @?= (["foo: #<fun>"], Right (ValFun [] "x" (Var "x"))),
      --
      testCase "KvPut KvGet 1" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
          @?= ([], Right (ValBool True)),
      --
      testCase "KvPut KvGet 2" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KvPut KvGet 3" $
        eval'
          ( Let
              "x"
              (KvPut (CstInt 0) (CstBool True))
              ( Let
                  "y"
                  (KvPut (CstInt 0) (CstBool False))
                  (KvGet (CstInt 0))
              )
          )
          @?= ([], Right (ValBool False))
    ]

tests :: TestTree
tests = testGroup "APL" [evalTests]
