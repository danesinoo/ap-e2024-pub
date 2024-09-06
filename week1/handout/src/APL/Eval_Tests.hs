module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "evaluating exp" $
        eval envEmpty (CstInt 3) @?= Right (ValInt 3),
      testCase "evaluating division by zero" $
        eval envEmpty (Div (CstInt 3) (CstInt 0)) @?= Left "Divide by zero",
      testCase "evaluating negative exponent" $
        eval envEmpty (Pow (CstInt 3) (CstInt (-1))) @?= Left "Negative exponent",
      testCase "evaluating let binding with add" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
      testCase "evaluating unknown variable" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y"))) @?= Left "Unknown variable: y"
    ]
