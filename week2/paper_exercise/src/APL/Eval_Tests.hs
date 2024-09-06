module APL.Eval_Tests (tests) where

import APL.AST (Term (..))
import APL.Eval (test)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "evaluating term0" $
        test term0 @?= "42"
    ]
