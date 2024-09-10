module APL.Eval_Tests (tests) where

import APL.AST ()
import APL.Eval (Term (..), test)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

term2 :: Term
term2 = Add (Add (Add (Add (Con 3) (Con 1)) (Con 2)) Count) (Add (Con 3) (Con 4))

-- term3 :: Term
-- term3 = App (Lam "x" (Add (Var "x") (Var "x"))) (At 0 (Con 3))
-- term4 :: Term
-- term4 = Add (Out (Con 41)) (Out (Con 1))
-- term5 :: Term
-- term5 = App (Lam "x" (Add (Var "x") (Var "x"))) (Amb (Con 1) (Con 2))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "evaluating term0" $
        test term0 @?= "",
      testCase "evaluating term2" $
        test term2 @?= ""
        --       testCase "evaluating term4" $
        --         test term4 @?= ""
        --      testCase "evaluating term5" $
        --        test term5 @?= ""
    ]
