module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

printAdd :: Exp
printAdd = Add (CstInt 2) (CstInt 5)

printIf :: Exp
printIf = If (CstBool True) (Sub (CstInt 5) (CstInt 2)) (Mul (CstInt 0) (CstInt 1))

printEq :: Exp
printEq = Eql (Let "x" (CstInt 1) (Var "x")) (CstInt 5)

printLambda :: Exp
printLambda = Lambda "x" (Pow (Var "x") (CstInt 1))

printApply :: Exp
printApply = Apply (Apply (Div (CstInt 2) (CstInt 1)) (CstBool True)) (CstInt 3)

printTryCatch :: Exp
printTryCatch = TryCatch (CstInt 2) (CstInt 3)

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "Add" $
        printExp printAdd
          @?= "(2 + 5)",
      --
      testCase "If" $
        printExp printIf
          @?= "(if True then (5 - 2) else (0 * 1))",
      --
      testCase
        "Eq"
        $ printExp printEq
          @?= "((let x = 1 in x) == 5)",
      --
      testCase "Lambda" $
        printExp printLambda
          @?= "(\\x -> (x ** 1))",
      --
      testCase "Apply" $
        printExp printApply
          @?= "(2 / 1) True 3",
      --
      testCase
        "TryCatch"
        $ printExp printTryCatch
          @?= "(try 2 catch 3)"
    ]
