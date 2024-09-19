module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Sub (left error)" $
        eval envEmpty (Sub (Let "y" (CstInt 8) (Var "x")) (CstInt 5))
          @?= Left "Unknown variable: x",
      --
      testCase "Mul" $
        eval envEmpty (Mul (CstInt 2) (CstInt 5))
          @?= Right (ValInt 10),
      --
      testCase "Mul (err second exp)" $
        eval envEmpty (Mul (CstInt 2) (Let "y" (Div (CstInt 5) (CstInt 1)) (Var "x")))
          @?= Left "Unknown variable: x",
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (first arg error)" $
        eval envEmpty (Eql (Let "x" (CstInt 2) (Var "y")) (CstInt 3))
          @?= Left "Unknown variable: y",
      --
      testCase "Eql (second arg error)" $
        eval envEmpty (Eql (CstInt 3) (Div (CstInt 1) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "Eql (false) bool" $
        eval envEmpty (Eql (CstBool True) (CstBool False))
          @?= Right (ValBool False),
      --
      testCase "Eql (true) bool" $
        eval envEmpty (Eql (CstBool False) (CstBool False))
          @?= Right (ValBool True),
      --
      testCase "Eql invalid operands to equality" $
        eval envEmpty (Eql (CstBool False) (CstInt 1))
          @?= Left "Invalid operands to equality",
      --
      testCase "If (cond err)" $
        eval envEmpty (If (Let "y" (Div (CstInt 7) (CstInt 0)) (Var "x")) (CstInt 2) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "If (cond true)" $
        eval envEmpty (If (CstBool True) (CstInt 2) (CstInt 0))
          @?= Right (ValInt 2),
      --
      testCase "If (cond false)" $
        eval envEmpty (If (CstBool False) (CstInt 2) (Div (CstInt 0) (CstInt 7)))
          @?= Right (ValInt 0),
      --
      testCase "If non-boolean condition" $
        eval envEmpty (If (CstInt 2) (CstInt 2) (CstInt 7))
          @?= Left "Non-boolean conditional.",
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Apply (with function)" $
        eval envEmpty (Let "a" (Lambda "x" (Add (Var "x") (CstInt 3))) (Apply (Var "a") (CstInt 2)))
          @?= Right (ValInt 5),
      --
      testCase "Apply non-function applied" $
        eval envEmpty (Let "a" (CstInt 3) (Apply (Var "a") (CstInt 2)))
          @?= Left "Non-function applied",
      --
      testCase "TryCatch working" $
        eval envEmpty (TryCatch (Div (CstInt 0) (CstInt 7)) (CstInt 1))
          @?= Right (ValInt 0),
      --
      testCase "TryCatch except" $
        eval envEmpty (TryCatch (Div (CstInt 7) (CstInt 0)) (CstInt 0))
          @?= Right (ValInt 0),
      --
      testCase "Assignment lambda" $
        eval envEmpty (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right
            (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "Assignment apply" $
        eval envEmpty (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= Right (ValInt 5),
      --
      testCase "Assignment try-catch working" $
        eval envEmpty (TryCatch (CstInt 0) (CstInt 1))
          @?= Right (ValInt 0),
      --
      testCase "Assignment try-catch missing" $
        eval envEmpty (TryCatch (Var "missing") (CstInt 1))
          @?= Right (ValInt 1),
      --
      testCase "Apply fn fails before argument" $
        eval envEmpty (Apply (Div (CstInt 7) (CstInt 0)) (Var "y"))
          @?= Left "Division by zero",
      --
      testCase "Apply argument fails before body" $
        eval envEmpty (Apply (Lambda "x" (Var "y")) (Div (CstInt 7) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Apply body fails last" $
        eval envEmpty (Apply (Lambda "x" (Var "y")) (CstInt 0))
          @?= Left
            "Unknown variable: y"
     --  testCase "Loop" $
     --    eval envEmpty (Let "x" (Lambda "y" (Apply (Var "y") (Var "y"))) (Apply (Var "x") (Var "x")))
     --      @?= Right (ValInt 8)
    ]
