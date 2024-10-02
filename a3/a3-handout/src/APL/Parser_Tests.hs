module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          parserTest "x**y" $ Pow (Var "x") (Var "y"),
          parserTest "x ** y ** z" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "(x ** y) ** z" $ Pow (Pow (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")),
          parserTest "x ** (y z)" $ Pow (Var "x") (Apply (Var "y") (Var "z"))
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Function Application"
        [ parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTestFail "x if x then y else z",
          parserTest "f(x) y z" $ Apply (Apply (Apply (Var "f") (Var "x")) (Var "y")) (Var "z"),
          parserTest "x (y z) w" $ Apply (Apply (Var "x") (Apply (Var "y") (Var "z"))) (Var "w")
        ],
      testGroup
        "Print"
        [ parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTestFail "print \"missing closing quote",
          -- Non string first argument should fail
          parserTestFail "print 123",
          -- Print with a function application
          parserTest "print \"log\" (f x)" $ Print "log" (Apply (Var "f") (Var "x")),
          -- Nested print expressions
          parserTest "print \"nested\" (print \"inner\" 1)" $ Print "nested" (Print "inner" (CstInt 1))
        ],
      testGroup
        "Get"
        [ parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          -- Get cannot work with control structures like if
          parserTestFail "get if x then y else z",
          -- Get with a function application
          parserTest "get (f x)" $ KvGet (Apply (Var "f") (Var "x"))
        ],
      testGroup
        "Put"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          -- Nested put operations
          parserTest "put (put x y) z" $ KvPut (KvPut (Var "x") (Var "y")) (Var "z"),
          -- Put with strings (invalid arguments)
          parserTestFail "put \"key\" \"value\""
        ],
      testGroup
        "Lambda"
        [ parserTest "\\x -> x" $ Lambda "x" (Var "x"),
          -- Lambda with an `if` expression
          parserTest "\\x -> if x then y else z" $ Lambda "x" (If (Var "x") (Var "y") (Var "z")),
          -- Lambda with a more complex expression
          parserTest "\\x -> (x + 1) * 2" $ Lambda "x" (Mul (Add (Var "x") (CstInt 1)) (CstInt 2)),
          -- Lambda with two variables
          parserTestFail "\\x y -> x + y"
        ],
      testGroup
        "let-binding"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTestFail "let true = y in z",
          -- Let binding with an if expression
          parserTest "let x = if y then z else w in x + 1" $ Let "x" (If (Var "y") (Var "z") (Var "w")) (Add (Var "x") (CstInt 1))
        ],
      testGroup
        "try-catch"
        [ parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          -- Failure case: Try without catch should fail
          parserTestFail "try x",
          -- Failure case: Catch without try should fail
          parserTestFail "catch y",
          -- Try-catch with function applications
          parserTest "try f x catch g y" $ TryCatch (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "y"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ]
    ]
