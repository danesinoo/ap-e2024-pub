module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

divZero :: Exp
divZero = Div (CstInt 1) (CstInt 0)

goodPut :: EvalM ()
goodPut = evalKvPut (ValInt 0) (ValInt 1)

badPut :: EvalM ()
badPut = evalKvPut (ValInt 0) (ValBool False) >> failure "die"

get0 :: Exp
get0 = KvGet (CstInt 0)

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      testGroup
        "TryOp"
        [ testCase "Example 1" $
            runEval (Free $ TryCatchOp (failure "Oh no!") (pure "Success!"))
              @?= ([], Right "Success!"),
          --
          testCase "Example 2" $
            eval' (TryCatch (CstInt 5) divZero)
              @?= ([], Right (ValInt 5))
        ],
      --
      testGroup
        "Get Put"
        [ testCase "Example 1" $
            runEval
              ( Free $
                  KvPutOp
                    (ValInt 0)
                    (ValInt 1)
                    (Free $ KvGetOp (ValInt 0) $ \val -> pure val)
              )
              @?= ([], Right (ValInt 1)),
          --
          testCase "Get fail" $
            runEval (Free $ KvGetOp (ValInt 0) $ \val -> pure val)
              @?= ([], Left "Invalid key: ValInt 0")
        ],
      testGroup
        "Transaction"
        [ testCase "Example 1" $
            runEval (transaction goodPut >> eval get0)
              @?= ([], Right (ValInt 1)),
          --
          testCase "Example 2" $
            runEval (transaction badPut >> eval get0)
              @?= ([], Left "Invalid key: ValInt 0"),
          --
          testCase "Example 3" $
            runEval (transaction (evalPrint "weee" >> failure "oh shit"))
              @?= (["weee"], Right ()),
          --
          testCase "Example 4" $
            runEval (transaction (goodPut >> transaction badPut) >> eval get0)
              @?= ([], Right (ValInt 1)),
          --
          testCase "Example 5" $
            runEval (transaction (transaction badPut) >> eval get0)
              @?= ([], Left "Invalid key: ValInt 0")
        ]
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
      --
      testCase "Example2.3" $ do
        let console = "Left \"Division by zero\""
        (out, res) <-
          captureIO [] $
            runEvalIO $
              eval $
                TryCatch (CstInt 0 `Eql` CstBool True) divZero
        (out, show res) @?= ([], console),
      testCase "Example3.4" $ do
        (_, res) <-
          captureIO ["ValBool True"] $
            runEvalIO $
              Free $
                KvGetOp (ValBool True) $
                  \val -> pure val
        res @?= Right (ValBool True),
      testCase "Example3.3" $ do
        (_, res) <-
          captureIO ["lol"] $
            runEvalIO $
              Free $
                KvGetOp (ValBool True) $
                  \val -> pure val
        res @?= Left "Invalid value input: lol",
      testCase "Example3.4" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $
                KvGetOp (ValInt 0) $
                  \val -> pure val
        res @?= Right (ValInt 1),
      -- NOTE: This test will give a runtime error unless you replace the
      -- version of `eval` in `APL.Eval` with a complete version that supports
      -- `Print`-expressions. Uncomment at your own risk.
      testCase "print 2" $ do
        (out, res) <-
          captureIO [] $
            evalIO' $
              Print "This is also 1" $
                Print "This is 1" $
                  CstInt 1
        (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
      --
      testCase "Example transaction good" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              transaction (goodPut >> transaction badPut) >> eval get0
        (out, res) @?= ([], Right $ ValInt 1),
      --
      testCase "Example transaction bad" $ do
        (_, res) <-
          captureIO ["lol"] $
            runEvalIO $
              transaction (transaction badPut) >> eval get0
        res @?= Left "Invalid value input: lol"
    ]
