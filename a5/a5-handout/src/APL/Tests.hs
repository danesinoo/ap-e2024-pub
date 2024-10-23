module APL.Tests
  ( properties,
  )
where

import APL.AST (Exp (..), VName, printExp, subExp)
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import APL.Eval (eval, runEval)
import APL.Parser (parseAPL)
import Control.Monad (guard)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    checkCoverage,
    choose,
    chooseInt,
    cover,
    elements,
    frequency,
    listOf,
    oneof,
    property,
    quickCheck,
    sample,
    sized,
    suchThat,
    vectorOf,
    withMaxSuccess,
  )

instance Arbitrary Exp where
  arbitrary = sized (`genExp` [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

keywords :: [String]
keywords = ["if", "then", "else", "true", "false", "let", "in", "try", "catch"]

generateValidVName :: Gen VName
generateValidVName = do
  len <- choose (2, 4)
  alpha <- elements ['a' .. 'z']
  alphaNums <- vectorOf (len - 1) (elements $ ['a' .. 'z'] ++ ['0' .. '9'])
  let vName = alpha : alphaNums
  if vName `elem` keywords
    then generateValidVName
    else pure vName

genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vars =
  frequency
    [ (20, CstInt <$> arbitrary),
      (20, CstBool <$> arbitrary),
      (9, Var <$> chooseVar vars),
      (10, Add <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, Sub <$> genExp halfSize vars <*> genExp halfSize vars),
      (5, Mul <$> genExp halfSize vars <*> genExp halfSize vars),
      (15, Div <$> genExp halfSize vars <*> genExp halfSize vars),
      (15, Pow <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, Eql <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars),
      ( 30,
        do
          newVar <- generateValidVName
          Let newVar <$> genExp halfSize (newVar : vars) <*> genExp halfSize (newVar : vars)
      ),
      ( 20,
        do
          newVar <- generateValidVName
          Lambda newVar <$> genExp (size - 1) (newVar : vars)
      ),
      (5, Apply <$> genExp halfSize vars <*> genExp halfSize vars),
      (5, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

chooseVar :: [VName] -> Gen VName
chooseVar [] = generateValidVName
chooseVar vars = do
  let filteredVars = filter (\v -> length v >= 2 && length v <= 4) vars
  if null filteredVars
    then elements vars
    else elements filteredVars

expCoverage :: Exp -> Property
expCoverage e =
  checkCoverage
    . cover 20 (any isDomainError (checkExp defaultVars e)) "domain error"
    . cover 20 (not $ any isDomainError (checkExp defaultVars e)) "no domain error"
    . cover 20 (any isTypeError (checkExp defaultVars e)) "type error"
    . cover 20 (not $ any isTypeError (checkExp defaultVars e)) "no type error"
    . cover 5 (any isVariableError (checkExp defaultVars e)) "variable error"
    . cover 70 (not $ any isVariableError (checkExp defaultVars e)) "no variable error"
    . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
    $ ()
  where
    defaultVars = ["x", "y", "z", "var1", "var2"]

parsePrinted :: Exp -> Bool
parsePrinted exp =
  case parseAPL "test" (printExp exp) of
    Left _ -> False
    Right parsedExp -> parsedExp == exp

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors exp = case runEval $ eval exp of
  Right _ -> True
  Left err -> err `elem` checkExp [] exp

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage),
    ("onlyCheckedErrors", property onlyCheckedErrors),
    ("parsePrinted", property parsePrinted)
  ]
