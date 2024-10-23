module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Eval (eval, runEval)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, listOf, oneof, shrink, sized)

genVar :: Gen VName
genVar = (:) <$> lowerCaseLetter <*> listOf (oneof [lowerCaseLetter, upperCaseLetter, digit])
  where
    lowerCaseLetter = elements ['a' .. 'z']
    upperCaseLetter = elements ['A' .. 'Z']
    digit = elements ['0' .. '9']

genExp :: Int -> Gen Exp
genExp size =
  if size <= 1
    then
      oneof
        [ CstInt <$> arbitrary,
          CstBool <$> arbitrary,
          Var <$> genVar
        ]
    else
      let half = (size - 1) `div` 2
          third = (size - 1) `div` 3
       in oneof
            [ CstInt <$> arbitrary,
              CstBool <$> arbitrary,
              Add <$> genExp half <*> genExp half,
              Sub <$> genExp half <*> genExp half,
              Mul <$> genExp half <*> genExp half,
              Div <$> genExp half <*> genExp half,
              Pow <$> genExp half <*> genExp half,
              Eql <$> genExp half <*> genExp half,
              If <$> genExp third <*> genExp third <*> genExp third,
              Var <$> genVar,
              Let <$> genVar <*> genExp half <*> genExp half,
              Lambda <$> genVar <*> genExp (size - 1),
              Apply <$> genExp half <*> genExp half,
              TryCatch <$> genExp half <*> genExp half
            ]

instance Arbitrary Exp where
  arbitrary = sized genExp

  shrink (CstInt a) = CstInt <$> shrink a
  shrink (CstBool a) = CstBool <$> shrink a
  shrink (Add e1 e2) = e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) = e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) = e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) = e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) = e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) = e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If e1 e2 e3) = e2 : e3 : [If e1' e2 e3 | e1' <- shrink e1] ++ [If e1 e2' e3 | e2' <- shrink e2] ++ [If e1 e2 e3' | e3' <- shrink e3]
  shrink (Var vname) = [Var vname' | vname' <- shrink vname, not (null vname')]
  shrink (Let vname e1 e2) = e1 : e2 : [Let vname e1' e2 | e1' <- shrink e1] ++ [Let vname e1 e2' | e2' <- shrink e2] ++ [Let vname' e1 e2 | vname' <- shrink vname, not (null vname')]
  shrink (Apply e1 e2) = e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) = e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink (Lambda vname e) = e : [Lambda vname e' | e' <- shrink e] ++ [Lambda vname' e | vname' <- shrink vname, not (null vname')]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))
