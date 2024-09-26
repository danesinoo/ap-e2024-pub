module APL.Parser (parseAPL, lInteger) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

keywords :: [String]
keywords = ["if", "then", "else", "true", "false", "let", "in", "try", "catch", "print", "put", "get"]

-- Do not change this definition.
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lKeyword :: String -> Parser ()
lKeyword s = void $ lexeme $ chunk s <* notFollowedBy (satisfy isAlpha)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lBool :: Parser Bool
lBool =
  try $
    lexeme $
      choice
        [ True <$ lKeyword "true",
          False <$ lKeyword "false"
        ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords then fail "Unexpected keyword" else return v

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pExp0 :: Parser Exp
pExp0 = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pAtom
            chain $ Add x y,
          do
            lString "-"
            y <- pAtom
            chain $ Sub x y,
          do
            lString "*"
            y <- pAtom
            chain $ Mul x y,
          do
            lString "/"
            y <- pAtom
            chain $ Div x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
