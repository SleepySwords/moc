{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ModelComputation.PrimativeRecursion.Parser where

import Control.Applicative (optional)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<|>), MonadParsec (try), Parsec, many, sepBy, single)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import Text.Megaparsec.Char.Lexer ()
import qualified Text.Megaparsec.Char.Lexer as L
import ModelComputation.PrimativeRecursion.Types (Recursive (Recursive, BaseCase))
import Utils (assocLeft)

type Parser a = Parsec Void Text a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "-[" " ]-")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

whitespace :: Parser ()
whitespace = void $ optional $ many space1

isRecursive :: Parser Recursive
isRecursive = do
  bound <- alphaNumChar
  if bound == '0'
    then
      return BaseCase
    else do
      _ <- symbol "+"
      _ <- symbol "1"
      return (Recursive bound)

sepEndWith :: Parser a -> Parser b -> Parser c -> Parser ([a], b)
sepEndWith a b sep =
  do
    x <- try (a <* sep)
    do
      (xs, f) <- sepEndWith a b sep
      return (x : xs, f)
    <|> ([],) <$> b

functionF :: Parser (String, ([Char], Recursive))
functionF = (,) <$> many letterChar <* single '(' <*> sepEndWith letterChar isRecursive (lexeme "," <* whitespace) <*  single ')'


function :: Parser (String, [Char], Recursive)
function = assocLeft <$> functionF
