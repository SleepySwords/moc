{-# LANGUAGE OverloadedStrings #-}

module Repl.Parser where

import Control.Applicative (Alternative ((<|>)), some, optional)
import Control.Monad (void)
import Data.Foldable
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Repl.Types (Expr (..), Statement (..))
import Text.Megaparsec (MonadParsec (eof, try), Parsec, between, many, satisfy, sepBy, single, sepEndBy)
import Text.Megaparsec.Char (alphaNumChar, eol, space1, symbolChar)
import Text.Megaparsec.Char.Lexer ()
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void Text a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "-[" " ]-")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

lambdaSymbol :: Parser ()
lambdaSymbol = void $ lexeme (single '\\') <|> lexeme (single 'λ')

dotSymbol :: Parser ()
dotSymbol = void $ lexeme (single '.')

whitespace :: Parser ()
whitespace = void $ many space1

parseArray :: Char -> Char -> Parser a -> Parser [a]
parseArray open close itemParser = between (single open) (single close) $ whitespace *> sepEndBy itemParser (lexeme "," <* whitespace) <* whitespace

parsePair :: Parser a -> Parser b -> Parser (a, b)
parsePair fstParser sndParser = between (single '(') (single ')') $ (,) <$> fstParser <* symbol "," <*> sndParser

parseFunction :: Parser (Expr, Expr)
parseFunction = lambdaSymbol *> ((,) <$> parseTerm <* whitespace <* symbol "->" <*> parseExpression)

literalChar :: Parser Char
literalChar = alphaNumChar <|> symbolChar

parseString :: Parser Expr
parseString = single '"' *> (Literal <$> many (satisfy (/= '"'))) <* single '"'

parseLiteral :: Parser Expr
parseLiteral = parseString <|> (Ident <$> some alphaNumChar)

parseCall :: Parser Expr
parseCall = do
  first_term <- parseTerm
  more_terms <- some (whitespace *> parseTerm)
  return $ foldl' Call first_term more_terms

sepBy2 :: Parser a -> Parser sep -> Parser [a]
sepBy2 p sep = (:) <$> p <*> some (sep *> p)

parseOr :: Parser Expr
parseOr = Set . Set.fromList <$> sepBy2 parseTerm (whitespace *> lexeme "|" <* whitespace) <* whitespace

parseTerm :: Parser Expr
parseTerm =
  parseLiteral
    <|> try (between (single '(') (single ')') parseExpression)
    <|> (Set . Set.fromList <$> parseArray '{' '}' parseExpression)
    <|> (Tuple <$> parseArray '(' ')' parseExpression)
    <|> (Function <$> parseFunction)

parseExpression :: Parser Expr
parseExpression = try parseCall <|> try parseOr <|> parseTerm

parseAssignment :: Parser (String, Expr)
parseAssignment = (,) <$> some alphaNumChar <* whitespace <* symbol ":=" <*> parseExpression

parseStatement :: Parser Statement
parseStatement = (uncurry Assignment <$> try parseAssignment <|> Expression <$> parseExpression) <* eof
