{-# LANGUAGE OverloadedStrings #-}

module Repl.Parser where

import Control.Applicative (Alternative ((<|>)), some)
import Control.Monad (void)
import Data.Foldable
import Data.Text (Text)
import Data.Void (Void)
import Repl.Types (Expr (..), Statement (..))
import Text.Megaparsec (MonadParsec (try, eof), Parsec, between, many, satisfy, sepBy, single)
import Text.Megaparsec.Char (alphaNumChar, space1, symbolChar, eol)
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
parseArray open close itemParser = between (single open) (single close) $ whitespace *> sepBy itemParser (lexeme "," <* whitespace) <* whitespace

parsePair :: Parser a -> Parser b -> Parser (a, b)
parsePair fstParser sndParser = between (single '(') (single ')') $ (,) <$> fstParser <* symbol "," <*> sndParser

parseFunction :: Parser (Expr, Expr)
parseFunction = single '\\' *> ((,) <$> parseExpression <* space1 <* symbol "->" <*> parseExpression)

literalChar :: Parser Char
literalChar = alphaNumChar <|> symbolChar

parseString :: Parser Expr
parseString = single '"' *> (Literal <$> many (satisfy (/= '"'))) <* single '"'

parseLiteral :: Parser Expr
parseLiteral = parseString <|> (Literal <$> some alphaNumChar)

parseCall :: Parser Expr
parseCall = do
  first_term <- parseTerm
  more_terms <- some (space1 *> parseTerm)
  return $ foldl' Call first_term more_terms

parseTerm :: Parser Expr
parseTerm = parseLiteral <|> (Array <$> parseArray '{' '}' parseExpression) <|> (Tuple <$> parseArray '(' ')' parseExpression) <|> (Function <$> parseFunction)

parseExpression :: Parser Expr
parseExpression = try parseCall <|> parseTerm

parseAssignment :: Parser (String, Expr)
parseAssignment = (,) <$> some alphaNumChar <* whitespace <* symbol ":=" <*> parseExpression

parseStatement :: Parser Statement
parseStatement = (uncurry Assignment <$> try parseAssignment <|> Expression <$> parseExpression) <* eof
