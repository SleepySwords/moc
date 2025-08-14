{-# LANGUAGE OverloadedStrings #-}

module ModelComputation.FiniteStateAutomota.Parser where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (void)
import Data.Set (fromList)
import Data.Text (Text)
import Data.Void (Void)
import ModelComputation.FiniteStateAutomota.DFA (DeterministFiniteAutomota (..))
import Text.Megaparsec (Parsec, between, many, sepBy, single)
import Text.Megaparsec.Char (alphaNumChar, newline, space1)
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
whitespace = void $ optional $ many space1

parseSet :: Parser a -> Parser [a]
parseSet itemParser = between (single '{') (single '}') $ whitespace *> sepBy itemParser (lexeme "," <* whitespace) <* whitespace

parseSetExpression :: Parser a -> Parser (String, [a])
parseSetExpression = parseExpression . parseSet

parseExpression :: Parser a -> Parser (String, a)
parseExpression itemParser = (,) <$> many alphaNumChar <* space1 <* symbol "=" <*> itemParser

parseTheExpression :: Text -> Parser a -> Parser a
parseTheExpression st itemParser = symbol st >> symbol "=" >> itemParser <* newline

parsePair :: Parser a -> Parser b -> Parser (a, b)
parsePair fstParser sndParser = between (single '(') (single ')') $ (,) <$> fstParser <* symbol "," <*> sndParser

parseFunction :: Parser ((String, Char), String)
parseFunction = (,) <$> parsePair (many alphaNumChar) alphaNumChar <* space1 <* symbol "-->" <*> many alphaNumChar

parseAutomota :: Parser DeterministFiniteAutomota
parseAutomota = do
  parsedStates <- parseTheExpression "states" (parseSet (many alphaNumChar))
  parsedAlphabet <- parseTheExpression "alphabet" (parseSet alphaNumChar)
  parsedFunction <- parseTheExpression "transitionFunctions" (parseSet parseFunction)
  parsedInitialState <- parseTheExpression "initialState" (many alphaNumChar)
  parsedFinalState <- parseTheExpression "finalStates " (parseSet (many alphaNumChar))

  return
    DeterministFiniteAutomota
      { states = fromList parsedStates,
        alphabet = fromList parsedAlphabet,
        transitionFunctions = parsedFunction,
        initialState = parsedInitialState,
        finalStates = fromList parsedFinalState
      }
