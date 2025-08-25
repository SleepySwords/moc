{-# LANGUAGE OverloadedStrings #-}

module ModelComputation.TuringMachine.Parser where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (void)
import Data.Set (fromList)
import Data.Text (Text)
import Data.Void (Void)
import ModelComputation.TuringMachine.Turing (Shift (..), TuringMachine (..))
import Text.Megaparsec (Parsec, between, many, sepBy, single)
import Text.Megaparsec.Char (alphaNumChar, newline, space1, symbolChar)
import Text.Megaparsec.Char.Lexer ()
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (mapSnd, mapThd)

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

parseTriple :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
parseTriple fstParser sndParser thdParser = between (single '(') (single ')') $ (,,) <$> fstParser <* symbol "," <*> sndParser <* symbol "," <*> thdParser

parseFunction :: Parser ((String, Char), (String, Char, Char))
parseFunction = (,) <$> parsePair (many setItemChars) setItemChars <* space1 <* symbol "-->" <*> parseTriple (many setItemChars) setItemChars alphaNumChar

setItemChars :: Parser Char
setItemChars = alphaNumChar <|> symbolChar <|> single '.' <|> single '[' <|> single ']'

parseTuringMachine :: Parser TuringMachine
parseTuringMachine = do
  parsedStates <- parseTheExpression "states" (parseSet (many setItemChars))
  parsedAlphabet <- parseTheExpression "alphabet" (parseSet setItemChars)
  parseInputAlphabet <- parseTheExpression "inputAlphabet" (parseSet alphaNumChar)
  parsedFunction <- parseTheExpression "transitionFunctions" (parseSet parseFunction)
  parsedInitialState <- parseTheExpression "initialState" (many setItemChars)
  parsedFinalState <- parseTheExpression "finalStates " (parseSet (many setItemChars))
  parsedBlank <- parseTheExpression "blank " setItemChars 

  let interpetedState = map (mapSnd (mapThd (\a -> if a == 'R' then RightShift else LeftShift))) parsedFunction

  return
    TuringMachine
      { states = fromList parsedStates,
        tapeAlphabet = fromList parsedAlphabet,
        transitionFunctions = interpetedState,
        initialState = parsedInitialState,
        finalStates = fromList parsedFinalState,
        blank = parsedBlank,
        inputSymbols = fromList parseInputAlphabet
      }
