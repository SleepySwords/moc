{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Parser where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad (void)
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Void (Void)
import ModelComputation.LambdaCalculus.Types (Expr (Abs, App, Var), integerToChurchEncoding)
import Text.Megaparsec (MonadParsec (try), Parsec, between, single)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, punctuationChar, space1, symbolChar, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

type SymbolTable = Map String (Expr ())

type Parser a = Parsec Void Text a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "-[" " ]-")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lambdaSymbol :: Parser ()
lambdaSymbol = void $ lexeme (single '\\') <|> lexeme (single 'λ')

dotSymbol :: Parser ()
dotSymbol = void $ lexeme (single '.')

parseVariable :: SymbolTable -> Parser (Expr ())
parseVariable s = (Var () <$> lowerChar) <|> parseChurchEncoding <|> try (parseAbbreviation s)

parseChurchEncoding :: Parser (Expr ())
parseChurchEncoding = integerToChurchEncoding <$> parseDigit

parseDigit :: Parser Int
parseDigit = L.decimal

parseBindString :: Parser String
parseBindString = ((:) <$> upperChar <*> some alphaNumChar) <|> some symbolChar <|> some punctuationChar -- FIXME: This includes paranthesis

parseAbbreviation :: SymbolTable -> Parser (Expr ())
parseAbbreviation s = do
  str <- parseBindString
  maybe (fail "ok") return (Map.lookup str s)

parseApplication :: SymbolTable -> Parser (Expr ())
parseApplication s = do
  first_term <- term s
  more_terms <- some (space1 *> term s)
  return $ foldl' (App ()) first_term more_terms

parseAbstraction :: SymbolTable -> Parser (Expr ())
parseAbstraction s = do
  lambdaSymbol
  (first : vars) <- reverse <$> some lowerChar
  dotSymbol
  body <- lambdaParser s
  return $ foldl' (flip (Abs ())) (Abs () first body) vars

paranthetical :: SymbolTable -> Parser (Expr ())
paranthetical s = between (single '(') (single ')') (lambdaParser s)

term :: SymbolTable -> Parser (Expr ())
term s = parseAbstraction s <|> parseVariable s <|> paranthetical s

lambdaParser :: SymbolTable -> Parser (Expr ())
lambdaParser s = try (parseApplication s) <|> term s

parseBind :: SymbolTable -> Parser (String, Expr ())
parseBind s = (,) <$> parseBindString <* string ":=" <*> lambdaParser s

parseCommand :: SymbolTable -> Parser (Either (Expr ()) (String, Expr ()))
parseCommand s = (Left <$> lambdaParser s) <|> (Right <$> parseBind s)

-- Consider using a parser expr
defaultSymbolTable :: Map String (Expr ())
defaultSymbolTable =
  Map.fromList
    [ ("True", Abs () 'x' (Abs () 'y' (Var () 'x'))),
      ("False", Abs () 'x' (Abs () 'y' (Var () 'y'))),
      ("IfThen", Abs () 'b' (Abs () 'x' (Abs () 'y' (App () (App () (Var () 'b') (Var () 'x')) (Var () 'y'))))),
      ("*", Abs () 'm' (Abs () 'n' (Abs () 'f' (App () (Var () 'm') (App () (Var () 'n') (Var () 'f'))))))
    ]