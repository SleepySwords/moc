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
import Text.Megaparsec (MonadParsec (try), Parsec, between, single, many)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, punctuationChar, space1, symbolChar, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer ()

type SymbolTable = Map String (Expr ())

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

parseVariable :: SymbolTable -> Parser (Expr ())
parseVariable s = (Var () <$> lowerChar) <|> parseChurchEncoding <|> try (parseAbbreviation s)

parseChurchEncoding :: Parser (Expr ())
parseChurchEncoding = integerToChurchEncoding <$> parseDigit

parseDigit :: Parser Int
parseDigit = L.decimal

parseBindString :: Parser String
parseBindString = ((:) <$> upperChar <*> many alphaNumChar) <|> some symbolChar <|> some punctuationChar -- FIXME: This includes paranthesis

parseAbbreviation :: SymbolTable -> Parser (Expr ())
parseAbbreviation s = do
  str <- parseBindString
  maybe (fail ("Could not find the abrreviation " ++ str)) return (Map.lookup str s)

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
parseBind s = (,) <$> parseBindString <* many space1 <* symbol ":=" <*> lambdaParser s

parseCommand :: SymbolTable -> Parser (Either (Expr ()) (String, Expr ()))
parseCommand s = try (Right <$> parseBind s) <|> (Left <$> lambdaParser s)

-- Consider using a parser expr
defaultSymbolTable :: Map String (Expr ())
defaultSymbolTable =
  Map.fromList
    [ ("True", Abs () 'x' (Abs () 'y' (Var () 'x'))),
      ("False", Abs () 'x' (Abs () 'y' (Var () 'y'))),
      ("IfThen", Abs () 'b' (Abs () 'x' (Abs () 'y' (App () (App () (Var () 'b') (Var () 'x')) (Var () 'y'))))),
      ("*", Abs () 'm' (Abs () 'n' (Abs () 'f' (App () (Var () 'm') (App () (Var () 'n') (Var () 'f'))))))
    ]

newSymbolTable :: [(String, String)]
newSymbolTable =
    [ ("True", "\\xy.x"),
      ("False", "\\xy.y"),
      ("If", "\\bxy.b x y"),

      ("Const", "True"),

      ("For", "\\nf.n f"),
      ("Y", "\\f.(\\x.f (x x)) (\\x.f (x x))"),
      ("O", "(\\xy.y (\\z.x x y z)) (\\xy.y (\\z.x x y z))"),
      ("Z", "\\f.(\\x.f (\\v.x x v)) (\\x.f (\\v.x x v))"),

      ("Succ", "\\nfx.f (n f x)"),
      -- ("+", "\\pq.For p (Succ) q")
      ("Pred", "λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)"),
      ("+", "(\\pqfx.(p f (q f x)))"),
      ("*", "\\mnf.m (n f)"),
      ("IsZero", "\\x.For x (Const False) True"),

      ("Fact", "\\fx.If (IsZero x) 1 (* x (f (Pred x)))"),
      ("FactZ", "\\fx.(If (IsZero x) (\\a.1) (\\b.(* x (f (Pred x))))) a"),


      ("Pair", "\\xyf.f x y"),
      ("Fst", "\\p.p True"),
      ("Snd", "\\p.p False"),

      ("Test", "(YCom) (\\r.r)")
    ]
