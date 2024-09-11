{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module ModelComputation.LambdaCalculus where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad (void)
import Data.Foldable (Foldable (foldl'))
import Data.List (nub, union)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (try), Parsec, between, single)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, space1, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Var Char
  | App Expr Expr
  | Abs Char Expr
  deriving (Eq)

-- Print non verbose
instance Show Expr where
  show (Var x) = [x]
  -- show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"
  show (App m n) = mPrint ++ " " ++ nPrint
    where mPrint = case m of
            Abs _ _ -> "(" ++ show m ++ ")"
            _ -> show m
          nPrint = case n of
            Var _ -> show n
            _ -> "(" ++ show n ++ ")"
  -- show (Abs var body) = "(λ" ++ [var] ++ "." ++ show body ++ ")"
  show (Abs var body) = "λ" ++ var : vars ++ "." ++ show inner_body
    where
      flattenAbs :: Expr -> (Expr, [Char])
      flattenAbs (Abs v b) =
        let (ib, vs) = flattenAbs b
         in (ib, v : vs)
      flattenAbs e = (e, [])
      (inner_body, vars) = flattenAbs body

-- t[x := r]
-- t, s and r are lambda variables
-- x and y are variables
-- Inside t, x replaces r
substitution :: Expr -> Char -> Expr -> Expr
substitution (Var t) x r
  -- x[x := r] -> r
  | t == x = r
  -- y[x := r] -> y, if x != y
  | otherwise = Var t
-- (t s)[x := r] -> (t[x := r])(s[x := r])
substitution (App t s) x r = App (substitution t x r) (substitution s x r)
substitution abstraction@(Abs v t) x r
  -- (λx.t)[x := r] -> λx.t
  | v == x = abstraction
  -- (λy.t)[x := r] -> λy.(t[x := r]), if y is not equal to x and y does not appear in the free
  -- variables of r
  | v /= x && v `notElem` freeVariables r = Abs v (substitution t x r)
  -- Must alpha reduce here to avoid name collisions
  | otherwise = substitution (aConversion abstraction (freeVariables r)) x r

-- (λx.t) s -> t[x := s]
bReduction :: Expr -> Expr
bReduction (App (Abs var body) x) = substitution body var x
bReduction (Abs var body) = Abs var (bReduction body)
bReduction (App left right) = App (bReduction left) (bReduction right)
bReduction a = a

aConversion :: Expr -> [Char] -> Expr
aConversion abstr@(Abs var body) free_vars = Abs suitable_char (substitution body var (Var suitable_char))
  where
    disallowed_chars = variables abstr `union` free_vars
    suitable_char = head [x | x <- ['a' .. 'z'] ++ ['A' .. 'Z'], x `notElem` disallowed_chars]
aConversion _ _ = error "Cannot alpha reduce with not an abstraction"

freeVariables :: Expr -> [Char]
freeVariables (Abs var body) = [x | x <- freeVariables body, x /= var]
freeVariables (Var x) = [x]
freeVariables (App lhs rhs) = freeVariables lhs `union` freeVariables rhs

boundVariables :: Expr -> [Char]
boundVariables (Abs var body) = var : boundVariables body
boundVariables (App lhs rhs) = boundVariables lhs `union` boundVariables rhs
boundVariables (Var _) = []

variables :: Expr -> [Char]
variables x = nub $ freeVariables x ++ boundVariables x

steps_to_reduce :: Expr -> [Expr]
steps_to_reduce expression
  | expression == bReduction expression = [expression]
  | otherwise = expression : steps_to_reduce (bReduction expression)

debug :: c -> String -> c
debug = flip trace

integerToChurchEncoding :: Int -> Expr
integerToChurchEncoding n = Abs 'f' (Abs 'x' inner)
  where
    inner = iterate (App (Var 'f')) (Var 'x') !! n

churchEncodingToInteger :: Expr -> Maybe Int
churchEncodingToInteger (Abs a (Abs b inner)) = hasApplied inner
  where
    hasApplied (App l r)
      | Var rs <- r, rs == b = Just 1
      | Var ls <- l, ls == a = (+ 1) <$> hasApplied r
      | otherwise = Nothing
    hasApplied (Var i)
      | i == b = Just 0
      | otherwise = Nothing
    hasApplied _ = Nothing
churchEncodingToInteger _ = Nothing

type Parser a = Parsec Void Text a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "-[" " ]-")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lambdaSymbol :: Parser ()
lambdaSymbol = void $ lexeme (single '\\') <|> lexeme (single 'λ')

dotSymbol :: Parser ()
dotSymbol = void $ lexeme (single '.')

parseVariable :: Parser Expr
parseVariable = (Var <$> lowerChar) <|> parseChurchEncoding <|> parseAbbreviation

parseChurchEncoding :: Parser Expr
parseChurchEncoding = integerToChurchEncoding <$> parseDigit

parseDigit :: Parser Int
parseDigit = L.decimal

parseAbbreviationString :: Parser String
parseAbbreviationString = (:) <$> upperChar <*> some alphaNumChar

parseAbbreviation :: Parser Expr
parseAbbreviation = do
  str <- parseAbbreviationString
  if str == "True"
    then
      return $ Abs 'x' (Abs 'y' (Var 'x'))
    else
      -- Come back when finished
      -- the custom errors in megaparsec tutorial
      fail ("Undefined abbreviation: " ++ str)

parseApplication :: Parser Expr
parseApplication = do
  first_term <- term
  more_terms <- some (space1 *> term)
  return $ foldl' App first_term more_terms

parseAbstraction :: Parser Expr
parseAbstraction = do
  lambdaSymbol
  (first : vars) <- reverse <$> some lowerChar
  dotSymbol
  body <- lambdaParser
  return $ foldl' (flip Abs) (Abs first body) vars

paranthetical :: Parser Expr
paranthetical = between (single '(') (single ')') lambdaParser

term :: Parser Expr
term = parseAbstraction <|> parseVariable <|> paranthetical

lambdaParser :: Parser Expr
lambdaParser = try parseApplication <|> term
