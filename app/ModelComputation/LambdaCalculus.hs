{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module ModelComputation.LambdaCalculus where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad (void)
import Data.Foldable (Foldable (foldl'))
import Data.List (intersect, nub, union)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (try), Parsec, between, single)
import Text.Megaparsec.Char (alphaNumChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Var Char
  | App Expr Expr
  | Abs Char Expr deriving (Eq)

instance Show Expr where
  show (Var x) = [x]
  show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"
  show (Abs var body) = "(λ" ++ [var] ++ "." ++ show body ++ ")"

-- t[x := r]
-- t, s and r are lambda variables
-- x and y are variables
-- Inside t, x replaces r
capture_avoid_sub :: Expr -> Char -> Expr -> Expr
capture_avoid_sub (Var t) x r
  -- x[x := r] -> r
  | t == x = r
  -- y[x := r] -> y, if x != y
  | otherwise = Var t
-- (t s)[x := r] -> (t[x := r])(s[x := r])
capture_avoid_sub (App t s) x r = App (capture_avoid_sub t x r) (capture_avoid_sub s x r)
capture_avoid_sub abstraction@(Abs v t) x r
  -- (λx.t)[x := r] -> λx.t
  | v == x = abstraction
  -- (λy.t)[x := r] -> λy.(t[x := r]), if y is not equal to x and y does not appear in the free
  -- variables of r
  | v /= x && v `notElem` free_variables r = Abs v (capture_avoid_sub t x r)
  | otherwise = capture_avoid_sub (alpha_reduce abstraction (free_variables r)) x r

-- (λx.t) s -> t[x := s]
bconversion :: Expr -> Expr
bconversion (App (Abs var body) x) = capture_avoid_sub body var x
bconversion (Abs var body) = Abs var (bconversion body)
bconversion (App left right) = App (bconversion left) (bconversion right)
bconversion a = a

alpha_reduce :: Expr -> [Char] -> Expr
alpha_reduce abstr@(Abs var body) free_vars = Abs suitable_char (capture_avoid_sub body var (Var suitable_char))
  where
    disallowed_chars = variables abstr `union` free_vars
    suitable_char = head [x | x <- ['a' .. 'z'] ++ ['A' .. 'Z'], x `notElem` disallowed_chars]
alpha_reduce _ _ = error "Cannot alpha reduce with not an abstraction"

free_variables :: Expr -> [Char]
free_variables (Abs var body) = [x | x <- free_variables body, x /= var]
free_variables (Var x) = [x]
free_variables (App lhs rhs) = free_variables lhs `union` free_variables rhs

bound_variables :: Expr -> [Char]
bound_variables (Abs var body) = var : bound_variables body
bound_variables (App lhs rhs) = bound_variables lhs ++ bound_variables rhs
bound_variables (Var _) = []

variables :: Expr -> [Char]
variables x = nub $ free_variables x ++ bound_variables x

steps_to_reduce :: Expr -> [Expr]
steps_to_reduce expression
  | expression == bconversion expression = [expression]
  | otherwise = expression:steps_to_reduce (bconversion expression)

debug :: c -> String -> c
debug = flip trace

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
parseVariable = Var <$> alphaNumChar

parseApplication :: Parser Expr
parseApplication = do
  first_term <- term
  more_terms <- some (space1 *> term)
  return $ foldl' App first_term more_terms

parseAbstraction :: Parser Expr
parseAbstraction = do
  lambdaSymbol
  (first : vars) <- reverse <$> some alphaNumChar
  dotSymbol
  body <- lambdaParser
  return $ foldl' (flip Abs) (Abs first body) vars

paranthetical :: Parser Expr
paranthetical = between (single '(') (single ')') lambdaParser

term :: Parser Expr
term = parseAbstraction <|> parseVariable <|> paranthetical

lambdaParser :: Parser Expr
lambdaParser = try parseApplication <|> term
