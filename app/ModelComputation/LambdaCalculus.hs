{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad (void)
import Data.Foldable (Foldable (foldl'))
import Data.List (nub, union)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (try), Parsec, between, single)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, space1, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

type SymbolTable = Map String (Expr ())

data ReduceInfo = ReduceInfo {substituted :: Maybe Char} deriving (Eq)

notSubstituted :: ReduceInfo
notSubstituted = ReduceInfo {substituted = Nothing}

replacedBind :: Char -> ReduceInfo
replacedBind x = ReduceInfo {substituted = Just x}

-- Use generics here, parser does not need this info..
data Expr a
  = Var {info :: a, name :: Char}
  | App {info :: a, function :: Expr a, input :: Expr a}
  | Abs {info :: a, bind :: Char, body :: Expr a}
  deriving (Eq)

instance Functor Expr where
  fmap f Var {info, name} = Var (f info) name
  fmap f App {info, function, input} = App (f info) (fmap f function) (fmap f input)
  fmap f Abs {info, bind, body} = Abs (f info) bind (fmap f body)

class Colour a where
  colour :: a -> String
  reset :: a -> String

instance Colour () where
  colour = const ""
  reset = const ""

instance Colour ReduceInfo where
  colour ReduceInfo {substituted = Just _} = "\x1b[32m"
  colour ReduceInfo {substituted = Nothing} = ""
  reset = const "\x1b[0m"

-- Print non verbose
instance (Colour a) => Show (Expr a) where
  show (Var {name = x, info}) = colour info ++ [x] ++ reset info
  -- show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"
  show (App {info, function = m, input = n}) = colour info ++ mPrint ++ " " ++ nPrint ++ reset info
    where
      addParantheses str = colour info ++ "(" ++ str ++ colour info ++ ")"
      mPrint = case m of
        Abs {} -> addParantheses (show m)
        _ -> show m
      nPrint = case n of
        Var {} -> show n
        _ -> addParantheses (show n)
  -- show (Abs var body) = "(λ" ++ [var] ++ "." ++ show body ++ ")"
  show (Abs {info, bind, body}) = colour info ++ "λ" ++ bind : vars ++ "." ++ show inner_body ++ reset info
    where
      flattenAbs :: Expr a -> (Expr a, [Char])
      flattenAbs (Abs {bind = v, body = b}) =
        let (ib, vs) = flattenAbs b
         in (ib, v : vs)
      flattenAbs e = (e, [])
      (inner_body, vars) = flattenAbs body

-- t[x := r]
-- t, s and r are lambda variables
-- x and y are variables
-- Inside t, x replaces r
substitution :: Expr ReduceInfo -> Char -> Expr ReduceInfo -> Expr ReduceInfo
substitution (Var {info, name = t}) x r
  -- x[x := r] -> r
  | t == x = r
  -- y[x := r] -> y, if x != y
  | otherwise = Var info t
-- (t s)[x := r] -> (t[x := r])(s[x := r])
substitution (App {info, function = t, input = s}) x r = App info (substitution t x r) (substitution s x r)
substitution abstraction@(Abs {info, bind = v, body = t}) x r
  -- (λx.t)[x := r] -> λx.t
  | v == x = abstraction
  -- (λy.t)[x := r] -> λy.(t[x := r]), if y is not equal to x and y does not appear in the free
  -- variables of r
  | v /= x && v `notElem` freeVariables r = Abs info v (substitution t x r)
  -- Must alpha reduce here to avoid name collisions
  | otherwise = substitution (aConversion abstraction (freeVariables r)) x r

-- (λx.t) s -> t[x := s]
bReduction :: Expr ReduceInfo -> Expr ReduceInfo
bReduction (App {function = (Abs {bind, body}), input = x}) = substitution body bind (replacedBind bind <$ x)
bReduction (Abs {info, bind, body}) = Abs info bind (bReduction body)
bReduction (App {info, function, input}) = App info (bReduction function) (bReduction input)
bReduction a = a

aConversion :: Expr ReduceInfo -> [Char] -> Expr ReduceInfo
aConversion abstr@(Abs {info, bind, body}) free_vars = Abs info suitable_char (substitution body bind (Var info suitable_char))
  where
    disallowed_chars = variables abstr `union` free_vars
    suitable_char = head [x | x <- ['a' .. 'z'], x `notElem` disallowed_chars]
aConversion _ _ = error "Cannot alpha reduce with not an abstraction"

freeVariables :: Expr ReduceInfo -> [Char]
freeVariables (Abs {bind, body}) = [x | x <- freeVariables body, x /= bind]
freeVariables (Var {name = x}) = [x]
freeVariables (App {function = lhs, input = rhs}) = freeVariables lhs `union` freeVariables rhs

boundVariables :: Expr ReduceInfo -> [Char]
boundVariables (Abs {bind, body}) = bind : boundVariables body
boundVariables (App {function, input}) = boundVariables function `union` boundVariables input
boundVariables (Var {}) = []

variables :: Expr ReduceInfo -> [Char]
variables x = nub $ freeVariables x ++ boundVariables x

steps_to_reduce :: Expr ReduceInfo -> [Expr ReduceInfo]
steps_to_reduce expression
  | expression == bReduction (notSubstituted <$ expression) = [expression]
  | otherwise = expression : steps_to_reduce (bReduction (notSubstituted <$ expression))

debug :: c -> String -> c
debug = flip trace

integerToChurchEncoding :: Int -> Expr ()
integerToChurchEncoding n = Abs () 'f' (Abs () 'x' inner)
  where
    inner = iterate (App () (Var () 'f')) (Var () 'x') !! n

churchEncodingToInteger :: Expr a -> Maybe Int
churchEncodingToInteger (Abs {bind = a, body = (Abs {bind = b, body = innerBody})}) = hasApplied innerBody
  where
    hasApplied (App {function, input})
      | Var {name = rs} <- input, rs == b = Just 1
      | Var {name = ls} <- function, ls == a = (+ 1) <$> hasApplied input
      | otherwise = Nothing
    hasApplied (Var {name = i})
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

parseVariable :: SymbolTable -> Parser (Expr ())
parseVariable s = (Var () <$> lowerChar) <|> parseChurchEncoding <|> parseAbbreviation s

parseChurchEncoding :: Parser (Expr ())
parseChurchEncoding = integerToChurchEncoding <$> parseDigit

parseDigit :: Parser Int
parseDigit = L.decimal

parseAbbreviationString :: Parser String
parseAbbreviationString = (:) <$> upperChar <*> some alphaNumChar

parseAbbreviation :: SymbolTable -> Parser (Expr ())
parseAbbreviation s = do
  str <- parseAbbreviationString
  return (s Map.! str)

-- if str == "True"
--   then
--     return $ Abs 'x' (Abs 'y' (Var 'x'))
--   else
--     -- Come back when finished
--     -- the custom errors in megaparsec tutorial
--     fail ("Undefined abbreviation: " ++ str)

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

-- Consider using a parser expr
defaultSymbolTable :: Map String (Expr ())
defaultSymbolTable =
  Map.fromList
    [ ("True", Abs () 'x' (Abs () 'y' (Var () 'x'))),
      ("False", Abs () 'x' (Abs () 'y' (Var () 'y'))),
      ("IfThen", Abs () 'b' (Abs () 'x' (Abs () 'y' (App () (App () (Var () 'b') (Var () 'x')) (Var () 'y')))))
    ]
