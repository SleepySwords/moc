{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Reduction where

import Control.Applicative
import Data.List (nub, union)
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..))

noSub :: ReduceInfo
noSub = ReduceInfo {substituted = Nothing, rainbow = 0}

replacedBind :: Char -> ReduceInfo
replacedBind x = ReduceInfo {substituted = Just x, rainbow = 0}

-- t[x := r]
-- t, s and r are lambda vars
-- x and y are vars
-- Inside t, x replaces r
substitution :: Expr ReduceInfo -> Char -> Expr ReduceInfo -> Expr ReduceInfo
substitution (Var {info, name}) x r
  -- x[x := r] -> r
  | name == x = r
  -- y[x := r] -> y, if x != y
  | otherwise = Var info name
-- (t s)[x := r] -> (t[x := r])(s[x := r])
substitution (App {info, function = t, input = s}) x r = App info (substitution t x r) (substitution s x r)
substitution abst@(Abs {info, bind = v, body = t}) x r
  -- (λx.t)[x := r] -> λx.t
  | v == x = abst
  -- (λy.t)[x := r] -> λy.(t[x := r]), if y is not equal to x and y does not appear in the free
  -- vars of r
  | v /= x && v `notElem` freeVars r = Abs info v (substitution t x r)
  -- Must alpha reduce here to avoid name collisions
  | otherwise = substitution (aConversion abst (freeVars r)) x r

-- (λx.t) s -> t[x := s]
bReduceGreedy :: Expr ReduceInfo -> Expr ReduceInfo
bReduceGreedy (App {function = (Abs {bind, body}), input = x}) = substitution body bind (replacedBind bind <$ x)
bReduceGreedy (Abs {info, bind, body}) = Abs info bind (bReduceGreedy body)
bReduceGreedy (App {info, function, input}) = App info (bReduceGreedy function) (bReduceGreedy input)
bReduceGreedy a = a

-- (λx.t) s -> t[x := s]
bReduceNonGreedy :: Expr ReduceInfo -> Maybe (Expr ReduceInfo)
bReduceNonGreedy (App {info, function = f@(Abs {bind, body}), input = x}) =
  (\a -> App info a x) <$> bReduceNonGreedy f
    <|> App info f <$> bReduceNonGreedy x
    <|> Just (substitution body bind (replacedBind bind <$ x))
bReduceNonGreedy (App {info, function = f, input = x}) =
  (\a -> App info a x) <$> bReduceNonGreedy f
    <|> App info f <$> bReduceNonGreedy x
bReduceNonGreedy (Abs {info, bind, body}) = Abs info bind <$> bReduceNonGreedy body
bReduceNonGreedy _ = Nothing

aConversion :: Expr ReduceInfo -> [Char] -> Expr ReduceInfo
aConversion abst@(Abs {info, bind, body}) free_vars = Abs info suitable_char (substitution body bind (Var info suitable_char))
  where
    disallowed_chars = vars abst `union` free_vars
    suitable_char = head [x | x <- ['a' .. 'z'], x `notElem` disallowed_chars]
aConversion _ _ = error "Cannot alpha reduce with not an abstraction"

freeVars :: Expr ReduceInfo -> [Char]
freeVars (Abs {bind, body}) = [x | x <- freeVars body, x /= bind]
freeVars (Var {name = x}) = [x]
freeVars (App {function = lhs, input = rhs}) = freeVars lhs `union` freeVars rhs

boundVars :: Expr ReduceInfo -> [Char]
boundVars (Abs {bind, body}) = bind : boundVars body
boundVars (App {function, input}) = boundVars function `union` boundVars input
boundVars (Var {}) = []

vars :: Expr ReduceInfo -> [Char]
vars x = nub $ freeVars x ++ boundVars x

lambdaReduceGreedy :: Expr ReduceInfo -> [Expr ReduceInfo]
lambdaReduceGreedy expression
  | expression == result = [expression]
  | otherwise = expression : lambdaReduceGreedy result
  where
    result = bReduceGreedy (noSub <$ expression)

lambdaReduceNonGreedy :: Expr ReduceInfo -> [Expr ReduceInfo]
lambdaReduceNonGreedy expression
  | Just exp <- result = expression : lambdaReduceNonGreedy exp
  | otherwise = [expression]
  where
    result = bReduceNonGreedy (noSub <$ expression)
