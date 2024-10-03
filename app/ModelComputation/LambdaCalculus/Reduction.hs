{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Reduction where

import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..))
import Control.Applicative
import Data.List (union, nub)

notSubstituted :: ReduceInfo
notSubstituted = ReduceInfo {substituted = Nothing, rainbow = 0}

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
bReduction :: Expr ReduceInfo -> Expr ReduceInfo
bReduction (App {function = (Abs {bind, body}), input = x}) = substitution body bind (replacedBind bind <$ x)
bReduction (Abs {info, bind, body}) = Abs info bind (bReduction body)
bReduction (App {info, function, input}) = App info (bReduction function) (bReduction input)
bReduction a = a

-- (λx.t) s -> t[x := s]
-- FIXME: Substitutes the outside first, it would be nicer if we substitute the inside first.
-- IE: breduce the input and the function in the top one.
bReductionFull :: Expr ReduceInfo -> Maybe (Expr ReduceInfo)
-- bReductionFull (App {function = (Abs {bind, body}), input = x}) = Just $ substitution body bind (replacedBind bind <$ x)
bReductionFull (App {info, function = function@(Abs {bind, body}), input = x}) = (((\a -> App info a x) <$> bReductionFull function) <|> (App info function <$> bReductionFull x)) <|> Just (substitution body bind (replacedBind bind <$ x))
bReductionFull (App {info, function, input}) = ((\x -> App info x input) <$> bReductionFull function) <|> (App info function <$> bReductionFull input)
bReductionFull (Abs {info, bind, body}) = Abs info bind <$> bReductionFull body
bReductionFull _ = Nothing

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

steps_to_reduce :: Expr ReduceInfo -> [Expr ReduceInfo]
steps_to_reduce expression
  | expression == bReduction (notSubstituted <$ expression) = [expression]
  | otherwise = expression : steps_to_reduce (bReduction (notSubstituted <$ expression))

steps_to_reduce_full :: Expr ReduceInfo -> [Expr ReduceInfo]
steps_to_reduce_full expression
  | Just exp <- result = expression : steps_to_reduce_full exp
  | otherwise = [expression]
  where
    result = bReductionFull (notSubstituted <$ expression)
