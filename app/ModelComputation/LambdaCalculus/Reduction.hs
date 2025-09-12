{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Reduction where

import Control.Applicative
import Control.Monad.State (State)
import qualified Data.Bifunctor as Bifunctor
import Data.List (nub, union)
import Data.Map (Map)
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..))

type SubstiutionState = State (Map (Expr ReduceInfo) (Expr ReduceInfo))

newtype DebugInfo = DebugInfo
  { subsitutions :: [(Char, Expr ReduceInfo)]
  }

type DebugExpr = (Expr ReduceInfo, DebugInfo)

noSub :: ReduceInfo
noSub = ReduceInfo {substituted = Nothing}

replacedBind :: Expr ReduceInfo -> Char -> Expr ReduceInfo
replacedBind (Var _ name) x = Var (ReduceInfo {substituted = Just x}) name
replacedBind (App _ fun input) x = App (ReduceInfo {substituted = Just x}) fun input
replacedBind (Abs _ bind body) x = Abs (ReduceInfo {substituted = Just x}) bind body

-- t[x := r]
-- t, s and r are lambda vars
-- x and y are vars
-- Inside t, x replaces r
substitution :: Expr ReduceInfo -> Char -> Expr ReduceInfo -> Expr ReduceInfo
substitution (Var {info, name}) x r
  -- x[x := r] -> r
  | name == x = replacedBind r x
  -- y[x := r] -> y, if x != y
  | otherwise = Var info name
-- (t s)[x := r] -> (t[x := r])(s[x := r])
substitution (App {info, function = t, input = s}) x r = App info (substitution t x r) (substitution s x r)
substitution abst@(Abs {info, bind = v, body = t}) x r
  -- (λx.t)[x := r] -> λx.t
  | v == x = abst
  -- (λy.t)[x := r] -> λy.(t[x := r]), if y is not equal to x and y does not appear in the free
  -- vars of r
  | v /= x && not (isFreeVar r v) = Abs info v (substitution t x r)
  -- Must alpha reduce here to avoid name collisions
  | otherwise = substitution (aConversion abst r) x r

-- (λx.t) s -> t[x := s]
bReduceNormal :: Expr ReduceInfo -> DebugExpr
bReduceNormal (App {function = (Abs {bind, body}), input = x}) = (substitution body bind x, DebugInfo [(bind, x)])
bReduceNormal (App {info, function, input}) = (App info rFun rInp, DebugInfo (dbgFun ++ dbgInp))
  where
    (rFun, DebugInfo dbgFun) = bReduceNormal function
    (rInp, DebugInfo dbgInp) = bReduceNormal input
bReduceNormal (Abs {info, bind, body}) = (Abs info bind rBody, DebugInfo dbgBody)
  where
    (rBody, DebugInfo dbgBody) = bReduceNormal body
bReduceNormal a = (a, DebugInfo [])

-- (λx.t) s -> t[x := s]
bReduceCBV :: Expr ReduceInfo -> Maybe DebugExpr
bReduceCBV (App {info, function = f@(Abs {bind, body}), input = x}) =
  Bifunctor.first (App info f) <$> bReduceCBV x
    <|> (\(a, d) -> (App info a x, d)) <$> bReduceCBV f
    <|> Just (substitution body bind x, DebugInfo [(bind, x)])
bReduceCBV (App {info, function = f, input = x}) =
  Bifunctor.first (App info f) <$> bReduceCBV x
    <|> (\(a, d) -> (App info a x, d)) <$> bReduceCBV f
bReduceCBV _ = Nothing

aConversion :: Expr ReduceInfo -> Expr ReduceInfo -> Expr ReduceInfo
aConversion abst@(Abs {info, bind, body}) toSub = Abs info suitable_char (substitution body bind (Var info suitable_char))
  where
    suitable_char = head [x | x <- ['a' .. 'z'], not $ isVar abst x, not $ isFreeVar toSub x]
aConversion _ _ = error "Cannot alpha reduce with not an abstraction"

freeVars :: Expr a -> String
freeVars (Abs {bind, body}) = [x | x <- freeVars body, x /= bind]
freeVars (Var {name = x}) = [x]
freeVars (App {function = lhs, input = rhs}) = freeVars lhs `union` freeVars rhs

isFreeVar :: Expr a -> Char -> Bool
isFreeVar (Abs {bind, body}) a = bind /= a && isFreeVar body a
isFreeVar (Var {name = x}) a = x == a
isFreeVar (App {function = lhs, input = rhs}) a = isFreeVar lhs a || isFreeVar rhs a

boundVars :: Expr a -> String
boundVars (Abs {bind, body}) = bind : boundVars body
boundVars (App {function, input}) = boundVars function `union` boundVars input
boundVars (Var {}) = []

vars :: Expr a -> String
vars x = nub $ freeVars x ++ boundVars x

isVar :: Expr a -> Char -> Bool
isVar (Abs {body}) a = isVar body a
isVar (Var {name = x}) a = x == a
isVar (App {function = lhs, input = rhs}) a = isVar lhs a || isVar rhs a

lambdaReduceNormal :: Expr ReduceInfo -> [DebugExpr]
lambdaReduceNormal expression
  | expression == fst result = [(expression, DebugInfo [])]
  | otherwise = result : lambdaReduceNormal (fst result)
  where
    result = bReduceNormal (noSub <$ expression)

lambdaReduceCBV :: Expr ReduceInfo -> [DebugExpr]
lambdaReduceCBV expression
  | Just exp <- result =  exp : lambdaReduceNormal (fst exp)
  | otherwise = []
  where
    result = bReduceCBV expression
