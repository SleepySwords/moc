{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Reduction where

import Control.Applicative
import Control.Monad.State (State, runState, get, MonadState (put))
import Data.List (nub, union)
import Data.Map (Map)
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..))
import qualified Data.Map as Map

type SubstiutionState = State (Map (Expr ReduceInfo) (Expr ReduceInfo))

noSub :: ReduceInfo
noSub = ReduceInfo {substituted = Nothing, rainbow = 0}

replacedBind :: Char -> ReduceInfo
replacedBind x = ReduceInfo {substituted = Just x, rainbow = 0}

-- t[x := r]
-- t, s and r are lambda vars
-- x and y are vars
-- Inside t, x replaces r
substitution :: Expr a -> Char -> Expr a -> Expr a
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
  | v /= x && not (isFreeVar r v) = Abs info v (substitution t x r)
  -- Must alpha reduce here to avoid name collisions
  | otherwise = substitution (aConversion abst r) x r

-- (λx.t) s -> t[x := s]
bReduceGreedy :: Expr ReduceInfo -> Expr ReduceInfo
bReduceGreedy (App {function = (Abs {bind, body}), input = x}) = substitution body bind (replacedBind bind <$ x)
bReduceGreedy (App {info, function, input}) = App info (bReduceGreedy function) (bReduceGreedy input)
bReduceGreedy (Abs {info, bind, body}) = Abs info bind (bReduceGreedy body)
bReduceGreedy a = a

-- (λx.t) s -> t[x := s]
bReduceGreedyMemo :: Expr ReduceInfo -> SubstiutionState (Expr ReduceInfo)
bReduceGreedyMemo app@(App {function = (Abs {bind, body}), input = x}) = do
  cacheMap <- get
  case Map.lookup (normalisation app) cacheMap of
    Just v -> return v
    Nothing -> do
      let state = substitution body bind (replacedBind bind <$ x)
      put $ Map.insert (normalisation app) state cacheMap
      return state
bReduceGreedyMemo (App {info, function, input}) = App info <$> bReduceGreedyMemo function <*> bReduceGreedyMemo input
bReduceGreedyMemo (Abs {info, bind, body}) = Abs info bind <$> bReduceGreedyMemo body
bReduceGreedyMemo a = return a

bReduceNonGreedy :: Expr ReduceInfo -> Maybe (Expr ReduceInfo)
-- (λx.t) s -> t[x := s]
bReduceNonGreedy (App {info, function = f@(Abs {bind, body}), input = x}) =
  App info f <$> bReduceNonGreedy x
    <|> (\a -> App info a x) <$> bReduceNonGreedy f
    <|> Just (substitution body bind (replacedBind bind <$ x))
bReduceNonGreedy (App {info, function = f, input = x}) =
  App info f <$> bReduceNonGreedy x
    <|> (\a -> App info a x) <$> bReduceNonGreedy f
bReduceNonGreedy (Abs {info, bind, body}) = Abs info bind <$> bReduceNonGreedy body
bReduceNonGreedy _ = Nothing

aConversion :: Expr a -> Expr a -> Expr a
aConversion abst@(Abs {info, bind, body}) toSub = Abs info suitable_char (substitution body bind (Var info suitable_char))
  where
    suitable_char = head [x | x <- ['a' .. 'z'], not $ isVar abst x, not $ isFreeVar toSub x]
aConversion _ _ = error "Cannot alpha reduce with not an abstraction"

freeVars :: Expr a -> [Char]
freeVars (Abs {bind, body}) = [x | x <- freeVars body, x /= bind]
freeVars (Var {name = x}) = [x]
freeVars (App {function = lhs, input = rhs}) = freeVars lhs `union` freeVars rhs

isFreeVar :: Expr a -> Char -> Bool
isFreeVar (Abs {bind, body}) a = (bind /= a) && isFreeVar body a
isFreeVar (Var {name = x}) a = x == a
isFreeVar (App {function = lhs, input = rhs}) a = isFreeVar lhs a || isFreeVar rhs a

boundVars :: Expr a -> [Char]
boundVars (Abs {bind, body}) = bind : boundVars body
boundVars (App {function, input}) = boundVars function `union` boundVars input
boundVars (Var {}) = []

vars :: Expr a -> [Char]
vars x = nub $ freeVars x ++ boundVars x

isVar :: Expr a -> Char -> Bool
isVar (Abs {body}) a = isVar body a
isVar (Var {name = x}) a = x == a
isVar (App {function = lhs, input = rhs}) a = isVar lhs a || isVar rhs a

lambdaReduceGreedy :: Expr ReduceInfo -> [Expr ReduceInfo]
lambdaReduceGreedy expression
  | expression == result = [expression]
  | otherwise = expression : lambdaReduceGreedy result
  where
    result = bReduceGreedy (noSub <$ expression)

lambdaReduceGreedyMemo :: Map (Expr ReduceInfo) (Expr ReduceInfo) -> Expr ReduceInfo -> [Expr ReduceInfo]
lambdaReduceGreedyMemo cacheMap expression
  | expression == result = [expression]
  | otherwise = expression : lambdaReduceGreedyMemo cache result
  where
    (result, cache) = runState (bReduceGreedyMemo (noSub <$ expression)) cacheMap

lambdaReduceNonGreedy :: Expr ReduceInfo -> [Expr ReduceInfo]
lambdaReduceNonGreedy expression
  | Just exp <- result = expression : lambdaReduceNonGreedy exp
  | otherwise = [expression]
  where
    result = bReduceNonGreedy (noSub <$ expression)

normalisation :: Expr a -> Expr a
-- A free variable so do not change anything
normalisation (Var info x) = Var info x
normalisation (App {input = input, info = info, function = fun}) = App info (normalisation input) (normalisation fun)
normalisation (Abs {body = body, info = info, bind = bind}) = Abs info potentialFree (normalisation $ substitution body bind (Var info potentialFree))
  where
    -- Using capitals as the parser dissalows capitals and
    -- hence they cannot conflict (we will have to store the lower case version when memorising).
    potentialFree = head $ filter (not . isVar body) ['A' .. 'Z']
