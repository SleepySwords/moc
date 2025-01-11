{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Types where

import Control.Monad
import Control.Monad.State (MonadState (get, put), State, evalState, runState)
import Data.Functor.Classes (showsBinary1)

data ReduceInfo = ReduceInfo {substituted :: Maybe Char, rainbow :: Int}

instance Eq ReduceInfo where
  _ == _ = True

instance Ord ReduceInfo where
  compare _ _ = EQ

-- Use generics here, parser does not need this info..
data Expr a
  = Var {info :: a, name :: Char}
  | App {info :: a, function :: Expr a, input :: Expr a}
  | Abs {info :: a, bind :: Char, body :: Expr a}
  deriving (Eq, Ord)

-- This is slow need to think
instance Functor Expr where
  fmap f Var {info, name} = Var (f info) name
  fmap f App {info, function, input} = App (f info) (fmap f function) (fmap f input)
  fmap f Abs {info, bind, body} = Abs (f info) bind (fmap f body)

class Colour a where
  colour :: a -> String
  rainbowP :: a -> String
  updateP :: a -> a
  reset :: a -> String

  sub :: a -> Bool

instance Colour () where
  colour = const ""
  rainbowP = const ""
  updateP = id
  reset = const ""

  sub = const False

rainbowColour :: [(Int, Int, Int)]
rainbowColour =
  [ (148, 0, 211),
    (75, 0, 130),
    (0, 0, 255),
    -- (0, 255, 0),
    -- (255, 255, 0),
    (255, 127, 0),
    (255, 0, 0)
  ]

rgbColour :: Int -> Int -> Int -> String
rgbColour r g b = "\x1b[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

-- This is slow need to think
instance Colour ReduceInfo where
  colour ReduceInfo {substituted = Just _} = "\x1b[32m"
  colour ReduceInfo {substituted = Nothing} = ""

  sub ReduceInfo {substituted = Just _} = True
  sub ReduceInfo {substituted = Nothing} = False

  -- rainbowP ReduceInfo {} = ""

  -- rainbowP ReduceInfo {rainbow = v} = "\x1b[" ++ show (33 + (v `mod` 3)) ++ "m"
  rainbowP ReduceInfo {rainbow = v} = let (r, g, b) = rainbowColour !! (v `mod` length rainbowColour) in rgbColour r g b
  updateP ReduceInfo {rainbow = v, substituted} = ReduceInfo {substituted, rainbow = v + 1}
  reset = const "\x1b[0m"

-- Print non verbose
instance (Colour a) => Show (Expr a) where
  show e = evalState (showInternal e) False

-- show (Var {name = x, info}) = colour info ++ [x] ++ reset info
-- show (App {info, function = m, input = n}) = colour info ++ mPrint ++ " " ++ nPrint ++ reset info
--   where
--     mPrint = case m of
--       Abs {} -> parantheses $ show $ updateP <$> m
--       _ -> show m
--     nPrint = case n of
--       Var {} -> show n
--       _ -> parantheses $ show $ updateP <$> n
--     parantheses str = concat [rainbowP info, colour info, "(", reset info, colour info, str, rainbowP info, colour info, ")", reset info]
-- show (Abs {info, bind, body}) = colour info ++ "λ" ++ [bind] ++ "." ++ show body ++ reset info

colourSt True = "\x1b[32m"
colourSt False = ""

resetSt True = "\x1b[0m"
resetSt False = ""


showInternal :: (Colour a) => Expr a -> State Bool String
showInternal (Abs {info, bind, body}) = do
  oldSt <- get

  when (sub info && not oldSt) $ put True
  newSt <- get

  printBody <- showInternal body
  let st = colourSt newSt ++ "λ" ++ [bind] ++ "." ++ printBody ++ resetSt (newSt && not oldSt) 

  when (newSt && not oldSt) $ put False

  return st
showInternal (App {info, function = m, input = n}) = do
  oldSt <- get

  when (sub info && not oldSt) $ put True
  newSt <- get

  printFun <- mPrint
  printInput <- nPrint
  let st = colourSt newSt ++ printFun ++ " " ++ printInput ++ resetSt (newSt && not oldSt)

  when (newSt && not oldSt) $ put False

  return st
  where
    mPrint = case m of
      Abs {} -> (parantheses) <$> (showInternal m)
      _ -> showInternal m
    nPrint = case n of
      Var {} -> showInternal n
      _ -> (parantheses) <$> (showInternal n)
    parantheses e = "(" ++ e ++ ")"
showInternal (Var {info, name}) = do
  oldSt <- get

  when (sub info && not oldSt) $ put True
  newSt <- get

  let st = colourSt newSt ++ [name] ++ resetSt (newSt && not oldSt)

  when (newSt && not oldSt) $ put False

  return st

-- show (Abs {info, bind, body}) = colour info ++ "λ" ++ bind : vars ++ "." ++ show inner_body ++ reset info
--   where
--     flattenAbs :: Expr a -> (Expr a, [Char])
--     flattenAbs (Abs {bind = v, body = b}) =
--       let (ib, vs) = flattenAbs b
--        in (ib, v : vs)
--     flattenAbs e = (e, [])
--     (inner_body, vars) = flattenAbs body

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
