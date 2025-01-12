{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ModelComputation.LambdaCalculus.Types where

import Control.Monad
import Control.Monad.State (MonadState (get, put), State, evalState)

newtype ReduceInfo = ReduceInfo {substituted :: Maybe Char}

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
  sub :: a -> Bool

instance Colour () where
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
  sub ReduceInfo {substituted = Just _} = True
  sub ReduceInfo {substituted = Nothing} = False

-- Print non verbose
instance (Colour a) => Show (Expr a) where
  show e = evalState (showInternal e) (False, 0)

type PrintState = (Bool, Int)

colourSt :: Bool -> PrintState -> String
colourSt _ (True, _) = "\x1b[32m"
colourSt True (False, v) = let (r, g, b) = rainbowColour !! (v `mod` length rainbowColour) in rgbColour r g b
colourSt False (False, _) = ""

resetSt :: PrintState -> PrintState -> String
resetSt (False, _) (True, _) = "\x1b[0m"
resetSt _ _ = ""

resetC :: String
resetC = "\x1b[0m"

showInternal :: (Colour a) => Expr a -> State PrintState String
showInternal e = do
  oldSt <- get

  when (sub (info e) && not (fst oldSt)) $ put (True, snd oldSt)
  newSt <- get

  print <- showExpr e oldSt

  when (fst newSt && not (fst oldSt)) $ put (False, snd newSt)

  return print

showExpr :: (Colour a) => Expr a -> PrintState -> State PrintState String
showExpr (Abs {bind, body}) oldSt = do
  newSt <- get

  printBody <- showInternal body
  let st = colourSt False newSt ++ "λ" ++ [bind] ++ "." ++ printBody ++ resetSt oldSt newSt

  return st
showExpr (App {function = m, input = n}) oldSt = do
  newSt <- get

  printFun <- mPrint
  printInput <- nPrint
  let st = colourSt False newSt ++ printFun ++ " " ++ printInput ++ resetSt oldSt newSt

  return st
  where
    mPrint = case m of
      Abs {} -> showInternal m >>= parantheses
      _ -> showInternal m
    nPrint = case n of
      Var {} -> showInternal n
      _ -> showInternal n >>= parantheses
    parantheses :: String -> State PrintState String
    parantheses e =
      do
        st <- get
        put (fst st, snd st + 1)
        return $ colourSt True st ++ "(" ++ resetC ++ e ++ colourSt True st ++ ")" ++ resetC
showExpr (Var {name}) oldSt = do
  newSt <- get

  let st = colourSt False newSt ++ [name] ++ resetSt oldSt newSt

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
