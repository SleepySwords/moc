{-# LANGUAGE NamedFieldPuns #-}

module Repl.Types where

import Data.List (intercalate)
import qualified Data.Set as Set
import ModelComputation.FiniteStateAutomota.DFA (DeterministFiniteAutomota)
import ModelComputation.FiniteStateAutomota.NFA (NondeterministFiniteAutomota (..))

data Expr
  = Literal String
  | Ident String
  | Set (Set.Set Expr)
  | Tuple [Expr]
  | Function (Expr, Expr)
  | Call Expr Expr
  | NFA NondeterministFiniteAutomota
  | DFA DeterministFiniteAutomota
  deriving (Ord, Eq)

data Statement = Assignment String Expr | Expression Expr

instance Show Expr where
  show (Literal x) = "\"" ++ x ++ "\""
  show (Set x) = "{ " ++ intercalate ", " (show <$> Set.toList x) ++ " }"
  show (Tuple x) = "( " ++ intercalate ", " (map show x) ++ " )"
  show (Function (x, y)) = "λ" ++ show x ++ " -> " ++ show y
  show (Call x y) = show x ++ " " ++ show y
  show (Ident n) = show n
  show (NFA n) = "NFA " ++ "(" ++ showNFA n ++ " )"
  show (DFA n) = show n

showNFA :: NondeterministFiniteAutomota -> String
showNFA
  NondetermisticFiniteAutomota
    { states,
      alphabet,
      transitionFunctions,
      initialState,
      finalStates
    } =
    ( "{ "
        ++ intercalate ", " (Set.toList states)
        ++ " }, "
    )
      ++ ( "{ "
             ++ intercalate ", " ((: []) <$> Set.toList alphabet)
             ++ " }, "
         )
      ++ ( " { "
             ++ intercalate ", " ((\((a, b), c) -> "\\( " ++ a ++ "," ++ [b] ++ " ) -> { " ++ intercalate ", " (Set.toList c) ++ " }") <$> transitionFunctions)
             ++ " }, "
         )
      ++ ( initialState ++ ", ")
      ++ ("{ " ++ intercalate ", " (Set.toList finalStates) ++ " }")

instance Show Statement where
  show (Assignment x y) = x ++ " := " ++ show y
  show (Expression x) = show x
