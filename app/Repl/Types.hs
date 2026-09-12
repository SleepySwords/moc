{-# LANGUAGE NamedFieldPuns #-}

module Repl.Types where

import Data.List (intercalate)
import qualified Data.Set as Set
import qualified ModelComputation.FiniteStateAutomota.DFA as DFA
import qualified ModelComputation.FiniteStateAutomota.NFA as NFA
import ModelComputation.TuringMachine.Turing (TuringMachine)

data Expr
  = Literal String
  | Ident String
  | Set (Set.Set Expr)
  | Tuple [Expr]
  | Function (Expr, Expr)
  | Call Expr Expr
  | NFA NFA.NondeterministFiniteAutomota
  | DFA DFA.DeterministFiniteAutomota
  | Turing TuringMachine
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
  show (DFA n) = "DFA " ++ "(" ++ showDFA n ++ " )"

showNFA :: NFA.NondeterministFiniteAutomota -> String
showNFA
  NFA.NondetermisticFiniteAutomota
    { NFA.states,
      NFA.alphabet,
      NFA.transitionFunctions,
      NFA.initialState,
      NFA.finalStates
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

showDFA :: DFA.DeterministFiniteAutomota -> String
showDFA
  DFA.DeterministFiniteAutomota
    { DFA.states,
      DFA.alphabet,
      DFA.transitionFunctions,
      DFA.initialState,
      DFA.finalStates
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
             ++ intercalate ", " ((\((a, b), c) -> "\\( " ++ a ++ "," ++ [b] ++ " ) -> " ++ c) <$> transitionFunctions)
             ++ " }, "
         )
      ++ ( initialState ++ ", ")
      ++ ("{ " ++ intercalate ", " (Set.toList finalStates) ++ " }")

instance Show Statement where
  show (Assignment x y) = x ++ " := " ++ show y
  show (Expression x) = show x
