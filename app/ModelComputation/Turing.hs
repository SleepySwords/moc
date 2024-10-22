{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.Turing where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, isSubsetOf)
import Debug.Trace (trace)

type Symbol = Char

type Symbols = Set Symbol

type State = String

data Shift = LeftShift | RightShift

instance Show Shift where
  show LeftShift = "L"
  show RightShift = "R"

type Tape = [Symbol]

data TuringMachine = TuringMachine
  { states :: Set State,
    tapeAlphabet :: Symbols,
    blank :: Symbol,
    inputSymbols :: Symbols,
    transitionFunction :: [((State, Symbol), (State, Symbol, Shift))],
    initialState :: State,
    finalStates :: Set State
  }

initialiseMachine :: TuringMachine -> Tape -> (Tape, State, Int)
initialiseMachine machine tape = (tape, initialState machine, 0)

-- FIXME: verify transitionFunction
verifyMachine :: TuringMachine -> Bool
verifyMachine TuringMachine {blank, tapeAlphabet, inputSymbols, states, initialState, finalStates} =
  blank `elem` tapeAlphabet
    && inputSymbols `isSubsetOf` tapeAlphabet
    && initialState `elem` states
    && finalStates `isSubsetOf` states

debug = flip trace

printState :: (Tape, State, Int) -> (State, Symbol, Shift) -> String
printState (tape, state, position) (newState, newSymbol, shift) =
  intercalate
    ","
    [ replicate position ' '
        ++ "▾"
        ++ state
        ++ " -> "
        ++ intercalate "," [newState, [newSymbol], show shift],
      tape,
      []
    ]

-- FIXME: hacky debug statement, do this properly.
step :: TuringMachine -> (Tape, State, Int) -> (Tape, State, Int)
step turingMachine currentState@(tape, state, position) = (newTape, newState, newPosition) `debug` printState currentState transitionResult
  where
    tapeSymbol = fromMaybe (blank turingMachine) (tape ^? element position)
    transitionResult@(newState, newSymbol, shift) = fromJust $ lookup (state, tapeSymbol) $ transitionFunction turingMachine
    newTape = take position tape ++ newSymbol : drop (position + 1) tape
    newPosition = case shift of
      LeftShift -> position - 1
      RightShift -> position + 1

runMachine :: TuringMachine -> Tape -> Tape
runMachine turingMachine tape = finalTape
  where
    (finalTape, _, _) = until (\(_, state, _) -> state `elem` finalStates turingMachine) (step turingMachine) (initialiseMachine turingMachine tape)
