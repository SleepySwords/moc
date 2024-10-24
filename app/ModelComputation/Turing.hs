{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.Turing where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, isSubsetOf)
import Utils (scanUntil)

type Symbol = Char

type Symbols = Set Symbol

type State = String

data Shift = LeftShift | RightShift

instance Show Shift where
  show LeftShift = "L"
  show RightShift = "R"

type Tape = [Symbol]

type TransitionFunction = ((State, Symbol), (State, Symbol, Shift))

data TuringMachine = TuringMachine
  { states :: Set State,
    tapeAlphabet :: Symbols,
    blank :: Symbol,
    inputSymbols :: Symbols,
    transitionFunctions :: [TransitionFunction],
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

transitionFunction :: TuringMachine -> (State, Symbol) -> Maybe (State, Symbol, Shift)
transitionFunction turingMachine l = lookup l $ transitionFunctions turingMachine

printState :: TuringMachine -> (Tape, State, Int) -> String
printState turingMachine (tape, state, position) =
  intercalate
    "\n"
    [ replicate position ' '
        ++ "▾"
        ++ state
        ++ context,
      tape,
      []
    ]
  where
    tapeSymbol = fromMaybe (blank turingMachine) (tape ^? element position)
    context = maybe "" contextString (transitionFunction turingMachine (state, tapeSymbol))
    contextString (newState, newSymbol, shift) =
      " -> " ++ intercalate "," [newState, [newSymbol], show shift]

step :: TuringMachine -> (Tape, State, Int) -> (Tape, State, Int)
step turingMachine (tape, state, position) = (newTape, newState, newPosition)
  where
    tapeSymbol = fromMaybe (blank turingMachine) (tape ^? element position)
    (newState, newSymbol, shift) = fromJust $ transitionFunction turingMachine (state, tapeSymbol)
    newTape = take position tape ++ newSymbol : drop (position + 1) tape
    newPosition = case shift of
      LeftShift -> position - 1
      RightShift -> position + 1

runMachine :: TuringMachine -> Tape -> [(Tape, State, Int)]
runMachine turingMachine tape = states
  where
    states = scanUntil (\(_, state, _) -> state `elem` finalStates turingMachine) (step turingMachine) (initialiseMachine turingMachine tape)
