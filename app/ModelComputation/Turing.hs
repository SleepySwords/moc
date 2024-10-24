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
initialiseMachine tm tape = (tape, initialState tm, 0)

-- FIXME: verify transitionFunction
verifyMachine :: TuringMachine -> Bool
verifyMachine TuringMachine {blank, tapeAlphabet, inputSymbols, states, initialState, finalStates} =
  blank `elem` tapeAlphabet
    && inputSymbols `isSubsetOf` tapeAlphabet
    && initialState `elem` states
    && finalStates `isSubsetOf` states

transitionFunction :: TuringMachine -> (State, Symbol) -> Maybe (State, Symbol, Shift)
transitionFunction tm l = lookup l $ transitionFunctions tm

printState :: TuringMachine -> (Tape, State, Int) -> String
printState tm (tape, state, position) =
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
    tapeSymbol = fromMaybe (blank tm) (tape ^? element position)
    context = maybe "" contextString (transitionFunction tm (state, tapeSymbol))
    contextString (newState, newSymbol, shift) =
      " -> " ++ intercalate "," [newState, [newSymbol], show shift]

step :: TuringMachine -> (Tape, State, Int) -> (Tape, State, Int)
step tm (tape, state, position) = (newTape, newState, newPosition)
  where
    tapeSymbol = fromMaybe (blank tm) (tape ^? element position)
    (newState, newSymbol, shift) = fromJust $ transitionFunction tm (state, tapeSymbol)
    newTape = take position tape ++ newSymbol : drop (position + 1) tape
    newPosition = case shift of
      LeftShift -> position - 1
      RightShift -> position + 1

runMachine :: TuringMachine -> Tape -> [(Tape, State, Int)]
runMachine tm tape = states
  where
    states = scanUntil (\(_, state, _) -> state `elem` finalStates tm) (step tm) (initialiseMachine tm tape)
