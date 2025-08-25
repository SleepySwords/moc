{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.TuringMachine.Turing where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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
    inputSymbols :: Symbols,
    tapeAlphabet :: Symbols,
    blank :: Symbol,
    transitionFunctions :: [TransitionFunction],
    initialState :: State,
    finalStates :: Set State
  }
  deriving (Show)

initialiseMachine :: TuringMachine -> Tape -> (Tape, State, Int)
initialiseMachine tm tape = (tape, initialState tm, 0)

-- FIXME: verify transitionFunction
verifyMachine :: TuringMachine -> Bool
verifyMachine TuringMachine {blank, tapeAlphabet, inputSymbols, states, initialState, finalStates, transitionFunctions} =
  blank `elem` tapeAlphabet
    && inputSymbols `isSubsetOf` tapeAlphabet
    && initialState `elem` states
    && finalStates `isSubsetOf` states
    && all
      ( \((i, a), (s, f, _)) ->
          i `elem` states
            && s `elem` states
            && a `elem` tapeAlphabet
            && f `elem` tapeAlphabet
      )
      transitionFunctions

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
      tape ++ blankPadding,
      []
    ]
  where
    tapeSymbol = fromMaybe (blank tm) (tape ^? element position)
    context = maybe "" contextString (transitionFunction tm (state, tapeSymbol))
    blankPadding = replicate (position + 1 - length tape) (blank tm)
    contextString (newState, newSymbol, shift) =
      " -> " ++ intercalate "," [newState, [newSymbol], show shift]

step :: TuringMachine -> (Tape, State, Int) -> Maybe (Tape, State, Int)
step tm (tape, state, position) = nextState <$> transition
  where
    tapeSymbol = fromMaybe (blank tm) (tape ^? element position)
    transition = transitionFunction tm (state, tapeSymbol)
    newTape newSymbol = take position tape ++ newSymbol : drop (position + 1) tape
    newPosition shift = case shift of
      LeftShift -> position - 1
      RightShift -> position + 1

    nextState (newState, newSymbol, shift) = (newTape newSymbol, newState, newPosition shift)

runMachine :: TuringMachine -> Tape -> [(Tape, State, Int)]
runMachine tm tape = states
  where
    states = scanUntil (step tm) (initialiseMachine tm tape)

isValid :: TuringMachine -> [(Tape, State, Int)] -> Bool
isValid tm states = checkState (last states)
  where
    checkState (_, state, _) = state `elem` finalStates tm
