{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.FiniteStateAutomota.NFA where

import Data.Set (Set, member)
import qualified Data.Set (map)

type Symbol = Char

type State = String

data Shift = LeftShift | RightShift

instance Show Shift where
  show LeftShift = "L"
  show RightShift = "R"

type AString = [Symbol]

type TransitionFunction = ((State, Symbol), Set State)

data NondeterministFiniteAutomota = NondetermenistFiniteAutomota
  { states :: Set State,
    alphabet :: Set Symbol,
    transitionFunctions :: [TransitionFunction],
    initialState :: State,
    finalStates :: Set State
  }

data Result = Success | Failiure deriving (Show, Eq, Ord)

type AutomotaInstance = (AString, State)

initialiseMachine :: NondeterministFiniteAutomota -> AString -> AutomotaInstance
initialiseMachine NondetermenistFiniteAutomota {initialState} str = (str, initialState)

transitionFunction :: NondeterministFiniteAutomota -> (State, Symbol) -> Maybe (Set State)
transitionFunction NondetermenistFiniteAutomota {transitionFunctions} l = lookup l transitionFunctions

-- FIXME: implement the empty string
isValid :: NondeterministFiniteAutomota -> AutomotaInstance -> Result
isValid NondetermenistFiniteAutomota {finalStates} ([], state) = if member state finalStates then Success else Failiure
isValid nfa (s : str, state) = maybe Failiure anyValid nextStates
  where
    anyValid :: Set State -> Result
    anyValid a = if Success `elem` Data.Set.map (\x -> isValid nfa (str, x)) a then Success else Failiure
    nextStates = transitionFunction nfa (state, s)

runNFA :: NondeterministFiniteAutomota -> AString -> Result
runNFA nfa str = isValid nfa initialMachine
  where
    initialMachine = initialiseMachine nfa str

-- translateToDFA :: NondetermenistFiniteAutomota -> DeterministFiniteAutomota
-- translateToDFA 
