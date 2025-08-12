{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.FiniteStateAutomota.NFA where

import Data.Set (Set, member)
import Data.Map (elems)
import ModelComputation.FiniteStateAutomota.DFA (DeterministFiniteAutomota)

type Symbol = Char

type State = String

data Shift = LeftShift | RightShift

instance Show Shift where
  show LeftShift = "L"
  show RightShift = "R"

type AString = [Symbol]

type TransitionFunction = ((State, Symbol), Set State)

data NondetermenistFiniteAutomota = NondetermenistFiniteAutomota
  { states :: Set State,
    alphabet :: Set Symbol,
    transitionFunctions :: [TransitionFunction],
    initialState :: State,
    finalStates :: Set State
  }

data Result = Success | Failiure deriving Show

type AutomotaInstance = (AString, State)

initialiseMachine :: NondetermenistFiniteAutomota -> AString -> AutomotaInstance
initialiseMachine NondetermenistFiniteAutomota {initialState} str = (str, initialState)

transitionFunction :: NondetermenistFiniteAutomota -> (State, Symbol) -> Maybe (Set State)
transitionFunction NondetermenistFiniteAutomota {transitionFunctions} l = lookup l transitionFunctions

translateToDFA :: NondetermenistFiniteAutomota -> DeterministFiniteAutomota
translateToDFA 
