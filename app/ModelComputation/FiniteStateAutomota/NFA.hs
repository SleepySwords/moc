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

(|||) :: Result -> Result -> Result
Failiure ||| Failiure = Failiure
_ ||| _ = Success

type AutomotaInstance = (AString, State)

emptyString :: Symbol
emptyString = 'λ'

initialiseMachine :: NondeterministFiniteAutomota -> AString -> AutomotaInstance
initialiseMachine NondetermenistFiniteAutomota {initialState} str = (str, initialState)

transitionFunction :: NondeterministFiniteAutomota -> (State, Symbol) -> Maybe (Set State)
transitionFunction NondetermenistFiniteAutomota {transitionFunctions} l = lookup l transitionFunctions

-- FIXME: implement the empty string
isValid :: NondeterministFiniteAutomota -> AutomotaInstance -> Result
isValid NondetermenistFiniteAutomota {finalStates} ([], state) = if member state finalStates then Success else Failiure
isValid nfa (s : str, state) = result ||| resultEmpty
  where
    anyValid :: AString -> Set State -> Result
    anyValid st a = if Success `elem` Data.Set.map (\x -> isValid nfa (st, x)) a then Success else Failiure
    nextStates = transitionFunction nfa (state, s)
    result = maybe Failiure (anyValid str) nextStates
    emptyStates = transitionFunction nfa (state, emptyString)
    resultEmpty = maybe Failiure (anyValid (s:str)) emptyStates

runNFA :: NondeterministFiniteAutomota -> AString -> Result
runNFA nfa str = isValid nfa initialMachine
  where
    initialMachine = initialiseMachine nfa str

-- translateToDFA :: NondetermenistFiniteAutomota -> DeterministFiniteAutomota
-- translateToDFA 
