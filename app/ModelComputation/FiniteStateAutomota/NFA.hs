{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.FiniteStateAutomota.NFA where

import Data.Foldable (Foldable (..))
import Data.List
import Data.Set (Set, fromList, member)
import qualified Data.Set (map)

type Symbol = Char

type State = String

data Shift = LeftShift | RightShift

instance Show Shift where
  show LeftShift = "L"
  show RightShift = "R"

type AString = [Symbol]

type TransitionFunction = ((State, Symbol), Set State)

data NondeterministFiniteAutomota = NondetermisticFiniteAutomota
  { states :: Set State,
    alphabet :: Set Symbol,
    transitionFunctions :: [TransitionFunction],
    initialState :: State,
    finalStates :: Set State
  }

instance Show NondeterministFiniteAutomota where
  show
    NondetermisticFiniteAutomota
      { states,
        alphabet,
        transitionFunctions,
        initialState,
        finalStates
      } =
      ( "states = {"
          ++ intercalate ", " (toList states)
          ++ "}\n"
      )
        ++ ( "alphabet = {"
               ++ intercalate ", " ((: []) <$> toList alphabet)
               ++ "}\n"
           )
        ++ ( "transitionFunctions = {"
               ++ intercalate ", " ((\((a, b), c) -> "(" ++ a ++ "," ++ [b] ++ ") --> {" ++ intercalate ", " (toList c) ++ "}") <$> transitionFunctions)
               ++ "}\n"
           )
        ++ ( "initialState = "
               ++ initialState
               ++ "\n"
           )
        ++ ( "finalStates = {"
               ++ intercalate ", " (toList finalStates)
               ++ "}"
           )

data Result = Success | Failiure deriving (Show, Eq, Ord)

(|||) :: Result -> Result -> Result
Failiure ||| Failiure = Failiure
_ ||| _ = Success

type AutomotaInstance = (AString, State)

emptyString :: Symbol
emptyString = 'λ'

initialiseMachine :: NondeterministFiniteAutomota -> AString -> AutomotaInstance
initialiseMachine NondetermisticFiniteAutomota {initialState} str = (str, initialState)

transitionFunction :: NondeterministFiniteAutomota -> (State, Symbol) -> Maybe (Set State)
transitionFunction NondetermisticFiniteAutomota {transitionFunctions} l = lookup l transitionFunctions

-- FIXME: implement the empty string
isValid :: NondeterministFiniteAutomota -> AutomotaInstance -> Result
isValid NondetermisticFiniteAutomota {finalStates} ([], state) = if member state finalStates then Success else Failiure
isValid nfa (s : str, state) = result ||| resultEmpty
  where
    anyValid :: AString -> Set State -> Result
    anyValid st a = if Success `elem` Data.Set.map (\x -> isValid nfa (st, x)) a then Success else Failiure
    nextStates = transitionFunction nfa (state, s)
    result = maybe Failiure (anyValid str) nextStates
    emptyStates = transitionFunction nfa (state, emptyString)
    resultEmpty = maybe Failiure (anyValid (s : str)) emptyStates

runNFA :: NondeterministFiniteAutomota -> AString -> Result
runNFA nfa str = isValid nfa initialMachine
  where
    initialMachine = initialiseMachine nfa str

-- translateToDFA :: NondetermenistFiniteAutomota -> DeterministFiniteAutomota
-- translateToDFA
