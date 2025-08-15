{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.FiniteStateAutomota.NFA where

import qualified Control.Applicative as Set
import Data.Foldable (Foldable (..))
import Data.List
import Data.Set (Set, difference, empty, fromList, insert, member)
import qualified Data.Set (map)
import Debug.Trace (trace)

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

isValid :: NondeterministFiniteAutomota -> Set State -> AutomotaInstance -> Result
isValid nfa emptyTransitions ([], state) = if member state (finalStates nfa) then Success else resultEmpty
  where
    anyValid :: AString -> Set State -> Set State -> Result
    anyValid st eTransitions a = if Success `elem` Data.Set.map (\x -> isValid nfa eTransitions (st, x)) a then Success else Failiure
    emptyStates = transitionFunction nfa (state, emptyString)
    resultEmpty = maybe Failiure (anyValid [] (Data.Set.insert state emptyTransitions) . (`difference` emptyTransitions)) emptyStates
isValid nfa emptyTransitions (s : str, state) = result ||| resultEmpty
  where
    anyValid :: AString -> Set State -> Set State -> Result
    anyValid st eTransitions a = if Success `elem` Data.Set.map (\x -> isValid nfa eTransitions (st, x)) a then Success else Failiure
    nextStates = transitionFunction nfa (state, s)
    result = maybe Failiure (anyValid str empty) nextStates
    emptyStates = transitionFunction nfa (state, emptyString)
    resultEmpty = maybe Failiure (anyValid (s : str) (Data.Set.insert state emptyTransitions) . (`difference` emptyTransitions)) emptyStates

runNFA :: NondeterministFiniteAutomota -> AString -> Result
runNFA nfa str = isValid nfa empty initialMachine
  where
    initialMachine = initialiseMachine nfa str

-- translateToDFA :: NondetermenistFiniteAutomota -> DeterministFiniteAutomota
-- translateToDFA
