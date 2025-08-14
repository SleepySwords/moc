{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.FiniteStateAutomota.DFA where

import Data.List (intercalate)
import Data.Set (Set, member, toList)

type Symbol = Char

type State = String

data Shift = LeftShift | RightShift

instance Show Shift where
  show LeftShift = "L"
  show RightShift = "R"

type AString = [Symbol]

type TransitionFunction = ((State, Symbol), State)

data DeterministFiniteAutomota = DeterministFiniteAutomota
  { states :: Set State,
    alphabet :: Set Symbol,
    transitionFunctions :: [TransitionFunction],
    initialState :: State,
    finalStates :: Set State
  }

instance Show DeterministFiniteAutomota where
  show
    DeterministFiniteAutomota
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
               ++ intercalate ", " ((\((a, b), c) -> "(" ++ a ++ "," ++ [b] ++ ") --> " ++ c) <$> transitionFunctions)
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

data Result = Success | Failiure deriving (Show)

type AutomotaInstance = (AString, State)

initialiseMachine :: DeterministFiniteAutomota -> AString -> AutomotaInstance
initialiseMachine DeterministFiniteAutomota {initialState} str = (str, initialState)

transitionFunction :: DeterministFiniteAutomota -> (State, Symbol) -> Maybe State
transitionFunction DeterministFiniteAutomota {transitionFunctions} l = lookup l transitionFunctions

step :: DeterministFiniteAutomota -> AutomotaInstance -> Either AutomotaInstance Result
step DeterministFiniteAutomota {finalStates} ([], state) = Right $ if member state finalStates then Success else Failiure
step dfa (s : str, state) = maybe (Right Failiure) (\a -> Left (str, a)) nextState
  where
    nextState = transitionFunction dfa (state, s)

runSteps :: DeterministFiniteAutomota -> AutomotaInstance -> Result
runSteps dfa inst = case step dfa inst of
  Left x -> runSteps dfa x
  Right x -> x

runDFA :: DeterministFiniteAutomota -> AString -> Result
runDFA dfa str = runSteps dfa initialMachine
  where
    initialMachine = initialiseMachine dfa str
