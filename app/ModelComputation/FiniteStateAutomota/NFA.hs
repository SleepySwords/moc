{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module ModelComputation.FiniteStateAutomota.NFA where

import Data.List hiding (insert)
import Data.Set (Set, difference, empty, insert, member, toList)

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

instance Semigroup Result where
  Failiure <> Failiure = Failiure
  _ <> _ = Success

instance Monoid Result where
  mempty = Failiure

type AutomotaInstance = (AString, State)

emptyString :: Symbol
emptyString = 'λ'

initialiseMachine :: NondeterministFiniteAutomota -> AString -> AutomotaInstance
initialiseMachine NondetermisticFiniteAutomota {initialState} str = (str, initialState)

transitionFunction :: NondeterministFiniteAutomota -> (State, Symbol) -> Maybe (Set State)
transitionFunction NondetermisticFiniteAutomota {transitionFunctions} l = lookup l transitionFunctions

anyValid :: NondeterministFiniteAutomota -> AString -> Set State -> Set State -> Result
anyValid nfa str eTransitions states = mconcat $ map (isValid nfa eTransitions . (str,)) $ toList states

isValid :: NondeterministFiniteAutomota -> Set State -> AutomotaInstance -> Result
isValid nfa emptyTransitions ([], state) = if member state (finalStates nfa) then Success else resultEmpty
  where
    eTransitions = transitionFunction nfa (state, emptyString)
    resultEmpty = maybe Failiure (anyValid nfa [] (insert state emptyTransitions) . (`difference` emptyTransitions)) eTransitions
isValid nfa emptyTransitions (s : str, state) = result <> resultEmpty
  where
    tranitions = transitionFunction nfa (state, s)
    result = maybe Failiure (anyValid nfa str empty) tranitions
    eTransitions = transitionFunction nfa (state, emptyString)
    
    resultEmpty = maybe Failiure (anyValid nfa (s : str) (insert state emptyTransitions) . (`difference` emptyTransitions)) eTransitions

runNFA :: NondeterministFiniteAutomota -> AString -> Result
runNFA nfa str = isValid nfa empty initialMachine
  where
    initialMachine = initialiseMachine nfa str
