{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module ModelComputation.FiniteStateAutomota.NFA where

import Data.List hiding (insert)
import Data.Maybe (mapMaybe)
import Data.Set (Set, difference, empty, fromList, insert, member, toList)
import qualified Data.Set as Set
import qualified ModelComputation.FiniteStateAutomota.DFA as D
import Utils (mapFst, mapSnd)

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

-- Not actually representitive of the delta* function, need to rethink this
-- ie: a --lambda--> b --lambda-->c is not translated properly.
getSetOfTransitions :: NondeterministFiniteAutomota -> Set State -> Symbol -> Set State
getSetOfTransitions nfa states s = fromList $ concatMap transitions (toList states)
  where
    combinations a = [(,s), (,emptyString)] <*> [a]
    transitions a = concatMap toList $ mapMaybe (transitionFunction nfa) (combinations a)

type TransitionFunctionIntermediate = ((Set State, Symbol), Set State)

data IntermediateFiniteAutomota = IntermediateFiniteAutomota
  { states_i :: Set (Set State),
    transitionFunctions_i :: [TransitionFunctionIntermediate],
    initialState_i :: Set State,
    finalStates_i :: Set (Set State),
    visited :: Set (Set State)
  }
  deriving (Show)

setToString :: Set (Set State) -> Set State
setToString = Set.map stateSetToString

stateSetToString :: Set State -> State
stateSetToString = (++ "]") . ('[' :) . intercalate "|" . toList

translateTransition :: [TransitionFunctionIntermediate] -> [D.TransitionFunction]
translateTransition = map (mapFst (mapFst stateSetToString) . mapSnd stateSetToString)

translateNFA :: NondeterministFiniteAutomota -> D.DeterministFiniteAutomota
translateNFA nfa@(NondetermisticFiniteAutomota {initialState, alphabet, finalStates}) = translate $ step initialised
  where
    initialised :: IntermediateFiniteAutomota
    initialised =
      IntermediateFiniteAutomota
        { states_i = fromList [fromList [initialState]],
          transitionFunctions_i = [],
          initialState_i = fromList [initialState],
          finalStates_i = empty,
          visited = empty
        }
    step :: IntermediateFiniteAutomota -> IntermediateFiniteAutomota
    step ifa@(IntermediateFiniteAutomota {visited, states_i, transitionFunctions_i}) = case Set.lookupMin toVisit of
      Just s ->
        let transitions = map (\a -> ((s, a), getSetOfTransitions nfa s a)) (toList alphabet)
         in step
              IntermediateFiniteAutomota
                { states_i = states_i `Set.union` fromList (map snd transitions),
                  transitionFunctions_i = transitionFunctions_i `union` transitions,
                  initialState_i = initialState_i ifa,
                  -- FIXME: pretty ugly
                  finalStates_i = if not $ Set.null (s `Set.intersection` finalStates) then Set.insert s (finalStates_i ifa) else finalStates_i ifa,
                  visited = Set.insert s visited
                }
      Nothing -> ifa
      where
        toVisit = difference states_i visited
    translate :: IntermediateFiniteAutomota -> D.DeterministFiniteAutomota
    translate ifa =
      D.DeterministFiniteAutomota
        { D.states = setToString (states_i ifa),
          D.alphabet = alphabet,
          D.transitionFunctions = translateTransition $ transitionFunctions_i ifa,
          D.initialState = stateSetToString $ initialState_i ifa,
          D.finalStates = setToString (finalStates_i ifa)
        }

-- Start with initial state {q0}
-- Forall a \in alphabet, add edge from {qi, qj, qk} to transitionFunction(qi, a) \union transitionFunction(qj, a) \union transitionFunction(qk, a)
-- Mark {qi, qj, qk} as visited and move onto the next state.
