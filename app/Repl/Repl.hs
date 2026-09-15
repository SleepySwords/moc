module Repl.Repl where

import Control.Monad.State (MonadState (get, put), State, gets)
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Set
import qualified Data.Set as Set
import qualified ModelComputation.FiniteStateAutomota.DFA as DFA
import qualified ModelComputation.FiniteStateAutomota.NFA as NFA
import ModelComputation.TuringMachine.Turing (Shift)
import qualified ModelComputation.TuringMachine.Turing as Turing
import Repl.Types (Expr (Call, DFA, Function, Ident, Literal, NFA, Set, Tuple, Turing), Statement (Assignment, Expression))
import Data.List (intercalate)

type SymbolTable = Map String Expr

evaluateStatement :: Statement -> State SymbolTable String
evaluateStatement s =
  case s of
    (Assignment v a) -> do
      st <- get
      let e = evaluateExpression st a
      put (insert v e st)
      return (v ++ " := " ++ show e)
    (Expression e) -> gets (\a -> (show . evaluateExpression a) e)

-- Return an error
evaluateExpression :: SymbolTable -> Expr -> Expr
evaluateExpression _ (Literal s) = Literal s
evaluateExpression st (Ident s) = fromMaybe (Literal s) (Data.Map.lookup s st)
evaluateExpression st (Set s) = Set $ Data.Set.map (evaluateExpression st) s
evaluateExpression st (Tuple s) = Tuple $ evaluateExpression st <$> s
evaluateExpression st (Function (a, b)) = Function (evaluateExpression st a, evaluateExpression st b)
-- Evaluate actual constructions
evaluateExpression st (Call a b) = evaluateCall (evaluateExpression st a) (evaluateExpression st b)
evaluateExpression _ (NFA nfa) = NFA nfa
evaluateExpression _ (DFA dfa) = DFA dfa
evaluateExpression _ (Turing tur) = Turing tur

exprToNFA :: Expr -> Either String Expr
exprToNFA (Tuple [st, sm, fn, i, fi]) = do
  states <- tryStates st
  alph <- trySymbols sm
  functions <- tryNonDetFunctions fn
  initialState <- tryState i
  fin <- tryStates fi
  return $ NFA (NFA.NondetermisticFiniteAutomota states alph functions initialState fin)
exprToNFA _ = Left "Invalid argument: expected tuple with length of five"

exprToDFA :: Expr -> Either String Expr
exprToDFA (Tuple [st, sm, fn, i, fi]) = do
  states <- tryStates st
  alph <- trySymbols sm
  functions <- tryDetFunctions fn
  initialState <- tryState i
  fin <- tryStates fi
  return $ DFA (DFA.DeterministFiniteAutomota states alph functions initialState fin)
exprToDFA _ = Left "Invalid argument: expected tuple with length of five"

exprToTuring :: Expr -> Either String Expr
exprToTuring (Tuple [st, inSym, tapeAlph, b, tf, initState, finalStates]) = do
  states <- tryStates st
  inputSymbols <- trySymbols inSym
  tapeAlphabet <- trySymbols tapeAlph
  blank <- trySymbol b
  transitionFunction <- tryTuringFunctions tf
  _initState <- tryState initState
  _finalStates <- tryStates finalStates
  return $ Turing (Turing.TuringMachine states inputSymbols tapeAlphabet blank transitionFunction _initState _finalStates)
exprToTuring _ = Left "Invalid argument: expected tuple with length of five"

evaluateCall :: Expr -> Expr -> Expr
evaluateCall (Literal "NFA") b = either Ident id (exprToNFA b)
evaluateCall (Literal "DFA") b = either Ident id (exprToDFA b)
evaluateCall (Literal "Turing") b = either Ident id (exprToTuring b)
evaluateCall (NFA n) (Literal b) = Ident (show $ NFA.runNFA n b)
evaluateCall (NFA n) (Ident b) = Ident (show $ NFA.runNFA n b)
evaluateCall (DFA n) (Literal b) = Ident (show $ DFA.runDFA n b)
evaluateCall (DFA n) (Ident b) = Ident (show $ DFA.runDFA n b)
evaluateCall (Turing n) (Literal b) = Tuple [result, traceback steps]
  where steps = Turing.runMachine n b
        result = Ident $ if Turing.isValid n steps then "Success" else "Failure"
        outputStr = Ident $ let (x, _, _) = last steps in x
evaluateCall (Turing n) (Ident b) = Ident (show $ Turing.runMachine n b)
evaluateCall a b = Call a b

traceback :: [(Turing.Tape, Turing.State, Int)] -> Expr
traceback machine =
  Tuple $ map (\(a, b, c) -> Tuple [Literal a, Ident b, Ident (show c)]) machine

printMachineSteps :: Turing.TuringMachine -> [(Turing.Tape, Turing.State, Int)] -> String
printMachineSteps machine output =
  intercalate "\n" (map (Turing.printState machine) output)

tryStates :: Expr -> Either String (Data.Set.Set String)
tryStates (Set s) = Set.fromList <$> mapM tryState (Set.toList s)
tryStates _ = Left "Invalid argument: states is not a set"

tryState :: Expr -> Either String String
tryState (Literal x) = Right x
tryState _ = Left "Invalid argument: a state is not a string"

trySymbols :: Expr -> Either String (Data.Set.Set Char)
trySymbols (Set s) = Set.fromList <$> mapM trySymbol (Set.toList s)
trySymbols _ = Left "Invalid argument: symbols is not a set"

trySymbol :: Expr -> Either String Char
trySymbol (Literal [x]) = Right x
trySymbol _ = Left "Invalid argument: symbol is not a literal"

tryNonDetFunctions :: Expr -> Either String [NFA.TransitionFunction]
tryNonDetFunctions (Set s) = mapM tryNonDetFunction (Set.toList s)
tryNonDetFunctions _ = Left "Invalid argument: functions is not a set"

tryNonDetFunction :: Expr -> Either String NFA.TransitionFunction
tryNonDetFunction (Function (Tuple [a, b], c)) = do
  inputState <- tryState a
  inputSymbol <- trySymbol b
  outputState <- tryStates c
  return ((inputState, inputSymbol), outputState)
tryNonDetFunction _ = Left "Invalid argument: functions contains a non-function type"

tryDetFunctions :: Expr -> Either String [DFA.TransitionFunction]
tryDetFunctions (Set s) = mapM tryDetFunction (Set.toList s)
tryDetFunctions _ = Left "Invalid argument: functions is not a set"

tryDetFunction :: Expr -> Either String DFA.TransitionFunction
tryDetFunction (Function (Tuple [a, b], c)) = do
  inputState <- tryState a
  inputSymbol <- trySymbol b
  outputState <- tryState c
  return ((inputState, inputSymbol), outputState)
tryDetFunction _ = Left "Invalid argument: functions contains a non-function type"

tryTuringFunctions :: Expr -> Either String [Turing.TransitionFunction]
tryTuringFunctions (Set s) = mapM tryTuringFunction (Set.toList s)
tryTuringFunctions _ = Left "Invalid argument: functions is not a set"

tryShift :: Expr -> Either String Shift
tryShift (Literal ['L']) = Right Turing.LeftShift
tryShift (Literal ['R']) = Right Turing.LeftShift
tryShift _ = Left "Invalid argument: symbol is not a literal"

tryTuringFunction :: Expr -> Either String Turing.TransitionFunction
tryTuringFunction (Function (Tuple [a, b], Tuple [c, d, e])) = do
  inputState <- tryState a
  inputSymbol <- trySymbol b
  outputState <- tryState c
  outputSymbol <- trySymbol d
  outputShift <- tryShift e
  return ((inputState, inputSymbol), (outputState, outputSymbol, outputShift))
tryTuringFunction _ = Left "Invalid argument: functions contains a non-function type"
