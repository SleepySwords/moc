{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (runState)
import Data.Map (empty, insert)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import ModelComputation.FiniteStateAutomota.DFA (runDFA)
import ModelComputation.FiniteStateAutomota.NFA (translateNFA)
import qualified ModelComputation.FiniteStateAutomota.NFA as NFA
import ModelComputation.FiniteStateAutomota.Parser (parseDetermisticAutomota, parseNondetermisticAutomota)
import ModelComputation.LambdaCalculus.Command
import ModelComputation.LambdaCalculus.Parser (newSymbolTable)
import ModelComputation.LambdaCalculus.Reduction (bReduceCBV, bReduceNormal)
import ModelComputation.TuringMachine.Parser (parseTuringMachine)
import ModelComputation.TuringMachine.Turing (isValid, printState, runMachine, verifyMachine)
import Repl.Parser (parseStatement)
import Repl.Repl (evaluateStatement)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = getArgs >>= parseArgument

runLambdaMode :: [String] -> IO ()
runLambdaMode args = do

  runInputT defaultSettings (replCommand runAllSteps lambdaReduce symbols)
  where
    runAllSteps = "all" `elem` args
    lambdaReduce = if "cbv" `elem` args then bReduceCBV else bReduceNormal
    -- run = either (outputStrLn . show) (evaluateLambda (lambdaReduceGreedyMemo Map.empty)) . parseLambda defaultSymbolTable
    -- run = either (outputStrLn . show) (evaluateLambda lambdaReduceGreedy) . parseLambda defaultSymbolTable
    -- runNormalise = either (outputStrLn . errorBundlePretty) (outputStrLn . show . normalisation) . parseLambda symbols
    run = either (outputStrLn . errorBundlePretty) (evaluateLambda False bReduceNormal) . parseLambda symbols
    symbols = foldl foldF Data.Map.empty newSymbolTable
    foldF s (k, l) = either (const s) (\v -> Data.Map.insert k v s) (parseLambda s l)

outprint :: (MonadIO m, Show s) => s -> InputT m ()
outprint = outputStrLn . show

runTuringMode :: String -> IO ()
runTuringMode filename = do
  turingText <- readFile filename

  case parse parseTuringMachine filename (pack turingText) of
    Right turing ->
      runInputT
        defaultSettings
        ( do
            outputStrLn "Parsed turing machine:"
            outputStrLn ""
            outprint turing
            outputStrLn ""
            outprint (verifyMachine turing)
            toEval <- getInputLine "Test Str> "
            let output = runMachine turing (fromMaybe "" toEval)
            mapM_ (outputStrLn . printState turing) output
            outputStrLn (if isValid turing output then "Accepted" else "Rejected")
        )
    Left err -> do
      putStrLn (errorBundlePretty err)

-- mapM_ (putStrLn . printState tm) (runMachine tm "")

runDFAMode :: String -> IO ()
runDFAMode filename = do
  dfaText <- readFile filename

  case parse parseDetermisticAutomota filename (pack dfaText) of
    Right dfa -> do
      putStrLn "Parsed deterministic finite state automota:"
      putStrLn ""
      print dfa
      putStrLn ""
      runInputT defaultSettings (runTest dfa)
    Left err -> do
      putStrLn (errorBundlePretty err)
  where
    runTest dfa = do
      toEval <- getInputLine "Test Str> "
      maybe (outputStrLn "") (outputStrLn . show . runDFA dfa) toEval
      runTest dfa

runRepl :: [String] -> IO ()
runRepl _ = do
  runInputT defaultSettings (rep empty)
  where
    -- rep :: Map String Expr -> InputT m b
    rep st = do
      toEval <- getInputLine "λ> "

      forM_
        toEval
        ( \a ->
            case parse parseStatement "REPL" (pack a) of
              Left l -> outputStrLn (errorBundlePretty l) >> rep st
              Right r -> let (x, y) = runState (evaluateStatement r) st in outputStrLn x >> rep y
        )

runNFAMode :: String -> IO ()
runNFAMode filename = do
  dfaText <- readFile filename

  case parse parseNondetermisticAutomota filename (pack dfaText) of
    Right nfa -> do
      putStrLn "Parsed nondeterministic finite state automota:"
      putStrLn ""
      print nfa
      putStrLn ""
      runInputT defaultSettings (runTest nfa)
    Left err -> do
      putStrLn (errorBundlePretty err)
  where
    runTest nfa = do
      toEval <- getInputLine "Test Str> "
      maybe (outputStrLn "") (outputStrLn . show . NFA.runNFA nfa) toEval
      runTest nfa

runNFATranslateMode :: String -> IO ()
runNFATranslateMode filename = do
  dfaText <- readFile filename

  case parse parseNondetermisticAutomota filename (pack dfaText) of
    Right nfa -> do
      let x = translateNFA nfa
      print x
    Left err -> do
      putStrLn (errorBundlePretty err)

parseArgument :: [String] -> IO ()
parseArgument ("lambda" : x) = runLambdaMode x
parseArgument ["turing"] = runTuringMode ""
parseArgument ["turing", a] = runTuringMode a
parseArgument ["dfa", a] = runDFAMode a
parseArgument ["nfa", a] = runNFAMode a
parseArgument ("repl" : a) = runRepl a
parseArgument ["translate_nfa", a] = runNFATranslateMode a
parseArgument x = runRepl x
