{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.Map (empty, insert)
import Data.Text (pack)
import ModelComputation.FiniteStateAutomota.DFA (runDFA)
import ModelComputation.FiniteStateAutomota.NFA (translateNFA)
import qualified ModelComputation.FiniteStateAutomota.NFA as NFA
import ModelComputation.FiniteStateAutomota.Parser (parseDetermisticAutomota, parseNondetermisticAutomota)
import ModelComputation.LambdaCalculus.Command
import ModelComputation.LambdaCalculus.Parser (lambdaParser, newSymbolTable)
import ModelComputation.LambdaCalculus.Reduction (lambdaReduceCBV, lambdaReduceNormal, normalisation)
import ModelComputation.LambdaCalculus.Types (integerToChurchEncoding)
import ModelComputation.TuringMachine.Parser (parseTuringMachine)
import ModelComputation.TuringMachine.Turing (isValid, printState, runMachine, verifyMachine)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, parse, parseTest)
import Data.Maybe (fromMaybe)

main :: IO ()
main = getArgs >>= parseArgument

runLambdaMode :: String -> IO ()
runLambdaMode a = do
  parseTest (lambdaParser symbols <* eof) "\\x. x \\a.x \\y.y"
  parseTest (lambdaParser symbols <* eof) "(\\x. (\\a.x \\y.y) x) a"
  parseTest (lambdaParser symbols <* eof) "\\xy.x"
  parseTest (lambdaParser symbols <* eof) "\\xy.x a v"
  parseTest (lambdaParser symbols <* eof) "λf.λx.f (f (f x))"
  runInputT
    defaultSettings
    ( do
        run "(\\x. x \\x.x) a"
        run "(λp.λa.λb.p b a) λx.λy.y"
        run "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"

        run "(λpq.p q p) (λxy.y) (λxy.y)"
        run "(λx.(\\x.x) x)"
        run "(λy.(\\x.y)) x"
        run "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"
        run "(λm.λn.λf.λx.m f (n f x)) 3 12"
        run "(λxyz.x y z) (λx.x x) (λx.x) x"
        run "(\\bxy.b x y) True 1 0"
        run "If False 1 100"
        run "(\\xy.x y) y"
        run "- 10 3"
        run "+ 10 3"
        run "10"
        runNormalise "\\x.\\y.x y"
        runNormalise "\\b.\\a.b a"
    )

  print $ integerToChurchEncoding 3

  runInputT defaultSettings (replCommand (if a == "cbv" then lambdaReduceCBV else lambdaReduceNormal) symbols)
  where
    -- run = either (outputStrLn . show) (evaluateLambda (lambdaReduceGreedyMemo Map.empty)) . parseLambda defaultSymbolTable
    -- run = either (outputStrLn . show) (evaluateLambda lambdaReduceGreedy) . parseLambda defaultSymbolTable
    runNormalise = either (outputStrLn . errorBundlePretty) (outputStrLn . show . normalisation) . parseLambda symbols
    run = either (outputStrLn . errorBundlePretty) (evaluateLambda lambdaReduceNormal) . parseLambda symbols
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
parseArgument ["lambda", a] = runLambdaMode a
parseArgument ["lambda"] = runLambdaMode ""
parseArgument ["turing"] = runTuringMode ""
parseArgument ["turing", a] = runTuringMode a
parseArgument ["dfa", a] = runDFAMode a
parseArgument ["nfa", a] = runNFAMode a
parseArgument ["translate_nfa", a] = runNFATranslateMode a
parseArgument _ = putStrLn "Unknown mode: Use either lambda, turing, dfa or nfa"
