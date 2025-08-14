{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map (empty, insert)
import Data.Set (fromList)
import Data.Text (pack)
import ModelComputation.FiniteStateAutomota.DFA (runDFA)
import qualified ModelComputation.FiniteStateAutomota.NFA as NFA
import ModelComputation.FiniteStateAutomota.Parser (parseAutomota)
import ModelComputation.LambdaCalculus.Command
import ModelComputation.LambdaCalculus.Parser (lambdaParser, newSymbolTable)
import ModelComputation.LambdaCalculus.Reduction (lambdaReduceCBV, lambdaReduceNormal, normalisation)
import ModelComputation.LambdaCalculus.Types (integerToChurchEncoding)
import ModelComputation.Turing (Shift (LeftShift, RightShift), TuringMachine (..), printState, runMachine, verifyMachine)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof),  parse, parseTest)

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
    runNormalise = either (outputStrLn . show) (outputStrLn . show . normalisation) . parseLambda symbols
    run = either (outputStrLn . show) (evaluateLambda lambdaReduceNormal) . parseLambda symbols
    symbols = foldl foldF Data.Map.empty newSymbolTable
    foldF s (k, l) = either (const s) (\v -> Data.Map.insert k v s) (parseLambda s l)

runTuringMode :: IO ()
runTuringMode = do
  let tm =
        TuringMachine
          { states = fromList ["b", "c", "e", "f"],
            tapeAlphabet = fromList ['.', '0', '1'],
            blank = '.',
            inputSymbols = fromList [],
            transitionFunctions =
              [ (("b", '.'), ("c", '0', RightShift)),
                (("c", '.'), ("e", '.', RightShift)),
                (("e", '.'), ("f", '1', RightShift)),
                (("f", '.'), ("b", '.', RightShift))
              ],
            initialState = "b",
            finalStates = fromList ["f"]
          }
  let addition =
        TuringMachine
          { states = fromList ["q0", "q1", "q2", "q3"],
            tapeAlphabet = fromList ['.', '0', 'c', 'X'],
            blank = '.',
            inputSymbols = fromList [],
            transitionFunctions =
              [ (("q0", '0'), ("q1", 'X', RightShift)),
                (("q0", 'c'), ("q3", '.', RightShift)),
                (("q1", '0'), ("q1", '0', RightShift)),
                (("q1", 'c'), ("q1", 'c', RightShift)),
                (("q1", '.'), ("q2", '0', LeftShift)),
                (("q2", '0'), ("q2", '0', LeftShift)),
                (("q2", 'c'), ("q2", 'c', LeftShift)),
                (("q2", '0'), ("q2", '0', LeftShift)),
                (("q2", 'X'), ("q0", '.', RightShift))
              ],
            initialState = "q0",
            finalStates = fromList ["q3"]
          }
  print (verifyMachine tm)
  print (verifyMachine addition)

  mapM_ (putStrLn . printState tm) (runMachine tm "")

runDFAMode :: String -> IO ()
runDFAMode filename = do
  dfaText <- readFile filename

  case parse parseAutomota "Failed" (pack dfaText) of
    Right dfa -> do
      putStrLn "Parsed deterministic finite state automota:"
      putStrLn ""
      print dfa
      putStrLn ""
      runInputT defaultSettings (runTest dfa)
    Left err -> do
      print (show err)
  where
    runTest dfa = do
      toEval <- getInputLine "Test Str> "
      maybe (outputStrLn "") (outputStrLn . show . runDFA dfa) toEval
      runTest dfa

runNFAMode :: IO ()
runNFAMode = do
  let nfa =
        NFA.NondetermenistFiniteAutomota
          { NFA.states = fromList ["q0", "q1", "q2", "q3", "q4"],
            NFA.alphabet = fromList ['a', 'b'],
            NFA.transitionFunctions =
              [ (("q0", 'a'), fromList ["q1", "q2"]),
                (("q1", 'a'), fromList ["q0"]),
                (("q2", 'a'), fromList ["q3"]),
                (("q3", 'b'), fromList ["q0"]),
                (("q0", 'b'), fromList ["q4"])
              ],
            NFA.initialState = "q0",
            NFA.finalStates = fromList ["q4"]
          }
  print $ NFA.runNFA nfa "aaaabb"

parseArgument :: [String] -> IO ()
parseArgument ["lambda", a] = runLambdaMode a
parseArgument ["lambda"] = runLambdaMode ""
parseArgument ["turing"] = runTuringMode
parseArgument ["dfa", a] = runDFAMode a
parseArgument ["nfa"] = runNFAMode
parseArgument _ = putStrLn "Unknown mode: Use either lambda, turing, dfa or nfa"
