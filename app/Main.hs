{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map (empty, insert)
import Data.Set (fromList)
import ModelComputation.LambdaCalculus.Command
import ModelComputation.LambdaCalculus.Parser (defaultSymbolTable, lambdaParser, newSymbolTable)
import ModelComputation.LambdaCalculus.Reduction (lambdaReduceCBV, lambdaReduceNormal, normalisation)
import ModelComputation.LambdaCalculus.Types (integerToChurchEncoding)
import ModelComputation.Turing (Shift (LeftShift, RightShift), TuringMachine (..), printState, runMachine, verifyMachine)
import System.Console.Haskeline (defaultSettings, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), parseTest)

main :: IO ()
main = getArgs >>= parseArgument

runLambdaMode :: String -> IO ()
runLambdaMode a = do
  let symbols = foldl foldF Data.Map.empty newSymbolTable
        where
          foldF s (k, l) = either (const s) (\v -> Data.Map.insert k v s) (parseLambda s l)
  -- let symbols = defaultSymbolTable
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
        run "IfThen False 1 100"
        run "(\\xy.x y) y"
        run "10"
        runNormalise "\\x.\\y.x y"
        runNormalise "\\b.\\a.b a"
    )

  print $ integerToChurchEncoding 3

  runInputT defaultSettings (replCommand (if a == "cbv" then lambdaReduceCBV else lambdaReduceNormal) symbols)
  where
    -- run = either (outputStrLn . show) (evaluateLambda (lambdaReduceGreedyMemo Map.empty)) . parseLambda defaultSymbolTable
    -- run = either (outputStrLn . show) (evaluateLambda lambdaReduceGreedy) . parseLambda defaultSymbolTable
    runNormalise = either (outputStrLn . show) (outputStrLn . show . normalisation) . parseLambda defaultSymbolTable
    run = either (outputStrLn . show) (evaluateLambda lambdaReduceNormal) . parseLambda defaultSymbolTable

parseArgument :: [String] -> IO ()
parseArgument ["lambda", a] = runLambdaMode a
parseArgument ["lambda"] = runLambdaMode ""
parseArgument ["turing"] = do
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
parseArgument _ = putStrLn "Unknown mode: Use either lambda or turing"
