{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Set (fromList)
import ModelComputation.LambdaCalculus.Command
import ModelComputation.LambdaCalculus.Parser (defaultSymbolTable, lambdaParser)
import ModelComputation.LambdaCalculus.Reduction (lambdaReduceGreedy)
import ModelComputation.LambdaCalculus.Types (integerToChurchEncoding)
import ModelComputation.Turing (Shift (LeftShift, RightShift), TuringMachine (..), printState, runMachine)
import System.Console.Haskeline (defaultSettings, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), parseTest)

main :: IO ()
main = getArgs >>= parseArgument

parseArgument :: [String] -> IO ()
parseArgument ["lambda"] = do
  let symbols = defaultSymbolTable
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
    )

  print $ integerToChurchEncoding 3

  runInputT defaultSettings (replCommand symbols)
  where
    run = either (outputStrLn . show) (evaluateLambda lambdaReduceGreedy) . parseLambda defaultSymbolTable
parseArgument ["turing"] = do
  let tm =
        TuringMachine
          { states = fromList ["b", "c", "e", "f"],
            tapeAlphabet = fromList ['.'],
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
          { states = fromList ["q0", "q1", "q2"],
            tapeAlphabet = fromList ['.', '0', 'c'],
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

  mapM_ (putStrLn . printState addition) (runMachine addition "0c00")
parseArgument _ = putStrLn "Unknown mode: Use either lambda or turing"
