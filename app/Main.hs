{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Set (fromList)
import ModelComputation.LambdaCalculus.Command
import ModelComputation.LambdaCalculus.Parser (defaultSymbolTable, lambdaParser)
import ModelComputation.LambdaCalculus.Types (integerToChurchEncoding)
import ModelComputation.Turing (Shift (LeftShift, RightShift), TuringMachine (..), printState, runMachine)
import System.Console.Haskeline (defaultSettings, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), parseTest)

main :: IO ()
main = getArgs >>= parseArgument

parseArgument :: [String] -> IO ()
parseArgument ["lambda"] = do
  let symbols = defaultSymbolTable
  parseTest (lambdaParser symbols <* eof) "\\x. x \\a.x \\y.y"
  parseTest (lambdaParser symbols <* eof) "(\\x. (\\a.x \\y.y) x) a"
  evaluateLambda symbols "(\\x. x \\x.x) a"
  parseTest (lambdaParser symbols <* eof) "\\xy.x"
  parseTest (lambdaParser symbols <* eof) "\\xy.x a v"
  parseTest (lambdaParser symbols <* eof) "λf.λx.f (f (f x))"
  evaluateLambda symbols "(λp.λa.λb.p b a) λx.λy.y"
  evaluateLambda symbols "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"

  evaluateLambda symbols "(λpq.p q p) (λxy.y) (λxy.y)"
  evaluateLambda symbols "(λx.(\\x.x) x)"
  evaluateLambda symbols "(λy.(\\x.y)) x"
  evaluateLambda symbols "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"
  evaluateLambda symbols "(λm.λn.λf.λx.m f (n f x)) 3 12"
  evaluateLambda symbols "(λxyz.x y z) (λx.x x) (λx.x) x"
  evaluateLambda symbols "(\\bxy.b x y) True 1 0"
  evaluateLambda symbols "IfThen False 1 100"
  evaluateLambda symbols "(\\xy.x y) y"
  evaluateLambda symbols "10"

  print $ integerToChurchEncoding 3

  runInputT defaultSettings (replCommand symbols)
parseArgument ["turing"] = do
  let tm =
        TuringMachine
          { states = fromList ["b", "c", "e", "f"],
            tapeAlphabet = fromList ['.'],
            blank = '.',
            inputSymbols = fromList [],
            transitionFunction =
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
            transitionFunction =
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
