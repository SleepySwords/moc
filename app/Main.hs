{-# LANGUAGE OverloadedStrings #-}

module Main where

import ModelComputation.LambdaCalculus (lambdaParser, steps_to_reduce, integerToChurchEncoding, churchEncodingToInteger, defaultSymbolTable, SymbolTable)
import Text.Megaparsec (MonadParsec (eof), parse, parseTest)
import Data.Text (pack)

main :: IO ()
main = do
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

evaluateLambda :: SymbolTable -> String -> IO ()
evaluateLambda s x = case parse (lambdaParser s <* eof) "Failed" (pack x) of
                  Right expression -> do
                    putStrLn ""
                    putStrLn ("Evaluating \x1b[32m" ++ show expression ++ "\x1b[0m")
                    let steps = steps_to_reduce expression
                    mapM_ print steps
                    case churchEncodingToInteger (last steps) of
                      Just v -> putStrLn ("Also known as value " ++ show v)
                      Nothing -> return ()
                  Left err -> print err
