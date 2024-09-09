{-# LANGUAGE OverloadedStrings #-}

module Main where

import ModelComputation.LambdaCalculus (lambdaParser, steps_to_reduce, integerToChurchEncoding)
import Text.Megaparsec (MonadParsec (eof), parse, parseTest)
import Data.Text (pack)

main :: IO ()
main = do
  parseTest (lambdaParser <* eof) "\\x. x \\a.x \\y.y"
  parseTest (lambdaParser <* eof) "(\\x. (\\a.x \\y.y) x) a"
  evaluateLambda "(\\x. x \\x.x) a"
  parseTest (lambdaParser <* eof) "\\xy.x"
  parseTest (lambdaParser <* eof) "\\xy.x a v"
  parseTest (lambdaParser <* eof) "λf.λx.f (f (f x))"
  evaluateLambda "(λp.λa.λb.p b a) λx.λy.y"
  evaluateLambda "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"

  evaluateLambda "(λpq.p q p) (λxy.y) (λxy.y)"
  evaluateLambda "(λx.(\\x.x) x)"
  evaluateLambda "(λy.(\\x.y)) x"
  evaluateLambda "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"
  evaluateLambda "(λxyz.x y z) (λx.x x) (λx.x) x"
  evaluateLambda "10"

  print $ integerToChurchEncoding 3

evaluateLambda :: String -> IO ()
evaluateLambda x = case parse (lambdaParser <* eof) "Failed" (pack x) of
                  Right expression -> do
                    putStrLn ""
                    putStrLn ("Evaluating \x1b[32m" ++ show expression ++ "\x1b[0m")
                    mapM_ print (steps_to_reduce expression)
                  Left err -> print err
