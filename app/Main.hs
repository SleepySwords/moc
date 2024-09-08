{-# LANGUAGE OverloadedStrings #-}

module Main where

import ModelComputation.LambdaCalculus (bconversion, lambdaParser, steps_to_reduce)
import Text.Megaparsec (MonadParsec (eof), parse, parseTest)
import Data.Text (pack)

main :: IO ()
main = do
  parseTest (lambdaParser <* eof) "\\x. x \\a.x \\y.y"
  parseTest (lambdaParser <* eof) "(\\x. (\\a.x \\y.y) x) a"
  let Right test = parse (lambdaParser <* eof) "Failed" "(\\x. x \\x.x) a"
  print $ bconversion test
  parseTest (lambdaParser <* eof) "\\xy.x"
  parseTest (lambdaParser <* eof) "\\xy.x a v"
  parseTest (lambdaParser <* eof) "λf.λx.f (f (f x))"
  let Right yes = parse (lambdaParser <* eof) "Failed" "(λp.λa.λb.p b a) λx.λy.y"
  print $ bconversion $ bconversion $ bconversion yes
  let Right wow = parse (lambdaParser <* eof) "Failed" "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"
  print wow

  let Right wow = parse (lambdaParser <* eof) "Failed" "(λpq.p q p) (λxy.y) (λxy.y)"
  print wow
  print $ bconversion $ bconversion $ bconversion $ bconversion wow

  let Right wow = parse (lambdaParser <* eof) "Failed" "(λx.(\\x.x) x)"
  print wow
  print $ bconversion $ bconversion $ bconversion $ bconversion wow

  let Right wow = parse (lambdaParser <* eof) "Failed" "(λy.(\\x.y)) x"
  print wow
  print $ bconversion wow

  let Right wow = parse (lambdaParser <* eof) "Failed" "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))"
  mapM_ print (steps_to_reduce wow)

  let Right wow = parse (lambdaParser <* eof) "Failed" "(λxyz.x y z) (λx.x x) (λx.x) x"
  putStrLn ("Evaluating \x1b[32m" ++ show wow ++ "\x1b[0m")
  mapM_ print (steps_to_reduce wow)


evaluateLambda :: String -> IO ()
evaluateLambda x = case parse (lambdaParser <* eof) "Failed" (pack x) of
                  Right expression -> do
                    putStrLn ("Evaluating \x1b[32m" ++ show expression ++ "\x1b[0m")
                    mapM_ print (steps_to_reduce expression)
                  Left err -> print err

