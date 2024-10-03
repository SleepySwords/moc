{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Text (pack)
import System.Console.ANSI (getTerminalSize, setCursorColumn)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, outputStrLn, runInputT)
import Text.Megaparsec (MonadParsec (eof), parse, parseTest)
import ModelComputation.LambdaCalculus.Parser (SymbolTable, lambdaParser, defaultSymbolTable, parseCommand)
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..), churchEncodingToInteger, integerToChurchEncoding)
import ModelComputation.LambdaCalculus.Reduction (notSubstituted, lambdaReduce, lambdaReduceFull)

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

  runInputT defaultSettings (repl symbols)

evaluateLambda :: SymbolTable -> String -> IO ()
evaluateLambda s x = case parse (lambdaParser s <* eof) "Failed" (pack x) of
  Right expression -> do
    putStrLn ""
    putStrLn ("Evaluating \x1b[35m" ++ show expression ++ "\x1b[0m")
    let steps = lambdaReduce (notSubstituted <$ expression)
    mapM_
      ( \a -> do
          print a
          putStrLn ""
      )
      steps

    case churchEncodingToInteger (last steps) of
      Just v -> putStrLn ("Also known as value " ++ show v)
      Nothing -> return ()
  Left err -> print err

evaluateLambdaFull :: SymbolTable -> String -> InputT IO ()
evaluateLambdaFull s x = case parse (lambdaParser s <* eof) "Failed" (pack x) of
  Right expression -> do
    outputStrLn ""
    outputStrLn ("Evaluating \x1b[35m" ++ show expression ++ "\x1b[0m")
    let steps = lambdaReduceFull (notSubstituted <$ expression)
    mapM_
      ( \a -> do
          outputStr (show a)
          liftIO $ do
            let subMsg = case map (\(ch, ex) -> "Substituted " ++ [ch] ++ ":=" ++ show (void ex)) (findSubstituted a) of
                  msg : _ -> msg
                  _ -> []

            Just (_, width) <- getTerminalSize
            if length (show (void a)) + length subMsg + 30 > width
              then
                putStrLn ""
              else do
                setCursorColumn (max 0 (min (width - length subMsg) (width - 50)))
                putStrLn subMsg
      )
      steps

    case churchEncodingToInteger (last steps) of
      Just v -> outputStrLn ("Also known as value " ++ show v)
      Nothing -> return ()
  Left err -> outputStrLn (show err)

findSubstituted :: Expr ReduceInfo -> [(Char, Expr ReduceInfo)]
findSubstituted s@Var {info = ReduceInfo {substituted = Just x}} = [(x, s)]
findSubstituted Var {info = ReduceInfo {substituted = _}} = []
findSubstituted hi = case substituted (info hi) of
  Just o -> [(o, hi)]
  Nothing -> case hi of
    Abs {body = b} -> findSubstituted b
    App {function, input} -> findSubstituted function ++ findSubstituted input

repl :: SymbolTable -> InputT IO ()
repl s = do
  toEval <- getInputLine "λ> "
  forM_ toEval (evaluateLambdaFull s)
  outputStrLn ""
  repl s
