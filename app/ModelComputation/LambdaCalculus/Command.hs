{-# LANGUAGE NamedFieldPuns #-}

module ModelComputation.LambdaCalculus.Command where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (void)
import Data.Map (insert)
import Data.Text (Text, pack)
import Data.Void (Void)
import ModelComputation.LambdaCalculus.Parser (SymbolTable, lambdaParser, parseCommand)
import ModelComputation.LambdaCalculus.Reduction
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..), churchEncodingToInteger, churchEncodingToBool)
import System.Console.ANSI (getTerminalSize, setCursorColumn)
import System.Console.Haskeline
import Text.Megaparsec (MonadParsec (eof), parse)
import Text.Megaparsec.Error (ParseErrorBundle)

parseLambda :: SymbolTable -> String -> Either (ParseErrorBundle Text Void) (Expr ())
parseLambda s = parse (lambdaParser s <* eof) "Failed" . pack

evaluateLambda :: (Expr ReduceInfo -> [Expr ReduceInfo]) -> Expr () -> InputT IO ()
evaluateLambda reduceFunction expression = do
  outputStrLn ""
  outputStrLn ("Evaluating \x1b[35m" ++ show expression ++ "\x1b[0m")
  let steps = reduceFunction (noSub <$ expression)
  -- mapM_ printLambdaWithContext steps
  printLambdaWithContext $ last steps

  case churchEncodingToInteger (last steps) of
    Just v -> outputStrLn ("Also known as value " ++ show v)
    Nothing -> return ()
  case churchEncodingToBool (last steps) of
    Just v -> outputStrLn ("Also known as value " ++ show v)
    Nothing -> return ()

printLambdaWithContext :: Expr ReduceInfo -> InputT IO ()
printLambdaWithContext a = do
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

executeCommand ::(Expr ReduceInfo -> [Expr ReduceInfo]) -> SymbolTable -> Either (Expr ()) (String, Expr ()) -> (SymbolTable, InputT IO ())
executeCommand reduceAlgorithm symbolTable (Left expression) = (symbolTable, evaluateLambda reduceAlgorithm expression)
executeCommand _ symbolTable (Right (command, expr)) = (insert command expr symbolTable, outputStrLn ("Inserted " ++ show expr ++ " for " ++ command))

findSubstituted :: Expr ReduceInfo -> [(Char, Expr ReduceInfo)]
findSubstituted s@Var {info = ReduceInfo {substituted = Just x}} = [(x, s)]
findSubstituted Var {info = ReduceInfo {substituted = _}} = []
findSubstituted s = case substituted (info s) of
  Just o -> [(o, s)]
  Nothing -> case s of
    Abs {body = b} -> findSubstituted b
    App {function, input} -> findSubstituted function ++ findSubstituted input

repl :: SymbolTable -> InputT IO ()
repl s = do
  toEval <- getInputLine "λ> "
  forM_ toEval (either (outputStrLn . show) (evaluateLambda lambdaReduceNormal) . parseLambda s)
  outputStrLn ""
  repl s

replCommand :: (Expr ReduceInfo -> [Expr ReduceInfo]) -> SymbolTable -> InputT IO ()
replCommand reduceAlgorithm symbolT = do
  toEval <- getInputLine "λ> "
  let (symbolTable, printMonad) = case toEval of
        Just s -> case parse (parseCommand symbolT <* eof) "Failed" (pack s) of
          Right expression -> executeCommand reduceAlgorithm symbolT expression
          Left err -> (symbolT, outputStrLn (show err))
        Nothing -> (symbolT, return ())
  printMonad

  outputStrLn ""
  replCommand reduceAlgorithm symbolTable
