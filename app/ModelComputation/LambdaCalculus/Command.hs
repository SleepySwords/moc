{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module ModelComputation.LambdaCalculus.Command where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity
import Control.Monad.State.Strict (put, runState)
import Data.Function (on)
import Data.Functor (void)
import Data.List (intercalate, maximumBy)
import Data.Map (insert)
import Data.Text (Text, pack)
import Data.Void (Void)
import ModelComputation.LambdaCalculus.Parser (SymbolTable, lambdaParser, parseCommand)
import ModelComputation.LambdaCalculus.Reduction
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..), churchEncodingToBool, churchEncodingToInteger)
import System.Console.Haskeline
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, parse)
import Text.Megaparsec.Error (ParseErrorBundle)

parseLambda :: SymbolTable -> String -> Either (ParseErrorBundle Text Void) (Expr ())
parseLambda s = parse (lambdaParser s <* eof) "Failed" . pack

maxDepths :: Expr a -> Int
maxDepths Var {} = 0
maxDepths App {function, input} = max (maxDepths function) (maxDepths input) + 1
maxDepths Abs {body} = maxDepths body + 1

evaluateLambda :: Bool -> (Expr ReduceInfo -> Maybe DebugExpr) -> Expr () -> InputT IO ()
evaluateLambda showAllSteps reduceFunction expression = do
  outputStrLn ""
  outputStrLn ("Evaluating \x1b[35m" ++ show expression ++ "\x1b[0m")
  -- lambdaReduceM (noSub <$ expression) (return ()) printLambdaWithContext
  let steps =
        runIdentity $
            lambdaReduceM
              (noSub <$ expression)
              reduceFunction
              ( return . fst
              )

  -- let steps = reduceFunction (noSub <$ expression)
  -- mapM_ printLambdaWithContext $ if showAllSteps then steps else [last steps]
  --
  -- outputStrLn $ "who" ++ show (maximum (map (maxDepths . fst) steps))

  case churchEncodingToInteger steps of
    Just v -> outputStrLn ("Also known as value " ++ show v)
    Nothing -> return ()
  case churchEncodingToBool steps of
    Just v -> outputStrLn ("Also known as value " ++ show v)
    Nothing -> return ()

printLambdaWithContext :: DebugExpr -> InputT IO ()
printLambdaWithContext (a, DebugInfo substitutions) = do
  outputStrLn (show a)
  liftIO $ do
    let subMsg = intercalate "\n" $ map (\(ch, ex) -> "          " ++ [ch] ++ ":=" ++ show (void ex)) substitutions
    putStrLn subMsg

executeCommand :: Bool -> (Expr ReduceInfo -> Maybe DebugExpr) -> SymbolTable -> Either (Expr ()) (String, Expr ()) -> (SymbolTable, InputT IO ())
executeCommand showAllSteps reduceAlgorithm symbolTable (Left expression) = (symbolTable, evaluateLambda showAllSteps reduceAlgorithm expression)
executeCommand _ _ symbolTable (Right (command, expr)) = (insert command expr symbolTable, outputStrLn ("Inserted " ++ show expr ++ " for " ++ command))

repl :: SymbolTable -> Bool -> InputT IO ()
repl s showAllSteps = do
  toEval <- getInputLine "λ> "
  forM_ toEval (either (outputStrLn . show) (evaluateLambda showAllSteps bReduceNormal) . parseLambda s)
  outputStrLn ""
  repl s showAllSteps

replCommand :: Bool -> (Expr ReduceInfo -> Maybe DebugExpr) -> SymbolTable -> InputT IO ()
replCommand showAllSteps reduceAlgorithm symbolT = do
  toEval <- getInputLine "λ> "
  let (symbolTable, printMonad) = case toEval of
        Just s -> case parse (parseCommand symbolT <* eof) "REPL" (pack s) of
          Right expression -> executeCommand showAllSteps reduceAlgorithm symbolT expression
          Left err -> (symbolT, outputStrLn (errorBundlePretty err))
        Nothing -> (symbolT, return ())
  printMonad

  outputStrLn ""
  replCommand showAllSteps reduceAlgorithm symbolTable
