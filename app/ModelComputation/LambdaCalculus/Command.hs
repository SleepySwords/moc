{-# LANGUAGE NamedFieldPuns #-}
module ModelComputation.LambdaCalculus.Command where
import ModelComputation.LambdaCalculus.Parser (SymbolTable, lambdaParser, parseCommand)
import System.Console.Haskeline
import ModelComputation.LambdaCalculus.Types (Expr (..), ReduceInfo (..), churchEncodingToInteger)
import Control.Monad (forM_)
import Text.Megaparsec (parse, MonadParsec (eof))
import Data.Text (pack)
import ModelComputation.LambdaCalculus.Reduction
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)
import System.Console.ANSI (getTerminalSize, setCursorColumn)
import Data.Map (insert)

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

executeCommand :: SymbolTable -> Either (Expr ()) (String, Expr ()) -> (SymbolTable, InputT IO ())
executeCommand symbolTable (Left expression) = (symbolTable, cmd)
  where
    cmd = do
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
executeCommand symbolTable (Right (command, expr)) = (insert command expr symbolTable, outputStrLn ("Inserted " ++ show expr ++ " for " ++ command))

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

replCommand :: SymbolTable -> InputT IO ()
replCommand symbolT = do
  toEval <- getInputLine "λ> "
  let (symbolTable, printMonad) = case toEval of
        Just s -> case parse (parseCommand symbolT <* eof) "Failed" (pack s) of
          Right expression -> executeCommand symbolT expression
          Left err -> (symbolT, outputStrLn (show err))
        Nothing -> (symbolT, return ())
  printMonad

  outputStrLn ""
  replCommand symbolTable
