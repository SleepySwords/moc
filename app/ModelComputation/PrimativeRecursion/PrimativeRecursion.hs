{-# LANGUAGE OverloadedStrings #-}

module ModelComputation.PrimativeRecursion.PrimativeRecursion where

import ModelComputation.PrimativeRecursion.Parser (function)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

primativeRecursion :: IO ()
primativeRecursion = do
  case parse function "test" "add(a, b, c, b+1)" of
    Right nfa -> do
      putStrLn "Parsed nondeterministic finite state automota:"
      putStrLn ""
      print nfa
      putStrLn ""
    Left err -> do
      putStrLn (errorBundlePretty err)
