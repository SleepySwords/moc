module Repl.Types where

data Expr = Literal String | Array [Expr] | Tuple [Expr] | Function (Expr, Expr) | Call Expr Expr deriving (Show)
data Statement = Assignment String Expr | Expression Expr deriving (Show)
