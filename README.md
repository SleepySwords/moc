# Memories of Chaos (Models of Computation)

Having fun with different models!!

## The models

- Turing machine
    - Does not have a parser currently, so you must edit the code directly
- Lambda Calculus
    - Lambda calculus supports variables, the format is `Name := Expression`
      where Name must start with a capital and Expression is a lambda expression.
      Whenever Name appears in a lambda expression it is replaced with Expression.

## Run

Via Cabal
```
cabal run -- <lambda | turing | dfa | nfa>
```
