# Models of Computation

Having fun with different models!!

## Supported Models

- Finite Automaton
- Turing machine
- Lambda Calculus

## Run

### REPL Mode
```bash
moc
```

### Lambda Calculus Mode
```bash
moc lambda [cbv] [all]
```

## Usage

### REPL Mode

In REPL mode there are two types of statements, assignments, and expressions.

Assignments have the following structure.

```
<name> := <expression>
```

They essentially bind an expression to a name, and whenever that name is used
later on, it is replaced with that expression.

There are many different types of expressions, there are sets, tuples, functions
literals and calls.

#### Sets

Sets are denoted as the following:

```
{ <expression_a>, <expression_b>, ... }
```

The order of sets do not matter, the position of each element has no relevance,
additionally, there are no duplicate items within sets.

#### Tuple

Tuples are denoted as the following:

```
( <expression_a>, <expression_b>, ... )
```

The order of tuples do matter, you may also have duplicate elements within a tuple.

#### Functions

Functions are denoted as the following:

```
\<expression_a> -> <expression_b>
```

These are not traditional functions, they are the functions that are used to
define transitions within Finite Automaton and Turing Machines.

#### Literals and Strings

Literals are any collection of alphanumeric symbols. An example is `abc`.

Strings use quotations, for example `"abc"`, this enables strings to contain
symbols that are not allowed within literals.

#### Call

A call is denoted as the following:

```
<expression_a> <expression_b>
```

#### How to Define Machines

Using the expressions above we can define different types of machines to for
different expressions.

##### Deterministic Finite Automaton

You can define a DFA by calling the DFA literal with a 5 tuple using the following
format.

```
DFA (States, Alphabet, TransitionFunctions, InitialState, FinalStates)
```

where:
- `States` is a set of States (literal or strings, they may be 1 or more characters long)
- `Alphabet` is a set of Symbols (literal or strings, they may be only 1 character long)
- `TransitionFunctions` is a set of functions that take a tuple of a State and
  Symbol and returns a State
- `InitialState` is a single State
- `FinalState` is a set of States.


Example:
```
states := {q0,qi}
alphabet := {a,b}
transitionFunctions := { \(q0,a) -> q0, \(q0,b) -> qi, \(qi,a) -> qi, \(qi,b) -> qi }
initialState := q0
finalStates := {qi}
dfa := DFA (states, alphabet, transitionFunctions, initialState, finalStates)
```

##### Non-Deterministic Finite Automaton

You can define an NFA by calling the NFA literal with a 5 tuple using the following
format.

```
NFA (States, Alphabet, TransitionFunctions, InitialState, FinalStates)
```

where:
- `States` is a set of States (literal or string, they may be 1 or more characters long)
- `Alphabet` is a set of Symbols (literal or string, they may be only 1 character long)
- `TransitionFunctions` is a set of functions that take a tuple of a State and
  Symbol and returns a set of State
- `InitialState` is a single State
- `FinalState` is a set of States.


Example:
```
states := {q0,qi}
alphabet := {a,b}
transitionFunctions := { \(q0,a) -> {q0}, \(q0,b) -> {qi}, \(qi,a) -> {qi}, \(qi,b) -> {qi} }
initialState := q0
finalStates := {qi}
nfa := NFA (states, alphabet, transitionFunctions, initialState, finalStates)
```

##### Turing Machines

You can define an Turing Machine by calling the Turing literal with a 7 tuple using the following
format.

```
Turing (States, TapeAlphabet, Blank, InputAlphabet, TransitionFunctions, InitialState, FinalStates)
```

where:
- `States` is a set of States (literal or string, they may be 1 or more characters long)
- `TapeAlphabet` is a set of Symbols (literal or string, they may be only 1 character long)
- `Blank` is a Symbol
- `InputAlphabet` is a set of Symbols
- `TransitionFunctions` is a set of functions that take a tuple of a State and
  Symbol and returns a tuple of a State, Symbol, and a Shift (either `L` or `R`)
- `InitialState` is a single State
- `FinalState` is a set of States.

Example:
```
states := { b, c, e, f }
tapeAlphabet := { ".", 0, 1 }
inputAlphabet := {}
transitionFunctions := { \(b, ".") -> (c, 0, R), \(c, ".") -> (e, ".", R), \(e, ".") -> (f, 1, R) }
initialState := b
finalStates := { f }
blank := "."
tur := Turing (states, tapeAlphabet, blank, inputAlphabet, transitionFunctions, initialState, finalStates)
```

#### Running Machines

To run a particular machine, use the call expression with the machine and the
input string.

Example:
```
λ> Turing ({ b, c, e, f }, { ., 0, 1 }, ., {  },  { \( b,. ) -> ( c, 0, L ), \( c,. ) -> ( e, ., L ), \( e,. ) -> ( f, 1, L ) }, b, { f } ) ""
( Success, "1.0", ( ( "", b, 0 ), ( "0", c, -1 ), ( ".0", e, -2 ), ( "1.0", f, -3 ) ) )
```

The output of these machines depend on the type, Finite Automaton return either
a Success or Failure literal. A Turing machine in contrast returns a triple
containing the Success or Failure literal, the final tape after execution and a
tuple of steps the machine had to take.

### Lambda Calculus Model

For now, lambda calculus is in a distinct mode, you can get into this mode using
`moc lambda`

Similar to REPL, there are only two types of statements for REPL, assignments
and expressions.

Assignments have the same structure as the REPL mode, however, the name must
begin with a capital letter.

Lambda calculus is defined using the standard notation, one character per
variable. This means that expressions such as `\abc.a b c` are evaluated as 
`λa.λb.λc.a b c`.

Within lambda expressions, variables that have been defined are replaced
directly,

Example:
```
λ> Hi := b
Inserted b for Hi

λ> \a.Hi a

Evaluating λa.b a
λa.b a
```

Numbers are also defined in Lambda Calculus using Church Encoding.

```
λ> 10

Evaluating λf.λx.f (f (f (f (f (f (f (f (f (f x)))))))))
λf.λx.f (f (f (f (f (f (f (f (f (f x)))))))))
Also known as value 10
```

Math operations use native lambda calculus and hence use polish notation.

```
λ> + 3 2

Evaluating (λp.λq.λf.λx.p f (q f x)) (λf.λx.f (f (f x))) (λf.λx.f (f x))
λf.λx.f (f (f (f (f x))))
Also known as value 5
```

Additionally, we have provided a set of base assignments that can be used to
define more complex programs. A list of the symbols and their corresponding
lambda calculus definition has been provided below.

| Name     | Lambda Calculus                                         |
| -------- |---------------------------------------------------------|
| True     | \\xy.x                                                  |
| False    | \\xy.y                                                  |
| If       | \\bxy.b x y                                             |
| And      | \\xy.If x y False                                       |
| Or       | \\xy.If x True y                                        |
| Const    | True                                                    |   
| For      | \\nf.n f                                                |
| Y        | \\f.(\\x.f (x x)) (\\x.f (x x))                         |
| O        | (\\xy.y (\\z.x x y z)) (\\xy.y (\\z.x x y z))           |
| Z        | \\f.(\\x.f (\\v.x x v)) (\\x.f (\\v.x x v))             |
| Succ     | \\nfx.f (n f x)                                         |
| Pred     | λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)                |
| +        | (\\pqfx.(p f (q f x)))                                  |
| *        | \\mnf.m (n f)                                           |
| IsZero   | \\x.For x (Const False) True                            |
| -        | \\mn.For n (Pred) m                                     |
| LEQ      | \\mn.IsZero (- m n)                                     |
| Fact     | \\fx.If (IsZero x) 1 (* x (f (Pred x)))                 |
| FactZ    | \\fx.(If (IsZero x) (\\a.1) (\\b.(* x (f (Pred x))))) a |
| Pair     | \\xyf.f x y                                             |
| Fst      | \\p.p True                                              |
| Snd      | \\p.p False                                             |
| Test     | (YCom) (\\r.r)                                          |

### Call-by-value

If you want to run Lambda Calculus using Call-by-value reduction use the following command
```bash
moc lambda cbv
```

### Print all steps

If you want to print all the reduction steps used to get the final expression,
use the following command
```bash
moc lambda all
```

## How to Build

```bash
cabal build
```

