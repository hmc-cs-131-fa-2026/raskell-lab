# CS 131 Lab 9: Exploring Raskell

Starter code for Lab 9. The full instructions are on the course site: <https://www.cs.hmc.edu/~bang/cs131/labs/lab09/>.

Raskell is a small, Racket-like language. This repository holds its interpreter, written in Haskell.

## Files

| File | What it is |
|---|---|
| `RaskEval.hs` | The evaluator. **The one file you edit:** implement `primRest` (search for `TODO`). |
| `RaskAST.hs` | The two datatypes: `SExpr` (syntax, before evaluation) and `Value` (what evaluation produces), plus printing and helpers for checking arguments. |
| `RaskParser.hs` | Turns source text into an `SExpr`. |
| `Rask.hs` | The REPL and the file runner. |
| `ParserCombinators.hs`, `ParserBase.hs` | **PROVIDED: DO NOT EDIT.** The course's parser-combinator library (the same one you used for piq). |
| `rask` | Runs Raskell, compiling it first when needed. |
| `run-tests` | Runs the tests in `tests/`. |
| `examples/` | Small programs to run. |

## Getting started (on cs131.cs.hmc.edu)

```bash
cd cs131
git clone https://github.com/hmc-cs-131-fa-2026/raskell-lab.git
cd raskell-lab
./rask
```

The first run compiles the interpreter, which takes a few seconds. At the `raskell>` prompt:

```
((lambda (x) x) 'apple)
:q
```

Other ways to run Raskell:

```bash
./rask --file examples/cat.rask    # run a program in a file
./run-tests                        # run all the tests
./run-tests 01                     # run just the tests for rest
```

`./rask` recompiles automatically after you edit a `.hs` file.

## Submitting

Upload your `RaskEval.hs` to the **Lab 09** assignment on Gradescope and answer the questions there.
