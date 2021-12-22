# Gomokururu

Gomokururu is a parallelized Gomoku solver.

This is a class project for [COMS W4995 Parallel Functional Programming, Fall 2021](http://www.cs.columbia.edu/~sedwards/classes/2021/4995-fall/index.html) at [Columbia University](http://columbia.edu/).


## Contributors

- Kevin Xue
- Andreas Cheng

## Compilation and Execution

To compile, run `stack build` to build the minimal project.

To execute the game, run `stack run`.

More information can be found at [The Haskell Tool Stack Homepage](https://docs.haskellstack.org/en/stable/README/).

## Testing / Developing

To test the main functions, run `stack test`.

To rapidly load and test the libraries, run `stack ghci` then run `:l <hs filename>`

## Environment

This project has only been tested on macOS Big Sur 11.3.1.

Emojis and full-width characters might fail on Windows. One workaround, for now, is to replace those Unicode characters with ASCII characters.