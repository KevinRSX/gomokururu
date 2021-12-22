# Gomokururu

Gomokururu is a parallelized Gomoku solver.

This is a class project for [COMS W4995 Parallel Functional Programming, Fall 2021](http://www.cs.columbia.edu/~sedwards/classes/2021/4995-fall/index.html) at [Columbia University](http://columbia.edu/).



## Contributors

- Kevin Xue: kx2154@columbia.edu
- Andreas Cheng: hc3142@columbia.edu

## Installation

Prerequisite: [haskell stack tool](https://docs.haskellstack.org/en/stable/README/).

To execute the main game, run `stack run`. To run the test suites, run `stack test`.

To rapidly load and test the libraries, run `stack ghci` then run `:l <hs filename>`



## Reproducing Results

To reproduce the results in our report, you should run the main game with specific number of cores with proper RTS options.

```
stack run -- +RTS -N4 -l -s
```

`-N4` can be changed to any number of cores.

To view the threadscope result, install [Threadscope](https://hackage.haskell.org/package/threadscope), and

```
threadscope gomokururu-exe.eventlog
```



### Tweaking parameters

Unfortunately, the parameters cannot be configured as command line arguments right now. Instead, in `src/AI.hs`, change the following parameters

```haskell
-- Tweakable parameters
searchLevel :: Int
searchLevel = 2 -- must be less than treeLevel

sequentialLevel :: Int
sequentialLevel = 0 -- level to be evaluated sequentially, must be
                    -- less than or equal to searchLevel

cutoffScore :: Int
cutoffScore = 1000
```

Note the caveat: `searchLevel = 2` means you are searching for three levels, because we count from 0 to 2 in minimax.



### Playing as Human

The game can be easily configured to play as PvP or PvE or EvE. In `app/Main.hs`,

```haskell
(newBoard, row, col) <- case piece of
        Black -> takeTurnAI board piece (step + 1)
        White -> takeTurnAI board piece (step + 1)
```

change `takeTurnAI` to `takeTurn`. Also don't forget to comment this line:

```haskell
if step + 1 >= totalSteps then do putStrLn $ "Game ended at step limit."
```



## Note on Character Encoding

This project has only been tested on macOS Big Sur 11.3.1.

Emojis and full-width characters might fail on Windows. One workaround, for now, is to replace those Unicode characters with ASCII characters.



## References

- http://www.cs.columbia.edu/~sedwards/classes/2019/4995-fall/reports/gomoku.pdf
- https://medium.com/@LukeASalamone/creating-an-ai-for-gomoku-28a4c84c7a52
- https://github.com/sowakarol/gomoku-haskell
- https://github.com/lihongxun945/myblog/issues/14