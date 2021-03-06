# Gomokururu

Gomokururu is a parallelized Go-Moku AI.

This is a class project for [COMS W4995 Parallel Functional Programming, Fall 2021](http://www.cs.columbia.edu/~sedwards/classes/2021/4995-fall/index.html) at [Columbia University](http://columbia.edu/).



## Contributors

- Kevin Xue: kx2154@columbia.edu
- Andreas Cheng: hc3142@columbia.edu


## Installation

Prerequisite: [haskell stack tool](https://docs.haskellstack.org/en/stable/README/).

To execute the main game, run `stack run`. To run the test suites, run `stack test`.

To rapidly load and test the libraries, run `stack ghci` then run `:l <hs filename>`



## Reproducing Results

To reproduce the results in our [report](http://www.cs.columbia.edu/~sedwards/classes/2021/4995-fall/reports/Gomokururu.pdf), you need to run the main game with specific number of cores and proper RTS options.

```
stack run -- +RTS -N4 -l -s
```

`-N4` can be changed to any number of cores.

To view the threadscope result, install [Threadscope](https://hackage.haskell.org/package/threadscope), and

```
threadscope gomokururu-exe.eventlog
```


### Tweaking Parameters

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

Note the caveat: `searchLevel=2` means searching for three levels, because we count from 0 to 2 in minimax.


### `seqAB` Testing

The code with alpha-beta pruning that is used to perform tests in  `mode=seqAB` are all in `seqAB` branch. If you don't have access to the `seqAB` branch locally, the code can be found [here](https://github.com/KevinRSX/gomokururu/tree/seqAB).

```
git clone https://github.com/KevinRSX/gomokururu -b seqAB
```


## Playing as Human

One of both of AIs can be replaced by human players. To do this, in `app/Main.hs`,

```haskell
(newBoard, row, col) <- case piece of
        Black -> takeTurnAI board piece (step + 1)
        White -> takeTurnAI board piece (step + 1)
```

change `takeTurnAI` to `takeTurn` for the side you want. Also don't forget to comment this line:

```haskell
if step + 1 >= totalSteps then do putStrLn $ "Game ended at step limit."
```


## Note on Character Encoding

This project has only been tested on macOS Big Sur 11.3.1.

Emojis and full-width characters might fail on Windows. One workaround, for now, is to replace those Unicode characters with ASCII characters.


## WARNING
If you happen to be able to write some code like this, you should think twice whether you should be doing programming any more.
```haskell
boardDiff :: Board -> Board -> (Int, Int)
boardDiff oldBoard newBoard = (drow, dcol)
  where (drow, dcol) = quotRem diffPos (dim newBoard)
        diffPos = getDiff oldBoard1d newBoard1d 0
        getDiff [] [] _ = error "Invalid parameters"
        getDiff (x:xs) (y:ys) ind | x /= y = ind
                                  | x == y = getDiff xs ys (ind + 1)
        oldBoard1d = (V.toList . V.concat . V.toList) (getBoard oldBoard)
        newBoard1d = (V.toList . V.concat . V.toList) (getBoard newBoard)
```



## References

- http://www.cs.columbia.edu/~sedwards/classes/2019/4995-fall/reports/gomoku.pdf
- https://medium.com/@LukeASalamone/creating-an-ai-for-gomoku-28a4c84c7a52
- https://github.com/sowakarol/gomoku-haskell
- https://github.com/lihongxun945/myblog/issues/14
