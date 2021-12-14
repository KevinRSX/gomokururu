module Main where

import Game 
  (
    Board (..),
    Piece (..), 
    genBoard,
    printBoard,
    placePiece,
    piece2emoji,
    showStepInfo
  )

import AI

main :: IO ()
main = do
    gameLoop (genBoard 10) 0 10

getPair :: IO (Int, Int)
getPair = do
    rl <- getLine
    cl <- getLine
    return (read rl, read cl)

gameLoop :: Board -> Int -> Int -> IO ()
gameLoop board step totalSteps = do
    putStrLn "\n====Current Board===="
    printBoard board
    putStrLn "====================="


    putStrLn "Input black row and col: "
    (brow, bcol) <- getPair
    let boardB = placePiece board Black brow bcol
    showStepInfo Black (step + 1)
    printBoard boardB

    putStrLn "Input white row and col: "
    (wrow, wcol) <- getPair
    let boardW = placePiece boardB White wrow wcol
    showStepInfo White (step + 2)
    printBoard boardW

    if step + 2 < totalSteps
        then gameLoop boardW (step + 2) totalSteps
    else return ()
